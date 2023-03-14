### ---------------------- ###
### --- Portugal Fish  --- ### 
### ---------------------- ###

# -------------------------------
# Purpose: Clean fish data provided by Maria Teresa Ferreira for Portugal.
# -------------------------------

# setup -----------------------------------------------
library(data.table)
library(magrittr)
library(mapview)
library(tidyr)
library(mapview)
library(readxl)
library(lubridate)
library(stringdist)
library(sf)
library(dplyr)

source("02_R/900_functions/add_typologies.R")

# load data -------------------------------------------
bio <- read_excel("01_data/002_fish/001_original_data/portugal_monitoring/raw/Fish_Fauna_MATRIX.xlsx")

harmonization_table <- readRDS("01_data/002_fish/2022-04-27_taxontable_fish.rds")
typologies <- readRDS("01_data/all_typologies.rds")

# prepare data ----------------------------------------

## separate site information form taxa information 
sites <- bio[1:7, ]
## drop empty column
sites <- sites[,-2]
sites <- data.table(
        site_id            = unlist(sites[1,]),
        original_site_name = unlist(sites[2,]), 
        x.coord            = unlist(sites[3,]), 
        y.coord            = unlist(sites[4,]), 
        date               = as.numeric(unlist(sites[5,])),
        EPSG               = 4326
)
sites <- sites[-1,]
sites <- sites[, date := as.Date(date, origin = "1899-12-30")]

bio2 <- bio[9:213, ]
bio2 <- bio2[,-2]
names(bio2) <- c("taxon", sites$site_id)
bio2 |> 
        pivot_longer(cols = !taxon, names_to = "site_id", values_to = "abundance") |> 
        filter(abundance != 0) -> 
        bio2
setDT(bio2)

data <- sites[bio2, on = "site_id"]
data[, data.set := "portugal_monitoring_fish"]

rm(bio, bio2, sites)
## add season and year 
data[,c("season", "year") := .(case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                            month(date) %in% c(3,4,5)   ~ "spring",
                                            month(date) %in% c(6,7,8)   ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn"), 
                               lubridate::year(date))]

data <- data[!taxon %in% c("TOTAL", "Híbrido")]

data%<>%rename("original_name" = taxon)
data3 <- taxontable[data, on = "original_name"]

data3[, site_id := .GRP, by = "original_site_name"]
data3[, site_id := as.numeric(site_id)]
data3[, date_id := .GRP, by = "date"]

## add leading zeros
data3[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data3[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

## add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_portugal_monitoring_fish")]

# - check that gr_sample_id matches sample_id

## reshape data
data4 <- data3[, list(
        gr_sample_id,
        original_site_name,
        date,
        year,
        season,
        site_id,
        date_id,
        original_name,
        species,
        genus,
        family,
        order,
        class,
        phylum,
        kingdom,
        abundance,
        x.coord,
        y.coord,
        EPSG
)]

## combine entries of same taxon
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

data4[, abundance := as.numeric(abundance)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data5 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))
data5 <- add_typologies(data5)

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]
data6 <- data6[abundance != 0]
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

# -  look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
summary(data6$richness)
hist(data6$richness)
data7 <- data6[richness > 2]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
# - visually check the assignment of sites 
rt <-
        data8 |>
        filter(!site_id %in% updated_type_old$site_id) |> 
        unique(by = "site_id") |>
        st_as_sf(coords = c("x.coord", "y.coord"),
                    crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))

updated_type <- data.table(site_id = rt$site_id)

options(warn = -1)

for (i in 1:nrow(rt)){
        i.percent <- i/nrow(rt) * 100
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview(i.plot_typology, zcol = "brt", map.type = "OpenStreetMap.DE") + mapview(i.rt, popup = "waterbody", color = "red")
        print(x)
        i.bool <- "n"
        i.bool <- readline(paste0(i,"/", nrow(rt)))
        if (i.bool == "break")
                break()
        if (i.bool == "n"){
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c"){
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        rm(list = ls()[grepl("i\\.", ls())])
}
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
data9 <- data9[month(date) %in% 5:9]
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data9, season_available = F)

data10$data.set <- "portugal_monitoring_fish"
saveRDS(data10, "01_data/002_fish/001_original_data/portugal_monitoring/final_aggregated.rds")
