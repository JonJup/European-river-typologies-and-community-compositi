### ------------------------- ###
### --- Netherlands Fish  --- ### 
### ------------------------- ###

# -------------------------------
# Purpose: Clean fish data provided by for the Netherlands 
# -------------------------------

# setup -----------------------------------------------
library(data.table)
library(magrittr)
library(mapview)
library(mapview)
library(readxl)
library(lubridate)
library(stringdist)
library(sf)
library(dplyr)

source("02_R/900_functions/add_typologies.R")
# load data -------------------------------------------

load("01_data/002_fish/001_original_data/netherlands_waterkwaliteitsportaal/raw/Fish_XY.RData")
taxontable <- readRDS("01_data/002_fish/harmonization_table_fish.rds")
typologies <- readRDS("01_data/all_typologies.rds")

# prepare data ----------------------------------------

data <- Pisc2
rm(Pisc2)
setDT(data)

## column indicating waterbody type. We are not investigating lentic waterbodies nor
## brackish or marine waters. These can be removed. 

table(data$Water.type)
data <- data[Water.type %in% c("Grote rivieren", 
                               "Kleine rivieren, benedenlopen", 
                               "Kleine rivieren, bovenlopen", 
                               "Kleine rivieren, middenlopen",
                               "Kreken",
                               "Beken, benedenlopen", 
                               "Beken, bovenlopen",
                               "Beken, middenlopen")]

## Keep only Elektrofishing 
table(data$type.vistuig)
table(data$Schattingsmethode)

data <- data[type.vistuig == "Elektro"]

data2 <- data.table (
        original_site_name = "",
                date_new = dmy(data$Datum),
                date_old = data$Datum,
                taxon = data$Wetenschappelijke.naam, 
                abundance = data$Aantal, 
                x.coord = data$X, 
                y.coord = data$Y, 
                waterbody = data$Water,
                data.set = "netherlands_monitoring_fish",
        EPSG = 28992
)

## Some dates failed to parse. 
data2[is.na(date_new), unique(date_old)]
data2[date_old == "17-04-207", date_new := dmy("17-04-2007")]
data2[date_old == "04-10-201", date_new := dmy("04-10-2001")]
data2 <- data2[date_old != ""]

data2[, date := date_new]
data2[, c("date_new", "date_old") := NULL]

## add season and year 
data2[,c("season", "year") := .(case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                     month(date) %in% c(3,4,5)   ~ "spring",
                                     month(date) %in% c(6,7,8)   ~ "summer",
                                     month(date) %in% c(9,10,11) ~ "autumn"), 
                                year(date))]



## drop rows with empty species column 
data2 <- data2[taxon != ""]
data2[taxon == "Barbusbarbus", taxon := "Barbus barbus"]
## assign site_name 
data2[, original_site_name := .GRP, by = c("x.coord", "y.coord")]

data2%<>%rename("original_name" = taxon)
data3 <- taxontable[data2, on = "original_name"]

data3[, site_id := as.numeric(original_site_name)]
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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_netherlands_monitoring_fish")]

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
        EPSG,
        data.set,
        waterbody
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
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

# - look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 2]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]

#- visually check the assignment of sites
rt <-
        data8 |>
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
        x <- mapview(i.plot_typology, zcol = "brt") + mapview(i.rt, popup = "waterbody", color = "red")
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
data9[, month := month(date)]
data9 <- data9[month %in% 5:9]
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data9, season_available = F)
saveRDS(data10, "01_data/002_fish/001_original_data/netherlands_waterkwaliteitsportaal/final_aggregated.rds")
