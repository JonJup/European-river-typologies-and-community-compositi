### ----------------------------- ###
### --- Clean FISH CZECH CHMI --- ### 
### ----------------------------- ###

# -------------------------------
# Purpose: Clean Czech FISH CHMI Data 
# -------------------------------

# setup -----------------------------------------------
library(data.table)
library(magrittr)
library(mapview)
library(tidyr)
library(mapview)
library(dplyr)
library(readxl)
library(lubridate)
library(stringdist)
library(sf)
library(stringr)

# load data -------------------------------------------
bio        <- read_excel("01_data/002_fish/001_original_data/czech_chmi/raw/Export CHMI fish_Baresova.xlsx")
taxontable <- readRDS("01_data/002_fish/harmonization_table_fish.rds")
typologies <- readRDS("01_data/all_typologies.rds")
source("02_R/900_functions/add_typologies.R")
# prepare data ----------------------------------------

setDT(bio)

data <- data.table(
        original_site_name = bio$LOCALITY_NAME,
        date               = ymd(bio$DATUM),
        taxon              = bio$TAXON,
        abundance          = 1,
        x.coord            = bio$COORDINATE_X,
        y.coord            = bio$COORDINATE_Y,
        EPSG               = 5514,
        data.set           = "chezch_chmi_fish",
        waterbody          = bio$RIVER_NAME
)

data <- data[!is.na(x.coord)]

## add season and year 
data[,c("season", "year") := .(case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                            month(date) %in% c(3,4,5)   ~ "spring",
                                            month(date) %in% c(6,7,8)   ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn"), 
                               year(date))]

data$taxon <- str_replace_all(data$taxon, "_", "\\ ")

TU <- unique(data$taxon)
TU <- setdiff(TU, taxontable$original_name)

# taxontable <- update_taxonomy2(TU)
# saveRDS(taxontable, "data/fish/2022-04-27_taxontable_fish.rds")

data %<>% rename("original_name" = taxon)
data3 <- taxontable[data, on = "original_name"]

data3[, site_id := .GRP, by = "original_site_name"]
data3[, site_id := as.numeric(site_id)]
data3[, date_id := .GRP, by = "date"]

## add leading zeros
data3[, site_id := dp$case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data3[, date_id := dp$case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

## add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_czech_chmi_fish")]

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
data6 <- data6[abundance != 0]
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

## look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))
data6[gr_sample_id == "site_00151_date_00085_czech_chmi_fish", c("gr_sample_id", "site_id", "original_site_name") := 
              .("site_00034_date_00085_czech_chmi_fish", 
                "00034", 
                "Tavíkovice")]

data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
summary(data6$richness)
hist(data6$richness)
data7 <- data6[richness > 2]

# - drop sites far removed from ECRINS river network 
data9 <- data7[distance < 300]
# - visually check the assignment of sites 
rt <-
        data9 |>
        unique(by = "site_id") |>
        st_as_sf(coords = c("x.coord", "y.coord"),
                    crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))

updated_type <- data.table(site_id = rt$site_id)

options(warn = -1)

for (i in 1:nrow(rt)){
        #if (i < 327) next()
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
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt]
        }
        rm(list = ls()[grepl("i\\.", ls())])
}

data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- dp$rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
data9[, month := month(date)]
data9 <- data9[month %in% 5:9]
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data9, season_available = F)

data10[, c("month", "waterbody") := NULL]
saveRDS(data10, "01_data/002_fish/001_original_data/czech_chmi/final_aggregated.rds")
