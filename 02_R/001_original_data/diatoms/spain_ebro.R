# ———————————————————————————————————————————————— #
# ——— Clean Diatom data from Spain, Ebro Basin ——— # 
# ———————————————————————————————————————————————— #

# ———————————————————————————————————
# Purpose: In this script I create a harmonized spatial data set of diatoms from the 
# raw data from the Ebro Hydrografic Confederation  
# ————————————————

# setup -----------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(fs)
library(lubridate)
library(mapview)
library(magrittr)
library(sf)
library(stringr)
library(stringdist)
library(readxl)
library(tidyr)
source("01_R/900_functions/add_typologies.R")
# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "01_data/001_diatoms/001_original_data/spain_ebro/raw/WCASF_20220119082845.xlsx"

bio <- read_excel(bio_wd)

# prepare data  ---------------------------------------------------------------------

# - to data.table
setDT(bio)
names(bio) <- c("original_site_name1", "water_body", "original_site_name2", "water_body2", "x.coord", "y.coord", "misc1", "misc2", "misc3", "sample_id", "date", "misc4", "parameter", "taxon", "misc5", "abundance", "misc6")
bio2 <- select(bio, !starts_with("misc"))
bio2 <- bio2[parameter == "Nº de valvas"]

# - the original_site_name1 works, 2 does not. 
all(pull(bio2[, uniqueN(x.coord), by = "original_site_name1"], V1) == 1)
all(pull(bio2[, uniqueN(x.coord), by = "original_site_name2"], V1) == 1)

bio2[, c("original_site_name2", "water_body2") := NULL]
#- multiple samples per site. Compare this to gr_sample_id later. 
bio2[, uniqueN(sample_id), by = "original_site_name1"]

# - add EPSG 
bio2[, EPSG := 25830]

sites <- unique(bio2, by = "original_site_name1")
sites2 <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs =  25830)
mapview(sites2)

data <- bio2
data <- data[taxon != "--"]
data[,date := dmy(date)]
#- add season and year
data[,c("year") := .(year(date))]


rm(sites, sites2, bio2, bio, bio_wd); gc()
# taxonomic harmonization -----------------------------------------------------------

harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")
data <- rename(data, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  

data2%<>%rename("original_site_name" = "original_site_name1")

data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

all (pull( data2[,uniqueN(x.coord), by = "site_id"], V1) == 1)

## add leading zeros
data2[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data2[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

## add gr_sample_id
data2[,gr_sample_id := paste0(sample_id,"_spain_ebro_diatoms")]

# - check that gr_sample_id matches sample_id
data2[, sample_id_per_gr := uniqueN(sample_id), by = "gr_sample_id"]
data2[sample_id_per_gr > 1 ] |> View()
# -> doesnt made changes above to fix this. Instead of the usual gr_sample_id method we
# simiply use the sample_id provided by the original data.set

## reshape data
data3 <- data2[, list(
        gr_sample_id,
        original_site_name,
        date,
        year,
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
        data.set = "spain_ebro_diatom",
        water_body
)]

## combine entries of same taxon
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

data3[, abundance := as.numeric(abundance)]
data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
typologies <- readRDS("01_data/all_typologies.rds")
data5 <- add_typologies(data4)

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]

# - look for sites with different ID but same coordinates 
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
# - visually check the assignment of sites 
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
options(warn = -1)
updated_type <- data.table(site_id = rt$site_id)

for (i in 1:nrow(rt)) {
        i.rt <- rt[i,]
        i.plot_typology <-
                st_crop(plot_typology,
                        st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <-
                mapview(i.plot_typology,
                        zcol = "brt",
                        map.type = "OpenStreetMap.DE") + mapview(i.rt,
                                                                 popup = c("water_body"),
                                                                 color = "red")
        print(x)
        #i.bool <- "n"
        i.bool <- readline(paste(i, "/", nrow(rt), ":"))
        if (i.bool == "break") {
                break()
        } else if (i.bool == "n") {
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c") {
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        
        rm(list = ls()[grepl("i\\.", ls())])
}

# - drop remove sites 
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")
# - drop "drop" rows determined in for-loop
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
data10 <- data9[lubridate::month(date) %in% 5:9]
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data10, season_available = FALSE)

saveRDS(data10, "01_data/001_diatoms/001_original_data/spain_ebro/final_aggregated.rds")

