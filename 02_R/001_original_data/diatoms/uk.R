# ————————————————————————————————— #
# ——— Clean Diatom data from UK ——— # 
# ————————————————————————————————— #

# ———————————————————————————————————
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from UK.  
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

source("02_R/900_functions/add_typologies.R")
source("02_R/900_functions/find_point.R")
# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "01_data/001_diatoms/001_original_data/uk_ee/raw/DIAT_OPEN_DATA_TAXA_2022-01-14.csv"
sit_wd <- "01_data/001_diatoms/001_original_data/uk_ee/raw/DIAT_OPEN_DATA_SITE_2022-01-14.csv"

bio <- fread(bio_wd) |> setDT()
sit <- fread(sit_wd) |> setDT()

# prepare data ----------------------------------------------------------------------

table(bio$SAMPLE_METHOD_DESCRIPTION)

#- subset to type: STANDARD DIATOM method for lakes and rivers (WFD Compliant) & remove
#DNA analyses. The results of the latter are not comparable with traditional laboratory
#results. Also, subset to diatoms. 
bio <- bio[SAMPLE_METHOD_DESCRIPTION == "STANDARD DIATOM method for lakes and rivers (WFD Compliant)"
           & ANALYSIS_TYPE_DESCRIPTION == "LABORATORY PRIMARY: Analysed in laboratory by primary analyst"  
           & TAXON_GROUP_NAME == "diatom"]

bio2 <- data.table(original_site_name = bio$SITE_ID,
                   date               = ymd(bio$SAMPLE_DATE), 
                   taxon              = bio$TAXON_NAME,
                   abundance          = bio$UNITS_FOUND_OR_SEQUENCE_READS, 
                   data.set           = "uk_monitoring_diatoms",
                   water_body         = bio$WATER_BODY
)
sit2 <- data.table(
        original_site_name = sit$SITE_ID,
        x.coord            = sit$FULL_EASTING,
        y.coord            = sit$FULL_NORTHING
)
data <- sit2[bio2, on = "original_site_name"]


data <- data[!is.na(x.coord)]
data[, EPSG := 27700]
## add season 
data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]

sites <- unique(data, by = "original_site_name")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = sites$EPSG[1])
mapview(sites)

# taxonomic harmonization -----------------------------------------------------------
harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")
data <- rename(data, original_name = taxon)
data2 <- harmonization_table[data, on = "original_name"]  

## add site and date ids
data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_uk_ee_diatoms")]


data2[,c("year", "season") := .(year(date), 
                                case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                          month(date) %in% c(3,4,5)   ~ "spring",
                                          month(date) %in% c(6,7,8)   ~ "summer",
                                          month(date) %in% c(9,10,11) ~ "autumn"))]

## reshape data
data3 <- data2[, list(
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

## visual checks
sites <- unique(data5, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

data6 <- data5[least.impacted == TRUE]

# - look for sites with different ID but same coordinates 
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))
x <- find_point(duplicate_sites[1])
data6 <- data6[site_id != "01917"]
## drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 10]
data8 <- data7[distance < 300]


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

#- drop remove sites 
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
data10 <- data9[month(date) %in% 5:9]
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data10, season_available = FALSE)
#- drop waterbody variable
data10[, water_body := NULL]
saveRDS(data10, "01_data/001_diatoms/001_original_data/uk_ee/final_aggregated.rds")
