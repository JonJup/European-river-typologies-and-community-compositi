### -------------------------------------- ###
# ——— Clean macrophyte data from Finland ——— # 
### -------------------------------------- ###

# Purpose: In this script I create a harmonized spatial data set from the raw data provided 
# from Finland. 

# setup -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(plotly)
library(readxl)
library(lubridate)
library(tidyr)
library(sf)
library(dplyr)
library(stringr)

source("02_R/900_functions/add_typologies.R")
# load data -------------------------------------------------------------------------
data  <- read_excel("01_data/003_macrophytes/001_original_data/finland_jukka/raw/MacrophytesAndBryophytes_MeanCoverPerRiffleSite_N188_ToJupke.xlsx", sheet = 2) 
setDT(data)
typologies          <- readRDS("01_data/all_typologies.rds")
harmonization_table <- readRDS("01_data/003_macrophytes/harmonization_table_macrophytes.rds")
# prepare data ----------------------------------------------------------------------
# - change names 
names(data)[1:5] <-c("sample_id", "original_site_name", "y.coord", "x.coord", "date")

## change date format 
data[, date := ymd(date)]

## pivot to long format 
data2 <- pivot_longer(data, cols = !(sample_id:date), names_to = "taxon", values_to = "coverage")
setDT(data2)
## drop zero coverage observations 
data2 <- data2[coverage > 0]

data2[, EPSG := 4055]
data2[, data.set := "finnland_monitoring_macrophytes"]

## add season 
data2[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]

data <- data2
rm(data2)


data[, taxon := str_replace(taxon, "\\.", "\\ ")]
data[, taxon := str_remove(taxon, "\\.L\\.$")]
data <- data[!taxon %in% c("Bryophytes_TotalCoverInMacrophyteSurvey", "Dichelymoides", "Mycopus europeus")]
# clean taxonomy --------------------------------------------------------------------
data  <- rename(data, original_name = taxon)
data2 <- harmonization_table[data, on = "original_name"]  

rm(taxontable, data, TU)

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_finland_monitoring_macrophytes")]

# - check that gr_sample_id matches sample_id

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
        abundance = NA,
        x.coord,
        y.coord,
        EPSG,
        data.set = "finland_monitoring_macrophytes"
)]

## combine entries of same taxon
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]


data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
data5 <- add_typologies(data4)

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
## look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6#[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]

# - visually check the assignment of sites 
rt <- 
        data8 |>
        filter(!site_id %in% uto$site_id) |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                    crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
updated_type <- data.table(site_id = rsite_id)
for (i in 1:nrow(rt)){
        print(paste(i, "/", nrow(rt)))
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview::mapview(i.plot_typology, zcol = "brt", map.type = "OpenStreetMap.DE") + mapview::mapview(i.rt, color = "red")
        print(x)
        i.bool <- "n"
        i.bool <- readline(paste(i, ":"))
        if (i.bool == "break"){
                break()
        }
        if (i.bool == "n"){
                # remove_list[length(remove_list) + 1] <- i.rsite_id 
                updated_type[site_id == i.rsite_id, new_type := "drop"]
        } else if (i.bool == "change"){
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rsite_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rsite_id, new_type := i.rbrt12]
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
data9 <- data9[species != ""]
data9$brt12 |> unique()

# temporal aggregation --------------------------------------------------------------
data10 <- data9[month(date) %in% 5:9]

saveRDS(data10, "01_data/003_macrophytes/001_original_data/finland_jukka/final_aggregated.rds")
