# ————————————————————————————————————————————— #
# ——— Clean Diatom data from Germany, Hesse ——— # 
# ——— ALL SAMPLES                           ——— #
# ————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 08.02.23
# date last modified: 08.02.23
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Germany, Lower Saxony.  
# Temporal aggregation:  
# CRS: Pulkovo 1942(83) / 3-degree Gauss-Kruger zone 3 (E-N); EPSG: 5673
# ————————————————

# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/jonjup/jjmisc")
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
# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "01_data/001_diatoms/001_original_data/germany_hesse/raw/untersuchung_dia_2005-2019.xlsx"
sit_wd <- "01_data/001_diatoms/001_original_data/germany_hesse/raw/Messstellen.xlsx"

bio <- read_excel(bio_wd) |> setDT()
sit <- read_excel(sit_wd) |> setDT()


taxontable <- readRDS("01_data/001_diatoms/2022-12-22_taxontable_diatoms.rds")
typologies <- readRDS("01_data/all_typologies.rds")
# prepare data ----------------------------------------------------------------------

bio <- bio[MESSSTELLE_ID %in% sit$MST_ID]

bio2 <- data.table(original_site_name = bio$MESSSTELLE_ID,
                   date               = ymd(bio$DATUM), 
                   taxon              = bio$TAXON,
                   abundance          = NA,
                   EPSG               = 5673,
                   data.set           = "germany_hesse_diatoms")
sit2 <- data.table(original_site_name = sit$MST_ID,
                   x.coord            = sit$RW_GK,
                   y.coord            = sit$HW_GK)

data <- sit2[bio2, on = "original_site_name"]

data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]

## remove sites without coordinates and taxa 
data <- data[!is.na(x.coord)]
data <- data[!is.na(y.coord)]
data <- data[!is.na(taxon)]

data <- data[!taxon %in% c("Centrales","Pennales", "Acroperus elongatus")]

data <- rename(data, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_germany_hesse_diatoms")]

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
        data.set
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

data5 <- add_typologies(data4)


# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
rl <- readRDS("01_data/001_diatoms/001_original_data/germany_hesse/2022-01-25_remove_list.rds")
data8 <- data8[!site_id %in% rl]
updated_type <- readRDS("01_data/001_diatoms/001_original_data/germany_hesse/2022-06-10_updated_type.rds")
#- drop remove sites 
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)
data10 <- data9[month(date) %in% 5:9]
saveRDS(data10, paste0("01_data/001_diatoms/001_original_data/germany_hesse/",Sys.Date(),"_final_aggregated_all_samples.rds"))
