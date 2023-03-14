# ——————————————————————————————————————————— #
# ——— Clean Diatom data from SWEDEN, AQEM ——— # 
# ——————————————————————————————————————————— #

# ———————————————————————————————————
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Sweden.  
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
# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "01_data/001_diatoms/001_original_data/sweden_aqem/raw/AQEMBilaga%205 Excel.xls"
sit_wd <- "01_data/001_diatoms/001_original_data/sweden_aqem/raw/AQEM_diatoms_coordinates.xlsx"

bio <- read_excel(bio_wd) |> setDT()
sit <- read_excel(sit_wd) |> setDT()

# prepare data ----------------------------------------------------------------------
names(bio)[1:3] <- c("old", "code", "taxon")
bio[is.na(taxon), taxon := old]
bio <- bio[!is.na(taxon)]

bio[, c("old", "code") := NULL]

bio2 <- 
        pivot_longer(bio, cols = !taxon, names_to = "original_site_name", values_to = "abundance") |> 
        filter(!is.na(abundance)) |> 
        filter(abundance != 0) |> 
        filter(!taxon %in% c("SUMMA RÄKNADE SKAL", "SUMMA EUNOTIA", "SUMMA EUNOTIA %", "SUMMA EUNOTIA–E. FORMICA %"))
data <- 
        sit |> 
        select(original_site_name = Site, y.coord = 'X_RAK (Xnew)', x.coord = 'Y_RAK (Xnew)', date = '1st sampling date') |> 
        right_join(bio2, by = "original_site_name") |> 
        mutate(EPSG = 2400, 
               data.set = "sweden_aqem_diatoms",
               date = dmy(date))

data <- data[!is.na(x.coord)]
## add season 
data[,c("year") := .(year(date))]

# taxonomic harmonization -----------------------------------------------------------
harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")

# join 
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_sweden_aqem_diatoms")]


data2[,year := .(year(date))]

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
typologies <- readRDS("01_data/all_typologies.rds")
data5 <- add_typologies(data4)

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]


## look for sites with different ID but same coordinates 
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

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview(i.plot_typology, zcol = "brt") + mapview(i.rt, popup = c("water_body"), color = "red")
        print(x)
        #i.bool <- "n"
        i.bool <- readline(paste(i, "/", nrow(rt), ":"))
        if (i.bool == "break"){
                break()
        } else if (i.bool == "n"){
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c"){
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
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)
data9 <- data9[brt12 != "drop"]


# temporal aggregation --------------------------------------------------------------
data10 <- data9[month(date) %in% 5:9]
saveRDS(data10, "01_data/001_diatoms/001_original_data/sweden_aqem/final_aggregated.rds")
