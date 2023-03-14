# —————————————————————————————————————————————————————————— #
# ——— Clean Diatom data from Germany, Lower Saxony ————————— # 
# —————————————————————————————————————————————————————————— #

# ———————————————————————————————————
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Germany, Lower Saxony.  
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
bio_wd <- "01_data/001_diatoms/001_original_data/germany_lower_saxony/raw/200918_WRRL-Monitoring_NLWKN_2008-2018_Dia.xlsx"
sit_wd <- "01_data/001_diatoms/001_original_data/germany_lower_saxony/raw/200918_WRRL-Monitoring_NLWKN_2008-2018_Proben_Stammdaten.xlsx"

bio <- read_excel(bio_wd) |> setDT()
sit <- read_excel(sit_wd) |> setDT()
# prepare data ----------------------------------------------------------------------

bio2 <- bio |> 
        select(original_site_name = PrMstID, 
               date = PrDatum,
               taxon = TaxTaxon,
               abundance = DiaAbRelativ
        ) |> 
        mutate( 
                data.set = "germany_lower_saxony_diatoms",
                EPSG = 4647,
                date = ymd(date)
        ) |> 
        setDT()

bio2[,year := year(date)]

sit2 <- sit[, c("PrMstID", "MstUTMx", "MstUTMy")]
names(sit2) <- c("original_site_name", "x.coord", "y.coord")
sit2 <- unique(sit2, by = "original_site_name")
data <- sit2[bio2, on = "original_site_name"]

## remove sites without coordinates and taxa 
data <- data[!is.na(x.coord)]
data <- data[!is.na(y.coord)]
data <- data[!is.na(taxon)]

## remove one site that is in the middle of the pacific ocean
data <- data[original_site_name != "22001018"]

# harmonize taxon names  ------------------------------------------------------------
harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")

data <- data[!taxon %in% c("Centrales","Marine Bacillariophyceae", "Pennales", "Bacillariophyceae", "Thalassiosiraceae et Melosiraceae")]

# join 
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_germany_lower_saxony_diatoms")]

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


# - look for sites with different ID but same coordinates 
sites <- unique(data6, by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))
data6[site_id == "01209", site_id := "01134"]

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
        x <- mapview(i.plot_typology, zcol = "brt") + mapview(i.rt, popup = "waterbody", color = "red")
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
#- save the remove list. 
saveRDS(updated_type, "01_data/001_diatoms/001_original_data/germany_lower_saxony/updated_type.rds")


#- drop remove sites 
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)
data10 <- copy(data9)
data10[month(date) %in% 5:9]
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data9, season_available = FALSE)
saveRDS(data10, "01_data/001_diatoms/001_original_data/germany_lower_saxony/final_aggregated.rds")
