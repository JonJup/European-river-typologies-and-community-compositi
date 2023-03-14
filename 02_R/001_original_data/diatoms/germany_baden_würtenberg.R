# ———————————————————————————————————————————————— #
# ——— Clean Diatom data from Baden Wuerttemberg ——— # 
# ———————————————————————————————————————————————— #

# ———————————————————————————————————
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Germany Baden Wuerttemberg  
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

source("02_R/900_functions/functions/add_typologies.R")
# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "01_data/001_diatoms/001_original_data/germany_baden_wuertenberg/raw/Taxalisten_MuP_alle_Ergebnistabelle_BJ_2021.xlsx"
sit_wd <- "01_data/001_diatoms/001_original_data/germany_baden_wuertenberg/raw/WRRL-Monitoring_MuP_2015-17_Stand_2021-03-23.xlsx"

bio <- read_excel(bio_wd) |> setDT()
sit <- read_excel(sit_wd, sheet = 2, skip = 2) |> setDT()
# prepare data ----------------------------------------------------------------------

bio2 <- 
        bio |> 
        filter(Dimension == "Schalen") |> 
        select(original_site_name = GCODE, 
               date = Datum,
               taxon = Taxon,
               abundance = Messwert,
               freitext = `Freitext Taxon`)

sit2 <- 
        sit |> 
        select(original_site_name = GCODE, 
               y.coord =  `Nord-Wert`, 
               x.coord = `Ost-Wert`)

data <- sit2[bio2, on = "original_site_name"]

data$EPSG = 25832
data$date <- ymd(data$date)
data[, year := year(date)]

## remove sites without coordinates and taxa 
data <- data[!is.na(x.coord)]
data <- data[!is.na(y.coord)]
data <- data[!is.na(taxon)]

# harmonize taxon names  ------------------------------------------------------------
harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")

data[str_detect(taxon, "Taxon siehe"), freitext := str_remove(freitext, "\\(.*")]
data[str_detect(taxon, "Taxon siehe"), freitext := str_trim(freitext)]
data[str_detect(taxon, "Taxon siehe"), taxon := freitext]

data <- data[!taxon %in% c("Pennales")]

data  <- rename(data, original_name = taxon)
data2 <- harmonization_table[data, on = "original_name"]  

# - add site and date ids
data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

# - add leading zeros
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

# - add gr_sample_id
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_germany_baden_wurttemberg_diatoms")]

# - reshape data
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
        data.set = "germany_baden_wuerttemberg_diatoms"
)]

# - combine entries of same taxon
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
        if (i< 47) next()
        i.rt <- rt[i,]
        buff_dist <- 1000
        i.plot_typology <-
                st_crop(plot_typology,
                        st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  buff_dist))
        while(nrow(i.plot_typology) == 0){
                buff_dist <- buff_dist + 1000
                i.plot_typology <-
                        st_crop(plot_typology,
                                st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  buff_dist))
        }
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
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

data10 <- copy(data9)
data10 <- data10[month(date) %in% 5:9]
saveRDS(data10, "01_data/001_diatoms/001_original_data/germany_baden_wuertenberg/final_aggregated.rds")