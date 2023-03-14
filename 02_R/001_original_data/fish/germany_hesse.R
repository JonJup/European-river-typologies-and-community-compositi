# ———————————————————————————————————————————— #
# ——— Clean fish data from Germany - Hesse ——— # 
# ———————————————————————————————————————————— #

# ———————————————————————————————————
# Purpose: In this script I create a harmonized spatial data set from the raw data  
# from Germany Hesse. 
# ————————————————


# setup -----------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(lubridate)
library(mapview)
library(sf)
library(stringr)
library(readxl)
source("02_R/900_functions/add_typologies.R")


# load data -------------------------------------------------------------------------
bio_wd  <- "01_data/002_fish/001_original_data/germany_hesse/raw/fisch_anzahl.xlsx"
sit_wd  <- "01_data/002_fish/001_original_data/germany_hesse/raw/Messstellen.xlsx"

bio     <- read_excel(bio_wd) |> setDT()
sit     <- read_excel(sit_wd) |> setDT()

harmonization_table <- readRDS("01_data/002_fish/harmonization_table_fish.rds")
typologies <- readRDS("01_data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------
data <- sit[bio, on = "MST_ID"]

data2 <- 
        data.table(
                original_site_name = data$MST_ID,
                x.coord = data$RW_GK,
                y.coord = data$HW_GK,
                date = ymd(data$PROBENAHMEDATUM), 
                taxon = data$TAXON_NAME,
                abundance = data$ANZAHL_GESAMT,
                EPSG = 5673 
        )


## add season 
data2[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]

# - remove sites without coordinates
data2 <- data2[!is.na(x.coord)]
data2 <- data2[!is.na(taxon)]
data2 <- data2[taxon != ""]

## —— Inspect taxa ———————————————————————————————————————————————————————————————————————
(TU <- unique(data2$taxon) |> sort())

translation_table <- data.table(taxon = TU,  sci = character(length(TU)))
translation_table[1, "sci"] <- "Anguilla anguilla"
translation_table[2, "sci"] <- "Thymallus thymallus"
translation_table[3, "sci"] <- "Salmo salar"
translation_table[4, "sci"] <- "Salmo trutta"
translation_table[5, "sci"] <- "Lampetra planeri"
translation_table[6, "sci"] <- "Salvelinus fontinalis"
translation_table[7, "sci"] <- "Barbus barbus"
translation_table[8, "sci"] <- "Perca fluviatilis"
translation_table[9, "sci"] <- "Rhodeus amarus"
translation_table[10, "sci"] <- "Pseudorasbora parva"
translation_table[11, "sci"] <- "Abramis brama"
translation_table[12, "sci"] <- "Squalius cephalus"
translation_table[13, "sci"] <- "Gasterosteus aculeatus"
translation_table[14, "sci"] <- "Gasterosteus aculeatus"
translation_table[15, "sci"] <- "Phoxinus phoxinus"
translation_table[16, "sci"] <- "Neogobius fluviatilis"
translation_table[17, "sci"] <- "Rutilus pigus"
translation_table[18, "sci"] <- "Carassius gibelio"
translation_table[19, "sci"] <- "Carassius gibelio"
translation_table[20, "sci"] <- "Ctenopharyngodon idella"
translation_table[21, "sci"] <- "Cottus gobio"
translation_table[22, "sci"] <- "Gobio gobio"
translation_table[23, "sci"] <- "Blicca bjoerkna"
translation_table[24, "sci"] <- "Leuciscus leuciscus"
translation_table[25, "sci"] <- "Esox lucius"
translation_table[26, "sci"] <- "Hucho hucho"
translation_table[27, "sci"] <- "Carassius carassius"
translation_table[28, "sci"] <- "Cyprinus carpio"
translation_table[29, "sci"] <- "Gymnocephalus cernua"
translation_table[30, "sci"] <- "Leuciscus idus"
translation_table[31, "sci"] <- "Proterorhinus marmoratus"
translation_table[32, "sci"] <- "Salmo trutta"
translation_table[33, "sci"] <- "Petromyzon marinus"
translation_table[34, "sci"] <- "Leucaspius delineatus"
translation_table[35, "sci"] <- "Chondrostoma nasus"
translation_table[36, "sci"] <- "Neogobio kessleri"
translation_table[37, "sci"] <- "Lota lota"
translation_table[38, "sci"] <- "Leuciscus aspius"
translation_table[39, "sci"] <- "Oncorhynchus mykiss"
translation_table[40, "sci"] <- "Rutilus rutilus"
translation_table[41, "sci"] <- "Scardinius erythrophthalmus"
translation_table[42, "sci"] <- "Misgurnus fossilis"
translation_table[43, "sci"] <- "Tinca tinca"
translation_table[44, "sci"] <- "Barbatula barbatula"
translation_table[45, "sci"] <- "Alburnoides bipunctatus"
translation_table[46, "sci"] <- "Neogobius melanostomus"
translation_table[47, "sci"] <- "Salmo trutta"
translation_table[48, "sci"] <- "Lepomis gibbosus"
translation_table[49, "sci"] <- "Cottus perifretum"
translation_table[50, "sci"] <- "Cobitis taenia"
translation_table[51, "sci"] <- "Alburnus alburnus"
translation_table[52, "sci"] <- "Romanogobio albipinnatus"
translation_table[53, "sci"] <- "Silurus glanis"
translation_table[54, "sci"] <- "Vimba vimba"
translation_table[55, "sci"] <- "Sander lucioperca"
translation_table[56, "sci"] <- "Pungitius pungitius"

data2 <- translation_table[data2, on = "taxon"]

names(data2)[which(names(data2) == "sci")] <- "original_name"
data2 <- harmonization_table[data2, on = "original_name"]
data2[, taxon := NULL]

# - add site and date ids 
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_", "germany_hesse_fish")]

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
        subclass,
        class,
        phylum,
        kingdom,
        abundance,
        x.coord,
        y.coord,
        EPSG,
        data.set = "germany_hesse_fish"
)]

## combine entries of same taxon 
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]

data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
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
data7 <- data6[richness > 2]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))

options(warn = -1)

updated_type <- data.table(site_id = rt$site_id)

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
data10 <- newest_sample(data9, season_available = FALSE)
saveRDS(data10, "01_data/002_fish/001_original_data/germany_hesse/final_aggregated.rds")

