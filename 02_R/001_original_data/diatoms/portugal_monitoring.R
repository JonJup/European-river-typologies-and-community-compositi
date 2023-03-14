# ————————————————————————————————————————————————————————— #
# ——— Clean Diatom data from Protugal - Monitoring  ————————— # 
# ————————————————————————————————————————————————————————— #

# ———————————————————————————————————
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Portugal provided by M. Teresa Ferreira
# ————————————————


# setup -----------------------------------------------------------------------------
library(tidyr)
library(data.table)
library(magrittr)
library(sf)
library(readxl)
library(dplyr)

source("02_R/900_functions/add_typologies.R")
# load data ----------------------------------------------------------------------
sites <- read_excel("01_data/001_diatoms/001_original_data/portugal_monitoring/raw_data/Diatoms matrix.xlsx", n_max = 9)
bio <-   read_excel("01_data/001_diatoms/001_original_data/portugal_monitoring/raw_data/Diatoms matrix.xlsx", skip = 10) 
harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")

# prepare data  ---------------------------------------------------------------------
sites <- transpose(sites)
names(sites) <- sites[1,]
sites <- sites[-c(1,2), ]
setDT(sites)      
names(sites)[c(2,5,7,8,9)] <- c("water_body", "original_site_name", "y.coord", "x.coord", "date")
sites <- sites[, c("water_body", "original_site_name", "y.coord", "x.coord", "date")]
sites[, EPSG := 4326]
sites[, date := as.Date(as.numeric(date), origin = "1899-12-30")]
sites[, date := lubridate::ymd(date)]
sites[, x.coord := as.numeric(x.coord)]
sites[, y.coord := as.numeric(y.coord)]

sites[,c("year", "month") := list(lubridate::year(date), lubridate::month(date))]
sites[, site_id := .GRP, by = c("x.coord", "y.coord")]

bio <- transpose(bio)
setDT(bio)
# last entry 665 is NA 
bio <- bio[, -665]

names(bio) <- unlist(bio [1, ])
bio <- bio[-c(1,2), ]
bio <- bio[,lapply(.SD, as.numeric), .SDcols = 1:ncol(bio)]


data <- 
        cbind(bio, sites)  |>
        pivot_longer(
                cols = !c(
                        "water_body",
                        "original_site_name",
                        "y.coord",
                        "x.coord",
                        "date",
                        "year",
                        "month",
                        "season",
                        "site_id",
                        "EPSG"
                ),
                names_to = "taxon",
                values_to = "abundance"
        ) |>
        setDT() 

data <- data[!is.na(abundance)]

# Some taxon names seem to have inherited a very strange property. calling names[x],
# copying the printed result (for sake of the example Fragilaria rinoi Almeida &
# C.Delgado), names[x] == "Fragilaria rinoi Almeida & C.Delgado" returns FALSE. This
# breaks the taxontable join. Since I join to taxontable again in the combined_data
# scripts, fixing the porblem manually here does not help. I will need to change
# the original names. 

data[stringr::str_detect(taxon, "Almeida & C.Delgado"), taxon := "Fragilaria rinoi"]
taxontable[species == "Fragilaria rinoi", original_name := "Fragilaria rinoi"]

data[stringr::str_detect(taxon, "microsta"), taxon := "Pinnularia microstauron"]
data[stringr::str_detect(taxon, "candidagilae"), taxon := "Fragilaria candidagilae"]
data[stringr::str_detect(taxon, "bourbo"), taxon := "Gomphonema bourbonense"]
data[stringr::str_detect(taxon, "neoni"), taxon := "Adlafia neoniana Cantonati" ]

## taxonomic cleaning in fix_tax script. 

## add taxon information 
data <- rename(data, original_name = taxon)
setDT(data)
data2 <- harmonization_table[data, on = "original_name"]  

## add site and date ids for this I need to round coordinates because some samples are
## categorized as from different sites even though they are from the same.
data2[, c("x.coord_round", "y.coord_round") := .(round(x.coord, 5), 
                                                 round(y.coord, 5))]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_portugal_monitoring_diatoms")]

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
        data.set = "portugal_monitoring_diatom",
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
rt <- 
        data8 |> 
        filter(!site_id %in% uto$site_id) |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                    crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
options(warn = -1)
updated_type <- data.table(site_id = rt$site_id)

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview::mapview(i.plot_typology, zcol = "brt", map.type = "OpenStreetMap.DE") + mapview::mapview(i.rt, popup = c("water_body"), color = "red")
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

# - join updated types to data 8
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

# - drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
data10 <- data9[lubridate::month(date) %in% 5:9]
data10[, water_body := NULL]

saveRDS(data10, "01_data/001_diatoms/001_original_data/portugal_monitoring/final_aggregated.rds")
