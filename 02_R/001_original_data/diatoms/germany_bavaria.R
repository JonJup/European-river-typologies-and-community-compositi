# —————————————————————————————————————————————————————————— #
# ——— Clean Diatom data from Germany, Bavaria——— # 
# —————————————————————————————————————————————————————————— #

# ———————————————————————————————————
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Germany, Bavaria.  
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
bio_wd <- "01_data/001_diatoms/001_original_data/germany_bavaria/raw/files/"


bio_files <- dir_ls(bio_wd)
## read and format all files. Combine them in one table  
for (i in seq_along(bio_files)){
        
        ## create list to save list output 
        if (i == 1) out = list()
        
        x <- fread(bio_files[i], fill = TRUE)
        
        header  <- x[1:7,]
        ms_nr   <- pull(header[5,2])
        x.coord <- pull(header[7,2])
        y.coord <- pull(header[7,4])
        
        data <- x[10:nrow(x),]
        data %<>% filter(V7 %in% c("Pennales", "Centrales"))
        date <- dmy(pull(data[,1]))
        taxa <- pull(data[,3])
        abundance <- as.numeric(str_replace(data$V5, ",", "."))
        
        xx <- data.table(original_site_name = ms_nr, 
                         date = date, 
                         taxon = taxa,
                         abundance = abundance,
                         x.coord = x.coord, 
                         y.coord = y.coord)
        
        out[[i]] <- xx
        if (i == length(bio_files)) out <- rbindlist(out)
        rm(i, x, header, x.coord, y.coord, data, xx, abundance, taxa, ms_nr)
        
}
# prepare data ----------------------------------------------------------------------
data <- out 
rm(out )
## add season 
data[,c("year") := .(year(date))]
## remove sites without coordinates
data <- data[!is.na(x.coord)]
data <- data[!is.na(y.coord)]

## add EPSG
data[, EPSG :=25832]

## check sites 
data |> 
        unique(by = "original_site_name") |> 
        pull(x.coord) |> 
        unique()
data |> 
        unique(by = "original_site_name") |> 
        pull(y.coord) |> 
        unique()

taxontable <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")

data <- data[!taxon %in% c("Centrales")]
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_germany_bavaria_diatoms")]

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
        data.set = "germany_bavaria_diatoms"
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

remove_list <- c()

options(warn = -1)

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, 
                                   st_buffer(
                                           st_transform(i.rt, 
                                                        crs = st_crs(typologies)
                                           ), 
                                           dist =  2000)
        )
        i.x <- 
                mapview(i.plot_typology, zcol = "brt12") + 
                mapview(i.rt, color = "red")
        print(i.x)
        i.bool <- "n"
        i.bool <- readline(paste(i,"/",nrow(rt), ":"))
        if (i.bool == "n"){
                remove_list[length(remove_list) + 1] <- i.rt$site_id  
        }
        if (i.bool == "c"){
                i.towhat <- readline("change to:")
                data8[site_id == i.rt$site_id, brt12 := i.towhat]
        }
        rm(list = ls()[grepl("i\\.", ls())])
}

connection_list <- data10[, c("site_id", "brt12")] |> unique(by = "site_id")
#- drop remove sites 
data9 <- data8[!site_id %in% remove_list]
data9 <- data9[month %in% 5:9]
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data9)
saveRDS(data10, "01_data/001_diatoms/900_original_data/germany_bavaria/final_aggregated.rds"))
