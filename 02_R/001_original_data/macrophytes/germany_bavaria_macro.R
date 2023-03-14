# —————————————————————————————————————————————————— #
# ——— Clean macrophyte data from Germany Bavaria ——— # 
# —————————————————————————————————————————————————— #

# ———————————————————————————————————
# Purpose: In this script I create a harmonized spatial data set from the raw data provided 
# from Bavaria, Germany. 
# ————————————————


# setup -----------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(fs)
library(lubridate)
library(mapview)
library(magrittr)
library(sf)

source("02_R/900_functions/add_typologies.R")

# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd  <- "01_data/003_macrophytes/001_original_data/germany_bavaria/raw"
## list of all bio data files 
bio_files <- dir_ls(bio_wd)
## read and format all files. Combine them in one table  
for (i in seq_along(bio_files)){
        
        ## create list to save list output 
        if (i == 1) out = list()
        
        if (i %in% c(90, 580, 581, 660,739)) {
                x <- fread(bio_files[i], fill = TRUE, sep = ";")
        }else{
                x <- fread(bio_files[i], fill = TRUE)
        }
        header  <- x[1:7,]
        ms_nr   <- pull(header[5,2])
        x.coord <- pull(header[7,2])
        y.coord <- pull(header[7,4])
        
        data <- x[10:nrow(x),]
        data %<>% filter(!V8 %in% c("Pennales", "Centrales"))
        date <- dmy(pull(data[,1]))
        taxa <- pull(data[,3])
        abundance <- as.numeric(data$V5)
        
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

harmonization_table <- readRDS("01_data/003_macrophytes/harmonization_table_macrophytes.rds")
typologies <- readRDS("01_data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------

# - rename 
data <- out 
rm(out)
## add season 
data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]
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
        
drop.taxa <- c("Chantransia - Stadien", 
               "Agapetus", 
               "Besdolus imhoffi", 
               "Chloroperla susemicheli", 
               "Hemianax",
               "Hydroptilidae", 
               "Marthamea vitripennis",
               "Oxyethira",
               "Protonemura praecox",
               "Rhabdiopteryx thienemanni",
               "Sphaerotylus",
               "Synagapetus",
               "Taeniopteryx hubaulti",
               "Taeniopteryx kuehtreiberi",
               "Taeniopteryx schoenemundi", 
               "Ephydatia fluviatilis", 
               "Spongilla lacustris")

data <- data[!taxon %in% drop.taxa]

## —— Inspect taxa ———————————————————————————————————————————————————————————————————————

names(data)[which(names(data) == "taxon")] <- "original_name"
data2 <- harmonization_table[data, on = "original_name"]
data2 <- data2[kingdom == "Plantae"]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_germany_bavaria_macrophytes")]

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
        data.set = "germany_bavaria_macrophytes"
)]

## combine entries of same taxon 
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]

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
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
options(warn = -1)

updated_type <- data.table(site_id = rt$site_id)
for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        if (nrow(i.plot_typology) == 0){
                x <- mapview(i.rt, color = "red")
        } else {
                x <- mapview(i.plot_typology, zcol = "brt") + mapview(i.rt, popup = c("water_body"), color = "red")
        }
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
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)
# temporal aggregation --------------------------------------------------------------
# - yes
source("02_R/900_functions/newest_sample.R")
data10 <- data9[month(date) %in% 5:9]
data10 <- newest_sample(data10, season_available = FALSE)
saveRDS(data10, "data/macrophytes/original_data/germany_bavaria/final_aggregated.rds")
