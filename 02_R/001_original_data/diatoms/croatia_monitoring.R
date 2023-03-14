# -------------------------------------- #
# --- Clean Diatom data from Croatia --- #
# -------------------------------------- #

# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data Croatian monitoring

# setup -----------------------------------------------------------------------------
library(pacman)
p_load(dplyr, 
       data.table,
       lubridate,
       magrittr, 
       mapview,
       sf,
       tidyr,
       readxl,
       stringr,
       stringdist,
       units)

source("02_R/900_functions/add_typologies.R")

# load data ----------------------------------------------------------------------
data1 <- read_excel("01_data/001_diatoms/001_original_data/croatioa_monitoring/raw/MEDGIG_ECGIG_Final_w_date.xlsx", sheet = 1) 
data2 <- read_excel("01_data/001_diatoms/001_original_data/croatioa_monitoring/raw/MEDGIG_ECGIG_Final_w_date.xlsx", sheet = 2) 
harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")
# prepare data  ---------------------------------------------------------------------

#. turn species columns to numeric 
data <- list(data1, data2)
for (j in 1:2){
        j.data <- data[[j]]
        for (i in 1:ncol(j.data)){
                if(i < 8) next()
                i.data <- as.character(j.data[[i]])
                i.data[which(i.data == "+")] <- 1
                i.data <- as.numeric(i.data)
                i.data[is.na(i.data)] <- 0
                j.data[[i]]  <- i.data
                rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
        }    
        data[[j]] <- j.data
        rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
}; rm(i,j)
data1 <- data[[1]]
data2 <- data[[2]]
rm(data)

# rename columns 
names(data1)[1:5] <- c("code", "original_site_name", "date", "x.coord", "y.coord")
names(data2)[1:5] <- c("code", "original_site_name", "date", "x.coord", "y.coord")
# drop columns 
data1 <- data1[, -c(6:7)]
data2 <- data2[, -c(6:7)]

# pivot data from wide format to long format 
data1 <- 
        data1 |>  
        pivot_longer(cols = 6:ncol(data1), names_to = "taxon", values_to = "abundance") |> 
        filter(abundance != 0)
data2 <- 
        data2 |>  
        pivot_longer(cols = 6:ncol(data2), names_to = "taxon", values_to = "abundance") |> 
        filter(abundance != 0)

# combine datasets 
data <- rbindlist(list(data1, data2))
# fix error in date 
data[date == "17.05.201.7", date := "17.05.2017"]
# add variables 
data[, c("EPSG", "data.set", "date") := .(3765, "croatia_monitoring_diatoms", dmy(date))]
data[,c("year", "month") := list(year(date), month(date))]
# drop sites without coordinates 
data <- data[!is.na(x.coord)]
# drop rows with Penales as taxon. Taxonomic resolution too low. 
data <- data[taxon != "Penales"]
## taxonomic cleaning in fix_tax script 
TU <- unique(data$taxon)
any(!TU %in% harmonization_table$original_name)
rm(TU)
## add taxon information 
data <- rename(data, original_name = taxon)
setDT(data)
data2 <- harmonization_table[data, on = "original_name"]  
data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]
data2[, site_id := as.numeric(site_id)]
data2[, date_id := as.numeric(date_id)]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_croatia_monitoring_diatoms")]

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
# - no duplicate sites 
# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 10]
# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
# - visually check the assignment of sites 
rt <- 
        data8 |>
        filter(!site_id %in% updated_type_old$site_id) |> 
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
data9 <- left_join(data8, 
                   updated_types_combined, 
                   by = "site_id")
#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)
# - subset to focal months 
data10 <- data9[month(date) %in% 5:9]

saveRDS(data10, paste0("01_data/001_diatoms/001_original_data/croatioa_monitoring/final_aggregated.rds"))
