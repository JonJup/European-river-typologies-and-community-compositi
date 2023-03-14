# ——————————————————————————————————————————————————————————— #
# ——— Clean Diatom data from Bulgaria - Monitoring  ————————— # 
# ——————————————————————————————————————————————————————————— #

# Purpose: In this script I create a harmonized spatial data set of diatom data from Bulgaria 
# Please note that this data set was not used in the final analysis, because all samples lay 
# within regions with less than 20 samples. 

# setup -----------------------------------------------------------------------------
library(pacman)
p_load(dplyr, 
       fs,
       sf, 
       tidyr, 
       data.table, 
       lubridate,
       magrittr,
       readxl,
       units)

source("02_R/900_functions/add_typologies.R")

# load data ----------------------------------------------------------------------
# Create a vector with file names. Files will be loaded in a for loop below
files <- dir_ls("01_data/001_diatoms/001_original_data/bulgaria/raw/") 
dt.ls <- list()
for (i in seq_along(files)){
        print(i)        
        i.file <- files[i]
        i.data <- read_excel(i.file)
        # There is an error in one file which gives the 31.06.2021 as a date. 
        # As this date does not exist we fixed it to the 30.06.2021.
        if (i.file == "01_data/001_diatoms/001_original_data/bulgaria/raw/River_Diatoms_BG_108.xlsx") {
               names(i.data)[2] <- "30.06.2021"
        }
        i.date <- dmy(names(i.data)[2])
        i.data <- data.frame(i.data)
        i.x.coord <- i.data[2,2]
        i.y.coord <- i.data[1,2]
        i.taxa <- i.data[4:nrow(i.data),1]
        i.dt   <- data.table(
                original_site_name = as.character(i),
                date               = i.date,
                taxon               = i.taxa,
                x.coord            = i.x.coord,
                y.coord            = i.y.coord
        )
        dt.ls[[i]] <- i.dt
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

# prepare data ----------------------------------------------------------------------

# Combine loaded data sets
data <- rbindlist(dt.ls)
# Add new columns
data[, c("EPSG", "abundance", "data.set", "year", "month") := 
             .(4326, 1, "bulgaria_monitoring_diatom", year(date), month(date))]
# Drop rows with unknown (NA) taxon
data <- data[!is.na(taxon)]
# Create a vector with unique taxon names 
all_taxa <- unique(data$taxon)
# Load harmonization table and check if any names are not already included. 
harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")
any(!all_taxa %in% harmonization_table$original_name)

## add taxon information from harmonization table to data. 
data <- rename(data, original_name = taxon)
setDT(data)
data2 <- harmonization_table[data, on = "original_name"]  

## add site and date id to create sample id 
data2[, date_id := .GRP, by = date]
data2[, site_id := .GRP, by = original_site_name]
data2[, gr_sample_id := paste0("bulgaria_", site_id, "_", date_id)]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_bulgaria_monitroting_diatoms")]


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
        data.set = "bulgaria_monitoring_diatom"
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
(duplicate_sites <- which(distances2 < as_units(1, "m")))

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 10]
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

#- join updated types to data 8
data9 <- left_join(data8, 
                    utc, 
                    by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# - drop samples outside focal months
data9 <- data9[month(date) %in% 5:9]
data10 <- copy(data9)


saveRDS(data10, paste0("01_data/001_diatoms/001_original_data/bulgaria/final_aggregated.rds"))
