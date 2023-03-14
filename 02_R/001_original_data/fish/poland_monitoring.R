### ----------------------------------------- ###
### --- Fish data from Polish Monitoring  --- ### 
### ----------------------------------------- ###

# -------------------------------
# Purpose: Clean and harmonize fish data provided by Piotr Panek.  
# -------------------------------

library(data.table)
library(magrittr)
library(mapview)
library(readxl)
library(lubridate)
library(stringdist)
library(sf)
library(dplyr)

# read data -------------------------------------------------------------------------

data       <- read_excel("01_data/002_fish/001_original_data/poland_monitoring/raw/Fish PL RW 2011_2021.xlsx")
harmonization_table <- readRDS("01_data/002_fish/harmonization_table_fish.rds")
typologies <- readRDS("01_data/all_typologies.rds")
source("02_R/900_functions/add_typologies.R")
source("02_R/900_functions/find_point.R")

# prepare data  ---------------------------------------------------------------------

data2 <- data.table(
        original_site_name = data$Nazwa_stanowiska,
        x.coord = data$`Dług_geogr_ E`,
        y.coord = data$`Szer_ geogr_N`,
        waterbody = data$Rzeka,
        lake = data$Jeziora_pow., 
        taxon = data$Gatunek,
        abundance = data$Osobn,
        month = data$`M-c`,
        day = data$Dz.,
        year = data$Rok,
        EPSG = 4326,
        data.set = "poland_monitoring_fish"
)

# create date variable 
data2[, date := ymd(paste0(year,"-",month,"-", day))]

# drop brak ryb (no fish) and brak wody (lack of water)
data2 <- data2[!taxon %in% c("brak wody", "brak ryb")]

# add season 
## add season and year 
data2[,c("season") := .(case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                            month(date) %in% c(3,4,5)   ~ "spring",
                                            month(date) %in% c(6,7,8)   ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn"))]

data2 <- rename(data2, original_name = taxon)
data3 <- harmonization_table[data2, on = "original_name"]
data3[, c("x.coord_round", "y.coord_round") := .(round(x.coord, 5), 
                                                 round(y.coord, 5))]

data3[, site_id := .GRP, by = c("x.coord_round", "y.coord_round")]
data3[, date_id := .GRP, by = "date"]

## add leading zeros
data3[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data3[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

## add gr_sample_id
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_poland_monitoring_fish")]

# - check that gr_sample_id matches sample_id

## reshape data
data4 <- data3[, list(
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
        data.set,
        waterbody
)]

## combine entries of same taxon
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

data4[, abundance := as.numeric(abundance)]
data4[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data5 <- unique(data4, by = c("gr_sample_id", "lowest.taxon"))
data5 <- add_typologies(data5)

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

## look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

for (i in seq_along(duplicate_sites)){
        if (i == 1){
                duplicate_site_locations <- data.table(id = 1:length(duplicate_sites))
        }
        i.location <- find_point(duplicate_sites[i], 
                       distane_matrix = distances2)
        i.data <- sites[i.location, ]
        data6[site_id == i.data$site_id[1], site_id := i.data$site_id[2]]
        rm(list = ls()[grepl("i\\.", ls())])
}

data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6[richness > 2]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]
# - visually check the assignment of sites 
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                    crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
updated_type <- data.table(site_id = rt$site_id)
options(warn = -1)
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
any(is.na(data9$new_type))

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
data9[, month := lubridate::month(date)]
data9 <- data9[month %in% 5:9]
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data9, season_available = FALSE)

saveRDS(data10, "01_data/002_fish/001_original_data/poland_monitoring/final_aggregated.rds")
