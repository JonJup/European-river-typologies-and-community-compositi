# ————————————————————————————————————————————————————————— #
# ——— Clean Diatom data from Poland - Monitoring  ————————— # 
# ————————————————————————————————————————————————————————— #

# ———————————————————————————————————
# Purpose: In this script I create a harmonized spatial data set of diatom from the 
# raw data from Poland provided by Piotr Panek
# ————————————————


# setup -----------------------------------------------------------------------------
source("02_R/900_functions/add_typologies.R")

library(data.table)
library(dplyr)

# load data ----------------------------------------------------------------------
bio <- read_excel("01_data/001_diatoms/001_original_data/poland_monitoring/raw/FB MF PL RW 2016-2020.xlsx") 
harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")

# prepare data  ---------------------------------------------------------------------
bio2 <- bio

names(bio2) <- c("sample_id", "water_body", "remove1", "remove4", "y.coord", "x.coord", "date", "remove2", "remove3", "taxon", "remove5", "abundance", "x2")

bio2 <- bio2 |> 
        select(!contains("remove")) |> 
        filter(!is.na(abundance)) |> 
        mutate(date = lubridate::ymd_hm(date),
                 EPSG = 4326, 
                 data.set = "poland_monitoring_poland") |> 
        mutate(date = lubridate::date(date)) |> 
        select(!x2) 

data <- bio2 
rm(bio2)

## taxonomic cleaning in fix_tax script. 

## add taxon information 
data <- rename(data, original_name = taxon)
setDT(data)
data2 <- harmonization_table[data, on = "original_name"]  

## add site and date ids for this I need to round coordinates because some samples are
## categorized as from different sites even though they are from the same.
data2[, c("x.coord_round", "y.coord_round") := .(round(x.coord, 5), 
                                                 round(y.coord, 5))]

data2[, original_site_name := .GRP, by = c("x.coord_round", "y.coord_round")]
data2[ original_site_name == "3112", original_site_name := "833"]
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_poland_monitoring_diatoms")]


data2[,c("date","year", "month") := list(lubridate::ymd(date), lubridate::year(date), lubridate::month(date))]

## reshape data
data3 <- data2[, list(
        gr_sample_id,
        original_site_name,
        date,
        year,
        month,
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
        data.set = "poland_monitoring_diatom"
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
        filter(!original_site_name %in% uto$original_site_name) |>  
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
options(warn = -1)
updated_type <- data.table(original_site_name = rt$original_site_name)

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview::mapview(i.plot_typology, zcol = "brt") + mapview::mapview(i.rt, popup = c("water_body"), color = "red")
        print(x)
        i.bool <- "n"
        i.bool <- readline(paste(i, ":"))
        if (i.bool == "n"){
                # remove_list[length(remove_list) + 1] <- i.rt$site_id 
                updated_type[original_site_name == i.rt$original_site_name, new_type := "drop"]
        } else if (i.bool == "change"){
                i.towhat <- readline("change to:")
                updated_type[original_site_name == i.rt$original_site_name, new_type := i.towhat]
        } else {
                updated_type[original_site_name == i.rt$original_site_name, new_type := i.rt$brt12]
        }
        
        rm(list = ls()[grepl("i\\.", ls())])
}
# - join updated types to data 8
data9 <- left_join(data8, 
                   updated_type, 
                   by = "original_site_name")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
data10 <- data9[month %in% 5:9]
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data10, season_available = FALSE)

saveRDS(data10, "01_data/001_diatoms/001_original_data/poland_monitoring/final_aggregated.rds")
