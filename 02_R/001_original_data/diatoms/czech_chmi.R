# ---------------------------------------------------- #
# --- Clean Diatom data from Czech Republic - CHMI --- #
# ---------------------------------------------------- #

#  Purpose: In this script I create a harmonized spatial data set of diatom from the 
#           raw data CHMI data from the Czech Republic

# setup -----------------------------------------------------------------------------

library(pacman)
p_load(dplyr, 
       fs,
       sf, 
       tidyr, 
       data.table, 
       lubridate,
       magrittr,
       mapview,
       readxl,
       units)

source("02_R/900_functions/add_typologies.R")

# load data ----------------------------------------------------------------------
bio     <- read_excel("01_data/001_diatoms/001_original_data/czech_chmi/raw/Export fytobentos_CHMI_Baresova.xlsx") 
harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")

# prepare data  ---------------------------------------------------------------------

# - reformat data 
data <- 
        data.table(
                original_site_name = bio$LOCALITY_ID,
                x.coord            = bio$COORDINATE_X,
                y.coord            = bio$COORDINATE_Y,
                date               = ymd(bio$DATUM),
                taxon              = bio$TAXON,
                abundance          = 1,
                data.set           = "czech_chmi_diatoms",
                waterbody          = bio$RIVER_NAME,
                EPSG               = 5514
        )
# - add year and month based on date 
data[,c("year", "month") := list(year(date), month(date))]
# - drop sites without coordinates 
data <- data[!is.na(x.coord)]
# - drop observations with very low taxonomic resolution
data <- data[!taxon %in% c("Bacillariophyceae", "Bacillariophyceae centricae", "Bacillariophyceae pennatae")]
rm(bio)


TU <- unique(data$taxon)
data <- rename(data, original_name = taxon)
setDT(data)
data2 <- harmonization_table[data, on = "original_name"]  
rm(data)
rm(harmonization_table)

# - add site and date coordinates
data2[, x.coord_round := round(x.coord, 5)]
data2[, y.coord_round := round(y.coord, 5)]
data2[, site_id := .GRP, by = c("x.coord_round", "y.coord_round")]
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_czech_chmi_diatoms")]

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
        data.set = "czech_chmi_diatoms",
        waterbody
)]
rm(data2)
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

rm(data3, data4)
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
#- join updated types to data 8
data9 <- left_join(data8, 
                   utc, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
# drop sites from outside focal months
data9 <- data9[month(date) %in% 5:9]
# - only the most recent sample from each site 
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data9, season_available = FALSE)

saveRDS(data10, paste0("01_data/001_diatoms/001_original_data/czech_chmi/final_aggregated.rds"))
