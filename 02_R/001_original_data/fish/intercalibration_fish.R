### -------------------------------- ###
### --- Intercalibration Fish DB --- ### 
### -------------------------------- ###

# -------------------------------
# Purpose: Clean intercalibration fish DB
# -------------------------------

# setup -----------------------------------------------
pacman::p_load(data.table,
               magrittr,
               mapview,
               tidyr,
               mapview, 
               lubridate, 
               sf,
               dplyr,
               readxl,
               stringr)
source("02_R/900_functions/add_typologies.R")

# load data -------------------------------------------
bio     <- read_excel("01_data/002_fish/001_original_data/mixed_intercalibration_didier_pont/raw/Catch selection final.xlsx")
sites   <- read_excel("01_data/002_fish/001_original_data/mixed_intercalibration_didier_pont/raw/Fishing_Occasion selection final.xlsx")
harmonization_table <- readRDS("01_data/002_fish/harmonization_table_fish.rds")
typologies <- readRDS("01_data/all_typologies.rds")

# prepare data ----------------------------------------

setDT(bio)
setDT(sites)

data <- sites[bio, on = "Site_code"]

data <- data[, c("Site_code", "Date", "Species", "Number", "E_latitude", "E_longitude", "Country_code")]
names(data) <- c("original_site_name", "date", "taxon","abundance", "y.coord", "x.coord", "country")
data$date <- ymd(data$date)

data$EPSG <- 4326
data$data.set <- "interaclibration_fish_db"

## add season and year 
data[,c("season", "year") := .(case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                            month(date) %in% c(3,4,5)   ~ "spring",
                                            month(date) %in% c(6,7,8)   ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn"), 
                               year(date))]

data$taxon <- str_replace_all(data$taxon, "_", "\\ ")

data %<>% rename("original_name" = taxon)
data3 <- harmonization_table[data, on = "original_name"]

data3[, site_id := .GRP, by = "original_site_name"]
data3[, site_id := as.numeric(site_id)]
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
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_intercalibration_fish")]

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
        country
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
data5[country == "NO", least.impacted := TRUE]

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]
data6 <- data6[abundance != 0]
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
## look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

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
        #if (i < 327) next()
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
data10 <- data9[month(date) %in% 5:9]

data10$data.set <- "intercalibration_fish"
data10[, country := NULL]
saveRDS(data10, "01_data/002_fish/001_original_data/mixed_intercalibration_didier_pont/final_aggregated.rds")
