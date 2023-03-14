# ------------------------------------------ #
# --- Clean Diatom data from Finland JJM --- #
# ------------------------------------------ #

#  Purpose: In this script I create a harmonized spatial data set of diatom from the 
#           raw data from Finland   

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
       stringr,
       units)

source("02_R/900_functions/add_typologies.R")


# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "01_data/001_diatoms/001_original_data/finland_jenny/raw/diatomsabu_pohjanmaa.csv"
sit_wd <- "01_data/001_diatoms/001_original_data/finland_jenny/raw/YMPdatakeskiarvot_Pohjanmaa.xlsx"

bio <- fread(bio_wd)
sit <- read_excel(sit_wd) |> setDT()
harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")
typologies <- readRDS("01_data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------
bio2 <- 
        pivot_longer(bio, cols = !Sampling_site, names_to = "taxon", values_to = "abundance") |> 
        filter(abundance != 0) |> 
        rename(original_site_name = Sampling_site)
sit2 <- 
        select(sit, original_site_name = site, date,y.coord =  coor_n, x.coord = coor_e)
data <- left_join(bio2, sit2, by = "original_site_name")

data$EPSG = 3067
data$date <- ymd(data$date)
data$taxon <- str_replace_all(data$taxon, "_", "\\ ")
setDT(data)
data[,c("year") := .(year(date)]

# - remove sites without coordinates and taxa 
data <- data[!is.na(x.coord)]
data <- data[!is.na(y.coord)]
data <- data[!is.na(taxon)]

# join data -------------------------------------------------------------------------
data <- rename(data, original_name = taxon)
data2 <- harmonization_table[data, on = "original_name"]  

# - check

# - add site and date ids
data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

# - add leading zeros
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

# - add gr_sample_id
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_finland_jenny_diatoms")]

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
        data.set = "finland_jenny_diatoms"
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
data5 <- add_typologies(data4)
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
        unique(by = "site_id") |>
        st_as_sf(coords = c("x.coord", "y.coord"),
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))
updated_type <- data.table(site_id = rt$site_id)
options(warn = -1)
 
for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview(i.plot_typology, zcol = "brt") + mapview(i.rt, popup = "original_site_name", color = "red")
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

#- drop remove sites 
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")
# - drop "drop" rows and fix changed types 
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
data9 <- data9[month(date) %in% 5:9]
saveRDS(data9, paste0("01_data/001_diatoms/001_original_data/finland_jenny/final_aggregated.rds"))

