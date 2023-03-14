# -------------------------------------------------------- #
# --- Clean diatom data from Finland —— Janne Soininen --- #
# -------------------------------------------------------- #

#  Purpose: In this script I create a harmonized spatial data set of diatoms from the 
#           raw data provided by the Janne Soininen.

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

# load data -------------------------------------------------------------------------
## directory with all biological data 
bio_wd <- "01_data/001_diatoms/001_original_data/finland_soininen/raw/Species_diatoms_Finland.xlsx"
sit_wd <- "01_data/001_diatoms/001_original_data/finland_soininen/raw/Streams_environment_Finland.xlsx"

bio <- read_excel(bio_wd, sheet = 1) |> setDT()
sit <- read_excel(sit_wd, sheet = 1) |> setDT()
typologies <- readRDS("01_data/all_typologies.rds")
harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")
# prepare data ----------------------------------------------------------------------
# - sampling method 
bio |> 
        select(-c("Species", "genus", "abbr. name")) |> 
        pivot_longer(cols = !`full name`, names_to = "original_site_name", "abundance") -> 
        bio2 

names(bio2)[1] <- "taxon"

bio2 %<>% filter(value!=0)

sit %<>% select(Site, longitude, latitude)

data <- left_join(bio2, sit, by = c("original_site_name" = "Site"))

# all sampling was conducted in August 2001 or 2004. See Soininen (2008): The
# Ecological Characteristics of Idiosyncratic and Nested Diatoms

data |> 
        mutate(EPSG = 2393,
               date = NA,
               year = NA,
               data.set = "finalnd_janne_soininen_diatoms") |> 
        rename(x.coord = longitude, 
               y.coord = latitude) -> 
        data
setDT(data)
# - remove sites without coordinates or taxa
data <- data[!is.na(x.coord)]
data <- data[!is.na(y.coord)]
data <- data[!is.na(taxon)]


# JOIN DATA -------------------------------------------------------------------------
names(data)[which(names(data) == "taxon")] <- "original_name"
data2 <- harmonization_table[data, on = "original_name"]

# - add site and date ids
data2[, site_id := .GRP, by = c("x.coord", "y.coord")]
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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_finland_janne_soininen_diatoms")]

# - reshape data
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
        abundance = value,
        x.coord,
        y.coord,
        EPSG,
        data.set = "finland_janne_soininen_diatoms"
)]

## combine entries of same taxon
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom))))))]

data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
data5 <- add_typologies(data4)

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
# - look for sites with different ID but same coordinates 
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
# - join data to updated types
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

# - drop "drop" rows and fix changed types 
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
data10 <- data9
data10[,date:=NULL]
data10$date <- lubridate::dmy("1-8-2004")

saveRDS(data10, paste0("data/diatoms/original_data/finland_soininen/final_aggregated.rds"))