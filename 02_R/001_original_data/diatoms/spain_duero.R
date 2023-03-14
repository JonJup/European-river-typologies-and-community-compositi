### ------------------------------------------- ###
### --- Diatom data from Duerto Basin Spain --- ### 
### ------------------------------------------- ###

# Purpose: Clean and harmonize data provided by Duero River authority

# setup -----------------------------------------------

library(data.table)
library(magrittr)
library(mapview)
library(readxl)
library(lubridate)
library(stringdist)
library(sf)
library(dplyr)

source("02_R/900_functions/add_typologies.R")
# load data -------------------------------------------

bio      <- setDT(read_excel("01_data/001_diatoms/001_original_data/spain_duero/raw_data/Datos_MACROF_DIATO_2016_20.xlsx"))
bio_id   <- setDT(read_excel("01_data/001_diatoms/001_original_data/spain_duero/raw_data/TAXONES_ID_TAXON.xlsx"))
sites    <- setDT(read_excel("01_data/001_diatoms/001_original_data/spain_duero/raw_data/INFO_PUNTOS_MUESTREO.xlsx"))
sampling <- setDT(read_excel("01_data/001_diatoms/001_original_data/spain_duero/raw_data/METODO_MUESTREO.xlsx"))

harmonization_table <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")

# prepare data ----------------------------------------

## join bio_id to bio 
bio2 <- bio_id[bio, on = "ID_TAXON"]
rm(bio_id);gc()

## subset to diatoms 
bio2 <- bio2[FILO_DIVISION == "Bacillariophyta"]

## add samplig information 
all(bio2$`PUNTO DE MUESTREO` %in% sites$`PUNTO DE MUESTREO`)
bio3 <- sites[bio2, on = "PUNTO DE MUESTREO"]

## add sampling method 
names(sampling)[which(names(sampling) == "COD_METMUES")] <- "CODIGO_METODO MUESTREO"
bio4 <- sampling[bio3, on = "CODIGO_METODO MUESTREO"]
bio4

## what parameters where measured? 
table(bio4$`CÓDIGO PARÁMETRO`)

bio5 <- bio4[`CÓDIGO PARÁMETRO` == "NUMVAL"]

## check parameters that should be all the same now 
unique(bio5$UNIDAD)
unique(bio5$`NOMBRE PARÁMETRO`)

## remove lakes 
bio5 <- bio5[LAGO == FALSE]
bio5 <- bio5[SISTQE == "DIATOMEAS"]
unique(bio5$DESCRIPCION)

## reshape 
data <- data.table(
        original_site_name = bio5$'PUNTO DE MUESTREO', 
        date               = ymd_hms(bio5$`FECHA MUESTREO`),
        taxon              = bio5$NOMBRE, 
        abundance          = bio5$`VALOR NUMÉRICO`,
        x.coord            = bio5$X30_PM_Corr_ETRS89,
        y.coord            = bio5$Y30_PM_Corr_ETRS89,
        EPSG               = 25830,
        data.set              = "spain_duero_diatom"
)

## add season and year 
data[,c("year", "season") := .(year(date), 
                               case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                         month(date) %in% c(3,4,5)   ~ "spring",
                                         month(date) %in% c(6,7,8)   ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"))]

## drop taxa 
data <- data[!taxon %in% c("Sin asignar", "Bacillariophyta")]

rm(sites, sites2, sampling)
rm(list = ls()[grepl(pattern = "^bio", x = ls())])

# join ------------------------------------------------------------------------------

data <- rename(data, original_name = taxon)
data2 <- harmonization_table[data, on = "original_name"]  

data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

all (pull( data2[,uniqueN(x.coord), by = "site_id"], V1) == 1)

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_spain_duero_diatoms")]

# - check that gr_sample_id matches sample_id

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
        data.set = "spain_duero_diatom"
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
options(warn = -1)
updated_type <- data.table(site_id = rt$site_id)
for (i in 1:nrow(rt)) {
        i.rt <- rt[i,]
        i.plot_typology <-
                st_crop(plot_typology,
                        st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <-
                mapview(i.plot_typology,
                        zcol = "brt",
                        map.type = "OpenStreetMap.DE") + mapview(i.rt,
                                                                 popup = c("water_body"),
                                                                 color = "red")
        print(x)
        #i.bool <- "n"
        i.bool <- readline(paste(i, "/", nrow(rt), ":"))
        if (i.bool == "break") {
                break()
        } else if (i.bool == "n") {
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c") {
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        
        rm(list = ls()[grepl("i\\.", ls())])
}
# - drop remove sites 
data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

# temporal aggregation --------------------------------------------------------------
data10 <- data9[lubridate::month(date) %in% 5:9]
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data10, season_available = FALSE)
saveRDS(data10, "01_data/001_diatoms/001_original_data/spain_duero/final_aggregated.rds")
