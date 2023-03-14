# ———————————————————————————————————————————————————————————————— #
# ——— Clean macrophyes data from Germany - Schleswig-Holstein  ——— # 
# ———————————————————————————————————————————————————————————————— #

# ———————————————————————————————————
# date first written: 24-11-21
# date last modified: 24-11-21
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: In this script I create a harmonized spatial macrophyte data set from the raw 
# data from Germany - Schleswig-Holstein.  
# Temporal aggregation: 
# EPSG:   
# ————————————————


# setup -----------------------------------------------------------------------------
#devtools::install_github("https://github.com/jonjup/jjmisc")
library(data.table)
library(dplyr)
library(jjmisc)
library(lubridate)
library(mapview)
library(sf)
library(stringr)
library(readxl)

# load data -------------------------------------------------------------------------
bio_wd1  <- "data/macrophytes/original_data/germany_schleswig_holstein/raw/SH_Makrophyten_Phytobenthos_Messstellen_2016-2020.xlsx"
bio_wd2  <- "data/macrophytes/original_data/germany_schleswig_holstein/raw/SH_Makrophyten_Phytobenthos_Messswerte_2016-2020.xlsx"

bio     <- read_excel(bio_wd1)

taxontable <- readRDS("data/fish/2021-11-23_taxontable_fish.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------
## subset to only fish abundances 
bio

## reshape and rename 
data <- 
        data.table(
                original_site_name = bio$MS_NR,
                x.coord = bio$`UTM-32 East`,
                y.coord = bio$`UTM-32 North`,
                taxon = bio$`wissenschaftl. Artname`,
                abundance = bio$`Anzahl Gesamt`,
                EPSG = 32632 
        )

sites  <- unique(bio, by = "MS_NR")
add2 <- function(x) x + 20000000

xp1 <- sites |> pull(`UTM-32 East`)|> word(start = 1, end = 7, sep = "")
xp2 <- sites |> pull(`UTM-32 East`)|> word(start = 7, end = 9, sep = "")
xp <- paste(xp1, xp2, sep = ".") 


sites3 <- 
tibble(id = 1:537, 
       x.coord = xp, 
       y.coord = sites$`UTM-32 North`) |> st_as_sf(coords = c("x.coord", "y.coord"), crs = 25832) 
mapview(sites3)


sites$`UTM-32 East`[1]
557991.25
sites_ETRS89 <- st_as_sf(sites, coords = c("UTM-32 East", "UTM-32 North"), crs = 25832) 
sites_WGS89  <- st_as_sf(sites, coords = c("UTM-32 East", "UTM-32 North"), crs = 32632) 

mapview(sites_ETRS89)
mapview(sites_WGS89)


library(tmap)
tmap_mode("view")
tm_shape(sites2) + tm_dots()

## remove sites without coordinates
data <- data[!is.na(x.coord)]
data <- data[!is.na(taxon)]
data <- data[taxon != ""]

## —— Inspect taxa ———————————————————————————————————————————————————————————————————————
(TU <- unique(data$taxon) |> sort())
(TU <- setdiff(TU,taxontable$original_name))

taxontable <- update_taxonomy2(TU)
taxontable[clean == FALSE]
saveRDS(taxontable, paste0("data/fish/",Sys.Date(),"_taxontable_fish.rds"))



names(data)[which(names(data) == "taxon")] <- "original_name"
data2 <- taxontable[data, on = "original_name"]

sort(unique(data2$kingdom))
sort(unique(data2$phylum))
sort(unique(data2$class))
sort(unique(data2$subclass))
sort(unique(data2$order))

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_", "spain_ebro_fish")]

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
        data.set = "spain_ebro_fish"
)]

## combine entries of same taxon 
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]
data3[, abundance := as.numeric(abundance)]
data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
data5 <- add_typologies(data4)

## visual checks 
sites <- unique(data5, by = "gr_sample_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

mapview(sites, zcol = "brt12")
mapview(sites, zcol = "ife")
mapview(sites, zcol = "bgr")
mapview(sites, zcol = "least.impacted")

## save to file 
saveRDS(data5, paste0("data/fish/original_data/spain_ebro/",Sys.Date(),"_final_non_aggregated.rds"))
data5 <- readRDS("data/fish/original_data/spain_ebro/2021-11-24_final_non_aggregated.rds")

# temporal aggregation --------------------------------------------------------------
##aggregation necessary? 
agg <- data5 |> unique(by = "gr_sample_id") 
unique(table(agg$site_id))

## yes  
source("R/functions/newest_sample.R")
data6 <- newest_sample(data5)
saveRDS(data6, paste0("data/fish/original_data/spain_ebro/",Sys.Date(),"_final_aggregated.rds"))

# statistics -------------------------------------------------------------------------
# time span
summary(data5$year)
# number of sites 
uniqueN(data5$site_id)
# number of samples 
uniqueN(data5$gr_sample_id)
# most recent 
data6[, uniqueN(gr_sample_id)]
# reference condition
data6[least.impacted == TRUE, uniqueN(site_id)]
data6[least.impacted == TRUE, uniqueN(gr_sample_id)]
