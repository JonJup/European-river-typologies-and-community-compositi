# ————————————————————————— #
# ——— Combine data sets ——— # 
# ————————— Fish —————————— #
# ————————————————————————— #

# Purpose: Combine fish data

# SETUP -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, fs, magrittr, sf, stringr, tmap)
taxontable <- readRDS("01_data/002_fish/harmonization_table_fish.rds")
# LOAD DATA -------------------------------------------------------------------------

## list of all data sets 
data.sets <- dir_ls("01_data/002_fish/001_original_data", type = "directory", regexp = "pre_", invert = TRUE)
data      <- list()
## loop over all (currently 5) data sets to load them as elements of the list 
## "data"
for (i in seq_along(data.sets)){
        i.ds <- data.sets[i]
        print(paste("LOADING", str_remove(i.ds, "01_data/002_fish/001_original_data/")))
        i.files <- dir_ls(i.ds, regexp = "final_aggregated")
        if(length(i.files) == 0) 
                i.files <- dir_ls(i.ds, regexp = "final_non_aggregated")
        i.x     <- readRDS(i.files)
        data[[i]] <- i.x 
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
}

# PREPARE DATA ----------------------------------------------------------------------
#- In these next steps, I apply several functions to all elements of the list "data", 
#- i.e. data sets to ensure that they are harmonized. 

#- Make sure all date variables are formatted as such:
data2    <- lapply(data, function(x) x[, date := as.Date(date)])
#- Make data spatial (sf) and transform to common coordinate reference system (LAEA Europe). 
data.st <- lapply(data2, function(x) st_as_sf(x, coords = c("x.coord", "y.coord"), crs = x$EPSG[1]))
data.st <- lapply(data.st, function(x) st_transform(x, crs = 3035))
#- Turn back into data.table to bind rows of list elements 
data2   <- lapply(data.st, setDT)
data2   <- rbindlist(data2, fill = TRUE, use.names = TRUE)

sample_counter <- data2[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter[, after.first.join := V1]
sample_counter[, V1 := NULL]

# - how many samples 
data2[, uniqueN(gr_sample_id)]

data2 |> unique(by = "gr_sample_id") |> st_as_sf() |> mapview::mapview()

#- Remove data from catchments that are missing in the data from Lemm et al. 2021
data3 <- data2[!is.na(least.impacted)]
data2[is.na(least.impacted)]

# - harmonize names of broad river types 

data3[brt12 == "RT8", brt12 := "RT08"]
data3[brt12 == "RT2", brt12 := "RT02"]
data3[brt12 == "RT6", brt12 := "RT06"]
data3[brt12 == "RT4", brt12 := "RT04"]
data3[brt12 == "RT1", brt12 := "RT01"]
data3[brt12 == "RT7", brt12 := "RT07"]
data3[brt12 == "RT5", brt12 := "RT05"]
data3[brt12 == "RT3", brt12 := "RT03"]
data3[brt12 == "RT9", brt12 := "RT03"]
data3[brt12 ==  "\"RT01\"", brt12 := "RT01"]
data3[brt12 ==  "\"RT02\"", brt12 := "RT02"]
data3[brt12 ==  "\"RT03\"", brt12 := "RT03"]
data3[brt12 ==  "\"RT04\"", brt12 := "RT04"]
data3[brt12 ==  "\"RT05\"", brt12 := "RT05"]
data3[brt12 ==  "\"RT06\"", brt12 := "RT06"]
data3[brt12 ==  "\"RT07\"", brt12 := "RT07"]
data3[brt12 ==  "\"RT08\"", brt12 := "RT08"]
data3[brt12 ==  "\"RT09\"", brt12 := "RT09"]
data3[brt12 ==  "\"RT10\"", brt12 := "RT10"]
data3[brt12 ==  "\"RT11\"", brt12 := "RT11"]
data3[brt12 ==  "\"RT12\"", brt12 := "RT12"]

data3 <- data3[brt12 != ""]
data3 <- data3[lowest.taxon != ""]

sample_counter2 <- data3[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter2[, after.removing.outliers := V1]
sample_counter2[, V1 := NULL]
sample_counter <- sample_counter[sample_counter2, on = "data.set"]

# ——— Harmonize Taxonomy ——— # 

#- Here I want to make sure that the taxonomy is harmonized. The taxontable is constantly 
#- evolving so potentially errors can occur if data sets are combined with different
#- versions of the taxontable. To avoid this, I join the data with the most recent version
#- of the taxontable here again. 

#- Load taxontable and drop "clean" variable 

taxontable[, clean := NULL]

#- Drop taxon variables except "original_name"
data3 %<>% select( - (species:kingdom))
#- Join data and taxontable
data4 <- taxontable[data3, on = "original_name"]

# ——— Seasons ——— # 
# data4 <- list(spring = data4[season == "spring"], 
#               summer = data4[season == "summer"], 
#               autumn = data4[season == "autumn"])

data4[, month := lubridate::month(date)]
data4 <- data4[month %in% 5:9]
data4[, c("month", "i.subclass", "sampling.events", "richness", "least.impacted", "distance", "waterbody", "site_id", "date_id", "abundance", "original_site_name", "season", "date", "kingdom", "phylum", "subclass","original_name", "class") := NULL]
data4[brt12 == "RT2", brt12 := "RT02"]

sample_counter2 <- data4[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter2[, after.removing.outliers := V1]
sample_counter2[, V1 := NULL]
sample_counter <- sample_counter[sample_counter2, on = "data.set"]

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data4, "01_data/002_fish/002_combined_data/01_combined_data_aggregated.rds")

