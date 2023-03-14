# -- Combine data sets 
# -- Macrophytes 

# date created: 09.12.21
# date last modified: 14.06.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: Combine macrophyte data

# SETUP -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, fs, magrittr, sf, stringr, tmap)

# LOAD DATA -------------------------------------------------------------------------

taxontable <- readRDS("01_data/003_macrophytes/harmonization_table_macrophytes.rds")

## list of all data sets 
data.sets <- dir_ls("01_data/003_macrophytes/001_original_data", type = "directory", regexp = "pre_", invert = TRUE)
data      <- list()
## loop over all (currently 5) data sets to load them as elements of the list 
## "data"
for (i in seq_along(data.sets)){
        i.ds <- data.sets[i]
        print(paste("LOADING", str_remove(i.ds, "01_data/003_macrophytes/001_original_data/")))
        i.files <- dir_ls(i.ds, regexp = "final_aggregated")
        if(length(i.files) == 0) 
                i.files <- dir_ls(i.ds, regexp = "final_non_aggregated")
        if(length(i.files) == 0)
                next()
        i.x     <- readRDS(i.files)
        data[[i]] <- i.x 
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
}


# PREPARE DATA ----------------------------------------------------------------------
#- In these next steps, I apply several functions to all elements of the list "data", 
#- i.e. data sets to ensure that they are harmonized. 

#- Make sure all date variables are formatted as such:
data2    <- lapply(data, function(x) x[, date := as.Date(date)])
lapply(data2, function(x) unique(x$EPSG))
#- Make data spatial (sf) and transform to common coordinate reference system (LAEA Europe). 
data.st <- lapply(data2, function(x) st_as_sf(x, coords = c("x.coord", "y.coord"), crs = x$EPSG[1]))
data.st <- lapply(data.st, function(x) st_transform(x, crs = 3035))
#- Turn back into data.table to bind rows of list elements 
data2   <- lapply(data.st, setDT)
data2   <- rbindlist(data2, fill = TRUE, use.names = TRUE)

#- Remove data from catchments that are missing in the data from Lemm et al. 2021
data3 <- data2[!is.na(least.impacted)]

sample_counter <- data2[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter[, after.first.join := V1]
sample_counter[, V1 := NULL]

#- Remove sites that were identified as outliers in PCCs 
sample_counter2 <- data3[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter2[, after.removing.outliers := V1]
sample_counter2[, V1 := NULL]
sample_counter <- sample_counter[sample_counter2, on = "data.set"]

data3[brt12 =="\"RT06\"", brt12 := "RT06"] 
data3[brt12 =="\"RT01\"", brt12 := "RT01"] 
data3[brt12 =="\"RT11\"", brt12 := "RT11"]
data3[brt12 =="\"RT12\"", brt12 := "RT12"] 
data3[brt12 =="\"RT02\"", brt12 := "RT02"] 
data3[brt12 =="\"RT03\"", brt12 := "RT03"] 
data3[brt12 =="\"RT07\"", brt12 := "RT07"] 
data3[brt12 =="\"RT04\"", brt12 := "RT04"]
data3[brt12 =="\"RT09\"", brt12 := "RT09"]
data3[brt12 ==     "RT1", brt12 := "RT01"]
data3[brt12 == "RT6"  , brt12 := "RT06"]
data3[brt12 == "RT8"  , brt12 := "RT08"]
data3[brt12 == "RT9"  , brt12 := "RT09"]
data3[brt12 == "RT4"  , brt12 := "RT04"]
data3[brt12 == "RT5"  , brt12 := "RT05"]
data3[brt12 == "RT3"  , brt12 := "RT03"]
data3[brt12 == "RT2"  , brt12 := "RT02"]
data3[brt12 == "RT7"  , brt12 := "RT07"]

data3[, unique(brt12)]

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
data4 <- data4[phylum != "Porifera"]

data4[, unique(taxon_state)]
data4[taxon_state == "moss", unique(phylum)]
# - fix typo
data4[taxon_state == "hydrophyte", taxon_state := "hydrophytes" ]

# - in how many data sets do which life forms occur? 
check_taxa <- copy(data4)
check_taxa <- unique(data4, by = c("taxon_state", "data.set"))
table(check_taxa$taxon_state)
which(!data4[, unique(data.set)] %in% data4[taxon_state == "moss", unique(data.set)])
data4[, unique(data.set)][c(4,14)]
data4 <- data4[taxon_state %in% c("hydrophytes", "helophytes", "moss")]

# - check remaining kingdoms 
data4[, unique(kingdom)]
# - count phyla per data set
data4[, uniqueN(phylum), by = "data.set"]
# - All have two or three phyla. 
data4[, uniqueN(data.set), by = "phylum"]
# - the difference is machantiophyta which does not occur in seven of the data sets
## combine entries of same taxon 
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(subclass), subclass,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom)))))))]
#- check for dupicate sites 
sites <- unique(data4, by = c("data.set", "site_id"))
sites %<>% st_as_sf()
distances <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 666
which(distances2< units::as_units(1, "m"))

### ——— remove sites with only one taxon ——— ### 
richness <- copy(data4)
richness[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
richness <- unique(richness, by = "gr_sample_id")
table(richness$richness)
drop_id <- richness[richness == 1, unique(gr_sample_id)]
data4 <- data4[! gr_sample_id %in% drop_id]

rm(drop_id, richness)

# ——— Seasons ——— # 
unique(data4$brt12)
data4[, month := lubridate::month(date)]
data4[data.set == "sweden_macrophytes_leo", month := 5]
data4 <- data4[month %in% 5:9]

sample_counter2 <- data4[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter2[, after.focal.month := V1]
sample_counter2[, V1 := NULL]
sample_counter <- sample_counter[sample_counter2, on = "data.set"]

data4[, c("month", "least.impacted","distance","site_id", "date_id", "abundance", "original_site_name", "season", "date", "kingdom", "phylum" ,"original_name", "class", "lowest.taxon", "date.set", "sampling.method", "sampling.events", "subclass") := NULL]

#- new richness
data4[, richness := uniqueN(species), by = "gr_sample_id"]

# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data4, paste0("01_data/003_macrophytes/combined_data/01_",Sys.Date(),"_combined_data_aggregated.rds"))

