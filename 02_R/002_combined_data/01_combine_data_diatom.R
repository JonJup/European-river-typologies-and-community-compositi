# -- Combine data sets 
# --- diatoms # Purpose: Combine diatom data


# SETUP -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, fs, magrittr, sf, stringr, tmap)
source("02_R/900_functions/find_point.R")
# LOAD DATA -------------------------------------------------------------------------

## list of all data sets 
data.sets <- dir_ls("01_data/001_diatoms/001_original_data", type = "directory", regexp = "pre_", invert = TRUE)

data <- list()
for (i in seq_along(data.sets)){
        i.ds <- data.sets[i]
        print(paste("LOADING", str_remove(i.ds, "01_data/001_diatoms/001_original_data/")))
        i.files <- dir_ls(i.ds, regexp = "final_aggregated.rds")
        if(length(i.files) == 0) 
                i.files <- dir_ls(i.ds, regexp = "final_non_aggregated")
        if(length(i.files) == 0)
                next()
        i.x     <- readRDS(i.files)
        setDT(i.x)
        # - drop waterbody column if present 
        if ("waterbody" %in% names(i.x)){
                i.x[, waterbody := NULL]
        }
        data[[i]] <- i.x 
        rm(list = ls()[grepl(x = ls(), pattern = "^i\\.")])
}

# PREPARE DATA ----------------------------------------------------------------------
#- In these next steps, I apply several functions to all elements of the list "data", 
#- i.e. data sets to ensure that they are harmonized. 

#- Make sure all date variables are formatted as such:
data2 <- lapply(data, function(x) x[, date := as.Date(date)])
#- Make data spatial (sf) and transform to common coordinate reference system (LAEA Europe). 
data.st <- lapply(data2, function(x) st_as_sf(x, coords = c("x.coord", "y.coord"), crs = x$EPSG[1]))
data.st <- lapply(data.st, function(x) st_transform(x, crs = 3035))


#- Turn back into data.table to bind rows of list elements 
data2   <- lapply(data.st, setDT)
data2   <- rbindlist(data2, fill = TRUE, use.names = TRUE)
sample_counter <- data2[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter[, after.first.join := V1]
sample_counter[, V1 := NULL]

data2[, c("water_body", "sampling.events", "richness", "brtXbgr", "brtXife") := NULL ]
data2[ brt12 == "\"RT11\"" , brt12 := "RT11"]
data2[ brt12 ==  "\"RT02\"", brt12 := "RT02"]
data2[ brt12 == "\"RT04\"" , brt12 := "RT04"]
data2[ brt12 == "\"RT05\"" , brt12 := "RT05"]
data2[ brt12 == "\"RT07\"" , brt12 := "RT07"]
data2[ brt12 == "RT1"      , brt12 := "RT01"]
data2[ brt12 == "RT8"      , brt12 := "RT08"]
data2[brt12 == "RT2",        brt12 := "RT02"]
data2[brt12 == "RT3",        brt12 := "RT03"]
data2[brt12 == "RT4",        brt12 := "RT04"]
data2[brt12 == "RT5",        brt12 := "RT05"]
data2[brt12 == "RT6",        brt12 := "RT06"]
data2[brt12 == "RT7",        brt12 := "RT07"]
data2[brt12 == "RT9",        brt12 := "RT09"]
sort(data2[, unique(brt12)])
#- Remove data from catchments that are missing in the data from Lemm et al. 2021
data3 <- data2[!is.na(least.impacted)]

sample_counter2 <- data3[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter2[, after.removing.outliers := V1]
sample_counter2[, V1 := NULL]
sample_counter <- sample_counter[sample_counter2, on = "data.set"]

data3[,uniqueN(gr_sample_id), by = "data.set"]

#- Neither IFE nor brt12 having missing values ... 
data3[is.na(ife)]
data3[is.na(brt12)]

# unique(data3$brt12)
# data3$data.set |> unique()

# ——— Harmonize Taxonomy ——— # 

#- Here I want to make sure that the taxonomy is harmonized. The taxontable is constantly 
#- evolving so potentially errors can occur if data sets are combined with different
#- versions of the taxontable. To avoid this, I join the data with the most recent version
#- of the taxontable here again. 

#- Load taxontable and drop "clean" variable 
taxontable <- readRDS("01_data/001_diatoms/harmonization_table_diatoms.rds")
#taxontable[, clean := NULL]

#- Drop taxon variables except "original_name"
data3 %<>% select( - (species:kingdom))
#- Join data and taxontable
data4 <- taxontable[data3, on = "original_name"]

## combine entries of same taxon 
data4[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                           ifelse(!is.na(class), class,
                                                                  ifelse(!is.na(phylum), phylum, kingdom))))))]

data4 <- unique(data4, by = c("lowest.taxon", "gr_sample_id"))

# find duplicates -------------------------------------------------------------------
sites <- unique(data4, by = c("data.set", "site_id"))
sites %<>% st_as_sf()
distances <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 666
identical_points <- which(distances2< units::as_units(1, "m"))

drop_id <- c()
# what data sets are affected? 
for (i in seq_along(identical_points)){
        x <- find_point(identical_points[i], distances2)
        xx <- sites[x, ] |> pull(original_site_name)
        xxx <- data4[original_site_name %in% xx]
        ## are there more than these two samples? 
        if (uniqueN(xxx$gr_sample_id) > 2){
                ## check seasons 
                season.table <- xxx |>  unique(by = "gr_sample_id") %$% table(season)
                
                ## more than one sample in one season
                if (any(season.table > 1)){
                        seas.table.id <- which(season.table > 1)
                        seas.table.id <- names(season.table)[seas.table.id]
                        for (seas in seas.table.id){
                                ## do both samples share one date? 
                                if (xxx[season == seas, uniqueN(date)] == 1){
                                        ## drop the first one 
                                        drop_id <- append(drop_id, xxx[season == seas, unique(gr_sample_id)][1])
                                } else{
                                        older_sample <- xxx[season == seas, min(date)]
                                        drop_id <- append(drop_id, xxx[date == older_sample, unique(gr_sample_id)])
                                }
                        }
                }
                ## homogenize sites 
                site_id <- xxx[, unique(site_id)][1]
                data.set <- xxx[, unique(data.set)][1]
                
                data4[gr_sample_id %in% unique(xxx$gr_sample_id), c("site_id", "data.set") := .(site_id, data.set)]
        ### There are only these two samples for the site         
        } else {
                ### same season? 
                if (uniqueN(xxx$season) == 1){
                        ### same date? 
                        if (uniqueN(xxx$date) == 1){
                                
                                drop_id <- append(drop_id, xxx[, unique(gr_sample_id)][1])   
                                
                        ### different dates        
                        } else {   
                                older_sample <- xxx[, min(date)]
                                drop_id <- append(drop_id, xxx[date == older_sample, unique(gr_sample_id)])
                        }
                ### different seasons         
                } else {  
                        ## homogenize sites 
                        site_id_var  <- xxx[, unique(site_id)][1]
                        data.set_var <- xxx[, unique(data.set)][1]
                        data4[gr_sample_id %in% unique(xxx$gr_sample_id), c("site_id", "data.set") := .(site_id_var, data.set_var)]
                }
        }
}
data4 <- data4[!gr_sample_id %in% drop_id]

sample_counter2 <- data4[,uniqueN(gr_sample_id), by = "data.set"]
sample_counter2[, after.removing.duplicates := V1]
sample_counter2[, V1 := NULL]
sample_counter <- sample_counter[sample_counter2, on = "data.set"]


# ——— Seasons ——— # 
data4[, month := lubridate::month(date)]
data4 <- data4[month %in% 5:9]
data4[, c("month", "least.impacted","distance","site_id", "date_id", "abundance", "original_site_name", "season", "date", "kingdom", "phylum" ,"original_name", "class", "lowest.taxon") := NULL]

data4[, uniqueN(gr_sample_id), by = "data.set"]



# SAVE TO FILE ----------------------------------------------------------------------
saveRDS(data4, "01_data/001_diatoms/001_combined_data/01_combined_data_aggregated.rds")

