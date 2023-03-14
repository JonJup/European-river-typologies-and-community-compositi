### -------------------------------- ###
# --- Combine all typology systems --- #
### -------------------------------- ###

# Purpose: Combine the Broad River Types, Illies Freshwater Ecoregions, Biogeographic Regions, Freshwater Ecoregions of the world, and Environmental Zones. 
# The data used in this script are available here: 
#  —— BRT: 
#  —— IFE: https://www.eea.europa.eu/data-and-maps/data/ecoregions-for-rivers-and-lakes 
#  —— BGR: https://www.eea.europa.eu/data-and-maps/data/biogeographical-regions-europe-3. 
#  —— FEW: https://www.feow.org/download
#  —— EnZ: https://datashare.ed.ac.uk/handle/10283/3091


# The file lemm_least_impacted was created in the scirpt 001_identify_least_impacted_fecs.R in the same folder as this script.  

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, sf, dplyr, magrittr, mapview)

# load data -------------------------------------------------------------------------
brt <- st_read("01_data/typologies/m_river_fec_broad_type/m_river_fec_broad_type.shp")
ill <- st_read("01_data/typologies/illies/Ecoregions.shp")
fec <- st_read("01_data/lemm_least_impacted.gpkg")
feo <- st_read("01_data/typologies/freshwater_ecoregions_of_the_world/feow_hydrosheds.shp")
bgr <- st_read("01_data/typologies/bgr/BiogeoRegions2016.shp")
enz <- st_read("01_data/typologies/environmental_zones/EnSv8/enz_v8.shp")


# prepare data ----------------------------------------------------------------------

#- Subset each data set to the relevant variables and give them informative names
bgr <- select(bgr, bgr = short_name)
bgr %<>% filter(bgr != "outside")
ill <- select(ill, illies = NAME)
ill %<>% st_transform(crs = st_crs(bgr))
brt %<>% select(brt = m_btype12, m_zhyd)
st_crs(feo) <- "EPSG:4326"
feo %<>% select(feow = FEOW_ID) %>% st_transform(crs = st_crs(ill))
feo <- st_crop(feo, brt)
enz %<>% select(enz = EnZ_name) %>% st_transform(crs = st_crs(ill))
fec %<>% st_transform(crs = st_crs(ill))

# Join data -------------------------------------------------------------------------
brt2 <- st_join(brt, ill)
bgr2 <- st_cast(bgr, "POLYGON") 
brt2 <- st_join(brt2, bgr2)
brt2 <- st_join(brt2, enz)
brt2 <- st_join(brt2, feo)


brt2 %<>% select(!least_disturbed)
brt2 %<>% select(!fec_number)
# new approach for least disturbed sites. Loop over and evaluate 
for (i in 1:nrow(brt2)) {
        #- Feedback
        print(paste(i, "/", nrow(brt2)))
        #- Intersect broad river type element with FEC layer with least disturbed classification 
        i.inter <- st_intersects(brt2[i,], fec)
        #- If there is no intersection assign NA to least disturbed and jump to next
        #- iteration
        if (length(i.inter[[1]])==0){
                brt2$least_disturbed[i] <- NA
                next()
        }
        #- extract line numbers from list
        i.inter <- i.inter[[1]]
        #- find FEC with biggest overlap if multiple intersect
        if (length(i.inter) > 1) {
                j.vec <- c()
                for (j in 1:length(i.inter)) {
                        j.diff <- st_difference(brt2[i,], fec[i.inter[j], ])
                        j.vec[j] <-
                                ifelse(nrow(j.diff) == 0,
                                       0,
                                       st_length(j.diff) / st_length(brt2[i,]))
                }
                i.inter <- i.inter[which.min(j.vec)]
                rm(j)
                rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
           
        }
        #- how much of the stream is not within the FEC
        i.diff <- st_difference(brt2[i,], fec[i.inter, ])
        #- If all of the stream is within the FEC i.diff is an empty table. Thus we
        #- replace - it with 0 instead of computing the length ratio. - If some of the
        #- stream is outsite the FEC we compute the ration of length outside to length
        #- inside. The smaller the better. 
        if (nrow(i.diff) == 0) {
                i.diff2 <- 0
        } else if (nrow(i.diff) != 0) {
                i.diff2 <- st_length(i.diff) / st_length(brt2[i, ])
        }
        #- diff2 is a units object. Turn to numeric for evaluation with thresholds.
        i.diff2 <- as.numeric(i.diff2)
        #- if more than two thirds of the river segment lie outside the catchment with the largest overlap. Drop it. 
        if (i.diff2 > 0.33) {
                brt2$least_disturbed[i] <- NA
        } else if (i.diff2 < 0.32) {
                brt2$least_disturbed[i] <- fec$least.impacted[i.inter]
        }
        
        #- quick save
        if (i %% 500 == 0) {
                saveRDS(brt2, paste0("01_data/typologies/quick_save_least_disturbed_delete_me/quicksave",i,".rds"))
        }
        
        rm(i)
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

# save to file  ---------------------------------------------------------------------
saveRDS(brt2, "data/all_typologies.rds")
st_write(brt2, "data/all_typologies.gpkg")
