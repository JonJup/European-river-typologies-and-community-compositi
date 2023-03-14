## -- Assign null model to types 

# created: 20.06.22
# modified: 12.12.22
# project : Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# purpose : Add the spatially constrained null model to data 


# setup -----------------------------------------------------------------------------
library(pacman)
p_load(conflicted, sf, data.table, dplyr, magrittr, mapview)

# load data -------------------------------------------------------------------------
diatom <- readRDS("01_data/001_diatoms/combined_data/01_combined_data_aggregated.rds")
fishes <- readRDS("01_data/002_fish/combined_data/01_combined_data_aggregated.rds")
macorp <- readRDS("01_data/003_macrophytes/combined_data/01_combined_data_aggregated.rds")

null.model1 <- readRDS("01_data/005_spatial_neutral/spatial_null_hl.rds")
null.model2 <- readRDS("01_data/005_spatial_neutral/spatial_null_hs.rds")
null.model3 <- readRDS("01_data/005_spatial_neutral/spatial_null_sl.rds")
null.model4 <- readRDS("01_data/005_spatial_neutral/spatial_null_ss.rds")

# prepare data ----------------------------------------------------------------------
diatom.sites <- unique(diatom, by = "gr_sample_id") |> st_as_sf() |> select("gr_sample_id")
macorp.sites <- unique(macorp, by = "gr_sample_id") |> st_as_sf() |> select("gr_sample_id")
fishes.sites <- unique(fishes, by = "gr_sample_id") |> st_as_sf() |> select("gr_sample_id")

null.model1$null_model1_type = 1:nrow(null.model1)
null.model2$null_model2_type = 1:nrow(null.model2)
null.model3$null_model3_type = 1:nrow(null.model3)
null.model4$null_model4_type = 1:nrow(null.model4)

# add null model --------------------------------------------------------------------
diatom.sites %<>% st_join(null.model1) %>% st_join(null.model2) %>% st_join(null.model3) %>% st_join(null.model4)
macorp.sites %<>% st_join(null.model1) %>% st_join(null.model2) %>% st_join(null.model3) %>% st_join(null.model4)
fishes.sites %<>% st_join(null.model1) %>% st_join(null.model2) %>% st_join(null.model3) %>% st_join(null.model4)

# check on map ----------------------------------------------------------------------
#mapview(null.model, zcol = "null_model_type") + mapview(diatom.sites, zcol = "null_model_type")
#mapview(null.model, zcol = "null_model_type") + mapview(macorp.sites, zcol = "null_model_type")
#mapview(null.model, zcol = "null_model_type") + mapview(fishes.sites, zcol = "null_model_type")

# add back to data ------------------------------------------------------------------
setDT(diatom.sites)
setDT(macorp.sites)
setDT(fishes.sites)

diatom.sites[, geometry := NULL]
macorp.sites[, geometry := NULL]
fishes.sites[, geometry := NULL]

diatom2 <- diatom.sites[diatom, on = "gr_sample_id"]
macorp2 <- macorp.sites[macorp, on = "gr_sample_id"]
fishes2 <- fishes.sites[fishes, on = "gr_sample_id"]

# save to file  ---------------------------------------------------------------------
saveRDS(diatom2, "01_data/001_diatoms/combined_data/02_w_null_model.rds")
saveRDS(fishes2, "01_data/002_fish/combined_data/02_w_null_model.rds")
saveRDS(macorp2, "01_data/003_macrophytes/combined_data/02_w_null_model.rds")


