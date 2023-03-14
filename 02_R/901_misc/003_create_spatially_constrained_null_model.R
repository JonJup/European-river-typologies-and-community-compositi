### ------------------------------------------- ###
# --- Create spatially constrained null model --- # 
### ------------------------------------------- ###

# Purpose: Create a grid of hexagons across Europe that can be used as spatially constrained null model for the analyses. 

# setup -----------------------------------------------
library(pacman)
p_load(
        data.table,
        dplyr,
        magrittr,
        mapview,
        sf,
        rstudioapi
)

# load data -------------------------------------------
dia <- readRDS("01_data/001_diatoms/002_combined_data/01_combined_data_aggregated.rds")
mph <- readRDS("01_data/003_macrophytes/002_combined_data/01_combined_data_aggregated.rds")
fsh <- readRDS("01_data/002_fish/002_combined_data/01_combined_data_aggregated.rds")

# prepare data ----------------------------------------
#- subset each to unique sites 
sites_d <- unique(dia, by = "gr_sample_id")
sites_m <- unique(mph, by = "gr_sample_id")
sites_f <- unique(fsh, by = "gr_sample_id")

sites_d %<>% st_as_sf() %>% select(gr_sample_id)
sites_m %<>% st_as_sf() %>% select(gr_sample_id)
sites_f %<>% st_as_sf() %>% select(gr_sample_id)

sites <- bind_rows(sites_d, sites_m, sites_f)

hex_grid_l <- st_make_grid(x = sites, square = FALSE,cellsize = 1000000)
squ_grid_l <- st_make_grid(x = sites, square = TRUE ,cellsize = 1000000)
hex_grid_s <- st_make_grid(x = sites, square = FALSE,cellsize = 500000 )
squ_grid_s <- st_make_grid(x = sites, square = TRUE ,cellsize = 500000 )

mapview(hex_grid_l) 
mapview(hex_grid_l) + mapview(squ_grid_l)
mapview(hex_grid_s) + mapview(squ_grid_s)
mapview(hex_grid_l) + mapview(hex_grid_s)
mapview(squ_grid_l) + mapview(squ_grid_s)

# - reduce to cells with observations 
hex_grid_l.2 <- hex_grid_l[which(lengths(st_contains(hex_grid_l, sites)) > 0)] |> st_as_sf()
hex_grid_s.2 <- hex_grid_s[which(lengths(st_contains(hex_grid_s, sites)) > 0)] |> st_as_sf()
squ_grid_l.2 <- squ_grid_l[which(lengths(st_contains(squ_grid_l, sites)) > 0)] |> st_as_sf()
squ_grid_s.2 <- squ_grid_s[which(lengths(st_contains(squ_grid_s, sites)) > 0)] |> st_as_sf()

mapview(hex_grid_l.2)
mapview(hex_grid_s.2)
mapview(squ_grid_l.2)
mapview(squ_grid_s.2)

grid2 %<>% st_as_sf()
square_grid2 %<>% st_as_sf()

# save data -------------------------------------------
saveRDS(hex_grid_l.2, "01_data/005_spatial_neutral/spatial_null_hl.rds")
saveRDS(hex_grid_s.2, "01_data/005_spatial_neutral/spatial_null_hs.rds")
saveRDS(squ_grid_l.2, "01_data/005_spatial_neutral/spatial_null_sl.rds")
saveRDS(squ_grid_s.2, "01_data/005_spatial_neutral/spatial_null_ss.rds")



