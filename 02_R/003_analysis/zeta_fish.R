### --------------------------------------- ###
### --- compute zeta diversity for fish --- ### 
### --------------------------------------- ###

# -------------------------------
# Purpose: Compute zeta diversity for fish 
# -------------------------------

library(pacman)
p_load(data.table, rstudioapi, zetadiv)

data <- readRDS("01_data/002_fish/002_combined_data/03_no_rare_taxa.rds")

source("02_R/900_functions/zeta.R")
zeta

saveRDS(zeta, "01_data/004_results/fish_zeta.rds")
