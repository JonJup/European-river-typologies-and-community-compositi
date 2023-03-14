### ------------------------------------------ ###
### --- compute zeta diversity for diatoms --- ### 
### ------------------------------------------ ###

# -------------------------------
# Purpose: Compute zeta diversity for diatoms 
# -------------------------------

library(pacman)
p_load(data.table, rstudioapi, zetadiv)

data <- readRDS("01_data/001_diatoms/002_combined_data/03_no_rare_taxa.rds")

source("02_R/900_functions/zeta.R")
zeta

saveRDS(zeta, "01_data/004_results/diatom_zeta.rds")
