### -------------------------------------- ###
### --- compute anosim for macrophytes --- ### 
### -------------------------------------- ###

# -------------------------------
# Purpose: Compute anosim for macrophytes 
# Notes: 
# -------------------------------

library(data.table)
library(vegan)

data <- readRDS("01_data/003_macrophytes/002_combined_data/03_no_rare_taxa.rds")
taxon <- "macrophytes"

source("02_R/003_analysis/anosim.R")
anosim_result

saveRDS(anosim_result, "01_data/004_results/macrophytes_anosim.rds")
