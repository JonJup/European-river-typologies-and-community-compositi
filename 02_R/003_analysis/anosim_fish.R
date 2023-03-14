## -- compute anosim for fish 

# Purpose: Compute ANOSIM for fish 


# setup -----------------------------------------------------------------------------
library(data.table)
library(vegan)
data <- readRDS("01_data/002_fish/002_combined_data/03_no_rare_taxa.rds")
taxon <- "fish"

# - call ANOSIM script 
source("02_R/900_functions/anosim.R")

saveRDS(anosim_result, "01_data/004_results/fish_anosim.rds")
