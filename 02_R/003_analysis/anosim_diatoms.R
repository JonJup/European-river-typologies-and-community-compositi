### ---------------------------------- ###
### --- compute anosim for diatoms --- ### 
### ---------------------------------- ###

# -------------------------------
# Purpose: Compute anosim for diatoms 
# -------------------------------

library(data.table)
library(vegan)

data <- readRDS("01_data/001_diatoms/002_combined_data/03_no_rare_taxa.rds")
taxon <- "diatoms"
source("02_R/900_functions/anosim.R")


saveRDS(anosim_result, "01_data/004_results/diatoms_anosim.rds")
