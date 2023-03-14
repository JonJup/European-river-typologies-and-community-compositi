### ----------------------------------- ###
### --- compute typical for diatoms --- ### 
### ----------------------------------- ###

# -------------------------------
# Purpose: Compute typical communities for diatoms 
# -------------------------------

library(data.table)
library(vegan)
library(dplyr)
library(DoE.base)

data <- readRDS("01_data/001_diatoms/002_combined_data/01_combined_data_aggregated.rds")
taxon <- "diatoms"
source("02_R/900_functions/typical.R")
res_lst2

saveRDS(res_lst2, "01_data/004_results/diatoms_typical.rds")
