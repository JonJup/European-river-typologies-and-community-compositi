### -------------------------------- ###
### --- compute typical for fish --- ### 
### -------------------------------- ###

# -------------------------------
# Purpose: Compute typical communities for fish 
# Notes: 
# -------------------------------

library(data.table)
library(vegan)
library(dplyr)
library(DoE.base)

data <- readRDS("01_data/002_fish/002_combined_data/03_no_rare_taxa.rds")
taxon <- "fish"

source("02_R/900_functions/typical.R")
res_lst2

saveRDS(res_lst2, "01_data/004_results/fish_typical.rds")
beepr::beep()