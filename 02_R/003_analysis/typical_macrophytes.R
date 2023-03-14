### --------------------------------------- ###
### --- compute typical for macrophytes --- ### 
### --------------------------------------- ###

# -------------------------------
# Purpose: Compute typical communities for macrophytes 
# last changes: compute for macropyhtes including bryphytes
# -------------------------------


# setup ----------------------------------------------------------------------------
library(data.table)
library(vegan)
library(dplyr)
library(DoE.base)

taxon <- "macrophytes"

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/003_macrophytes/002_combined_data/03_no_rare_taxa.rds")

# compute similarity between typical assemblages ------------------------------------
source("02_R/900_functions/typical.R")
res_lst2

saveRDS(res_lst2, "01_data/004_results/macrophytes_typical.rds")
