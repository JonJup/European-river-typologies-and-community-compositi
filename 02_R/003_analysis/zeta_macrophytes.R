### ---------------------------------------------- ###
### --- compute zeta diversity for macrophytes --- ### 
### ---------------------------------------------- ###

# -------------------------------
# Purpose: Compute zeta diversity for macrophytes 
# -------------------------------

library(data.table)
library(dplyr)
library(tidyr)
library(zetadiv)


## save setup to file 
data <- readRDS("01_data/003_macrophytes/002_combined_data/03_no_rare_taxa.rds")

source("02_R/900_functions/zeta.R")
zeta

saveRDS(zeta, "01_data/004_results/macrophytes_zeta.rds")

