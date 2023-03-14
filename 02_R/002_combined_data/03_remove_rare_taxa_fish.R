### --- Drop rare Taxa    --- ###
### --- Fish              --- ### 

# Purpose: Remove rare fish taxa from data

# setup -----------------------------------------------
library(pacman)
p_load(data.table, dplyr, tidyr)

# load data -------------------------------------------
data <- readRDS("01_data/002_fish/002_combined_data/02_w_null_model.rds")

# prepare data ----------------------------------------
data <- rename(data, brt = brt12)

# - get one row per sample 
sites <- unique(data, by = "gr_sample_id")

# - count samples within each type of each typology 
table.lst <- list(
        brt = table(sites$brt), 
        ife = table(sites$ife), 
        bgr = table(sites$bgr), 
        few = table(sites$few), 
        enz = table(sites$enz) 
                  )
# - drop types with less than 20 samples

while (any(unlist(table.lst)<20)){
        
        if (any(table.lst$brt <20)){
                rare_types <- names(which(table.lst$brt<20))        
                data <- data[! brt %in% rare_types]
        }
        if (any(table.lst$ife<20)){
                rare_types <- names(which(table.lst$ife<20))        
                data <- data[! ife %in% rare_types]
        }
        if (any(table.lst$bgr<20)){
                rare_types <- names(which(table.lst$bgr<20))        
                data <- data[! bgr %in% rare_types]
        }
        if (any(table.lst$few<20)){
                rare_types <- names(which(table.lst$few<20))        
                data <- data[! few %in% rare_types]
        }
        if (any(table.lst$enz<20)){
                rare_types <- names(which(table.lst$enz<20))        
                data <- data[! enz %in% rare_types]
        }
        sites <- unique(data, by = "gr_sample_id")
        table.lst <- list(
                brt = table(sites$brt), 
                ife = table(sites$ife), 
                bgr = table(sites$bgr), 
                few = table(sites$few), 
                enz = table(sites$enz) 
        )
        
}

# - add a "presence" variable that is always 1. Zeros will be automatically added in the
# - next step (pivot_wider).
data[, presence := 1]
data %<>%
        unique(by = c("species", "gr_sample_id")) %>%
        select(!geometry) %>%
        pivot_wider(
                id_cols = c(
                        "gr_sample_id",
                        "brt",
                        "ife",
                        "bgr",
                        "few",
                        "enz",
                        "null_model1_type",
                        "null_model2_type",
                        "null_model3_type",
                        "null_model4_type"
                ),
                names_from = "species",
                values_from = "presence",
                values_fill = 0
        ) %>%
        setDT %>%
        {\(x) x[, "NA" := NULL]}()

occurrence_frequencies <- apply(data[,-c(1:10)],2,sum)

# - drop singletons
singletons <- which(occurrence_frequencies <= 1) %>% 
        names %>% 
        unique
data <- data[, (singletons) := NULL]

# save data -------------------------------------------
saveRDS(data, file = "01_data/002_fish/002_combined_data/03_no_rare_taxa.rds")
