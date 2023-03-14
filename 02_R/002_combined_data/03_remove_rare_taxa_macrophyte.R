### ------------------------- ###
### --- Drop rare Taxa    --- ###
### --- Macrophytes       --- ### 
### ------------------------- ###

# -------------------------------
# Purpose: Remove rare macrophyte taxa from data
# Notes: 
# -------------------------------

# setup -----------------------------------------------
library(pacman)
p_load(conflicted,
       data.table, 
       dplyr, 
       tidyr,
       magrittr
       )

# load data -------------------------------------------
data <- readRDS("01_data/003_macrophytes/002_combined_data/02_w_null_model.rds")

# prepare data ----------------------------------------
### --- remove rare types --- ###  
# - rename variable with broad river types
data  <- rename(data, brt = brt12)
# - extract sites, i.e., one entry per site
sites <- unique(data, by = "gr_sample_id")

# - create a list. each element is a table of how often each type occurs. 
# - Does not include neutral models. 
table.lst <- list(
        brt = table(sites$brt), 
        ife = table(sites$ife), 
        bgr = table(sites$bgr), 
        few = table(sites$few), 
        enz = table(sites$enz) 
)

# - loop until no type occurrs less than 20 times. 
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
## add a "presence" variable that is always 1. Zeros will be automatically added in the
## next step (pivot_wider).
data[, presence := 1]

#-  next step (pivot_wider).
data[, geometry := NULL]

data %<>%
        unique(by = c("species", "gr_sample_id")) %>%
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

occurrence_frequencies <- apply(data[,-c(1:11)],2,sum)
# - identify rare taxa and drop rare taxa. Store results in list "data2"
singletons <- which(occurrence_frequencies <= 1) %>% 
        names %>% 
        unique
data <- data[, (singletons) := NULL]

# save data -------------------------------------------
saveRDS(data, file = paste0("01_data/003_macrophytes/002_combined_data/03_no_rare_taxa.rds"))
