### ------------------------- ###
### --- Drop rare Taxa    --- ###
### --- Diatoms           --- ### 
### ------------------------- ###

# ------------------------------- # 
# Purpose: Remove rare diatom taxa from data
# Notes: 
# ------------------------------- # 

# setup -----------------------------------------------
library(pacman)
p_load(data.table, 
      dplyr, 
      tidyr)

# load data -------------------------------------------
data <- readRDS("01_data/001_diatoms/002_combined_data/02_2022-12-12_w_null_model.rds")

# prepare data ----------------------------------------
data <- rename(data, brt = brt12)
sites <- unique(data, by = "gr_sample_id")

# table.sites <- table(sites$year)
# sum(table.sites[c(10, 11,12,13,14,15,16)]) / sum(table.sites)
# 
# length(table.sites)

table.lst <- list(
        brt = table(sites$brt), 
        ife = table(sites$ife), 
        bgr = table(sites$bgr), 
        few = table(sites$few), 
        enz = table(sites$enz) 
)


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

data[, uniqueN(gr_sample_id), by = "data.set"]


#-  add a "presence" variable that is always 1. Zeros will be automatically added in the
#-  next step (pivot_wider).
data[, presence := 1]
data[, geometry := NULL]

data[, uniqueN(gr_sample_id), by = "data.set"]

## -- check for richness trends 
data0 <- copy(data)
data0[, richness := uniqueN(fixed_name), by = "gr_sample_id"]
data0 <- unique(data0, by = "gr_sample_id")

library(ggplot2)
ggplot(data0, aes(x = brt, y = richness)) + 
        geom_violin(,draw_quantiles = .5)  
        geom_point(alpha = 0.1)

# - all of them include NA so report the number - 1 
data$species |> uniqueN()
data$genus |> uniqueN()
data$family |> uniqueN()
data$order |> uniqueN()


piv <- function(x) {
        ub <- c(x, "gr_sample_id")
        data |> 
                unique(by=ub) |> 
                pivot_wider(id_cols = all_of(c("gr_sample_id", "brt", "ife", "bgr", 
                                        "few", "enz", "null_model1_type", "null_model2_type", 
                                        "null_model3_type", "null_model4_type")), 
                            names_from = x, 
                            values_from = "presence", 
                            values_fill = 0) -> 
                out 
        return(out)
        
}

# -  pivot wider to site X taxon format
data.spe <- piv("species") |> setDT() |> {\(x) x[, "NA" := NULL]}()
data.gen <- piv("genus")   |> setDT() |> {\(x) x[, "NA" := NULL]}()
data.fam <- piv("family")  |> setDT() |> {\(x) x[, "NA" := NULL]}()
data.ord <- piv("order")   |> setDT() |> {\(x) x[, "NA" := NULL]}()

data.ls <- list(data.spe, data.gen, data.fam, data.ord)

occurrence_frequencies <- lapply(data.ls, function(x) apply(x[,-c(1:7)],2,sum))

# - what are the three most common species 
data.spe |> select(-c(1:10)) |> apply(2,sum) |> sort(decreasing = T) |> {\(x) x[1:3]}()
# - how many singletons 
data.spe |> select(-c(1:10)) |> apply(2,sum) |> {\(x) x == 1}() |> sum()
# - what is the mean number of occurrences? And the SD? 
data.spe |> select(-c(1:10)) |> apply(2,sum) |> mean()
data.spe |> select(-c(1:10)) |> apply(2,sum) |> sd()
# - what is the mean species richness? And the SD? 
data.spe |> select(-c(1:10)) |> apply(1,sum) |> mean()
data.spe |> select(-c(1:10)) |> apply(1,sum) |> sd()

## How much is one percent of the number of sites? 
occurrence_threshold <- 1# lapply(data.ls, function(x) round(0.01 * nrow(x)))


# - identify rare taxa and drop rare taxa. Store results in list "data2"
data2 <- list()
for (i in seq_along(data.ls)){
        i.rare      <- which(occurrence_frequencies[[i]] <= occurrence_threshold)
        i.remove <- names(i.rare)
        i.remove <- unique(i.remove)
        data2[[i]] <- data.ls[[i]][, (i.remove) := NULL]
        rm(list = ls()[grep(pattern = "^i\\.", x = ls())])
}

# analyze --------------------------------------------

## Remaining number of taxa species
data2[[1]] |> ncol() |> {\(x) x - 7}()
## Remaining number of taxa genera
data2[[2]] |> ncol() |> {\(x) x - 7}()
## Remaining number of taxa families
data2[[3]] |> ncol() |> {\(x) x - 7}()
## Remaining number of taxa orders
data2[[4]] |> ncol() |> {\(x) x - 7}()

data2[[1]] <- data2[[1]][which(rowSums(data2[[1]][, -c(1:10)]) > 1), ]
data2[[2]] <- data2[[2]][which(rowSums(data2[[1]][, -c(1:10)]) > 1), ]
data2[[3]] <- data2[[3]][which(rowSums(data2[[1]][, -c(1:10)]) > 1), ]
data2[[4]] <- data2[[4]][which(rowSums(data2[[1]][, -c(1:10)]) > 1), ]

# save data -------------------------------------------
saveRDS(data2, file = paste0("01_data/001_diatoms/002_combined_data/03_no_rare_taxa.rds"))
