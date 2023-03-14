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
       tidyr
       )

# load data -------------------------------------------
data <- readRDS("01_data/003_macrophytes/002_combined_data/02_2022-12-12_w_null_model.rds")

# prepare data ----------------------------------------
### --- remove rare types --- ###  
# - rename variable with broad river types
data  <- rename(data, brt = brt12)
# - extract sites, i.e., one entry per site
sites <- unique(data, by = "gr_sample_id")

table.sites <- table(sites$year)
sum(table.sites[c(9:15)]) / sum(table.sites)

length(table.sites)

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


data[, uniqueN(gr_sample_id), by = "data.set"]

## add a "presence" variable that is always 1. Zeros will be automatically added in the
## next step (pivot_wider).
data[, presence := 1]

#-  next step (pivot_wider).
data[, presence := 1]
data[, geometry := NULL]

# - all of them include NA so report the number - 1 
data$species |> uniqueN()
data$genus |> uniqueN()
data$family |> uniqueN()
data$order |> uniqueN()


data0 <- copy(data)
data0[, richness := uniqueN(species), by = "gr_sample_id"]
data0 <- unique(data0, by = "gr_sample_id")
library(ggplot2)
ggplot(data0, aes(x = brt, y = richness)) + 
        geom_violin(,draw_quantiles = .5) +
        facet_wrap(.~taxon_state)
geom_point(alpha = 0.1)


piv <- function(x) {
        ub <- c(x, "gr_sample_id")
        data |> 
                unique(by=ub) |> 
                pivot_wider(id_cols = c("gr_sample_id", "brt", "ife", "bgr", 
                                        "few", "enz", "null_model1_type", "null_model2_type",
                                        "null_model3_type", "null_model4_type"), 
                            names_from = x, 
                            values_from = "presence", 
                            values_fill = 0) -> 
                out 
        return(out)
        
}


# -  pivot wider to site X taxon format
data.spe <- piv("species") |> setDT() |> {\(x) x[, "NA" := NULL]}()
data.gen <- piv("genus")   |> setDT() 
data.fam <- piv("family")  |> setDT() 
data.ord <- piv("order")   |> setDT() 

data.ls <- list(data.spe, data.gen, data.fam, data.ord)

occurrence_frequencies <-  lapply(data.ls, function(x) apply(x[,-c(1:7)],2,sum))

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

# - what proportion of observations is at species level? 
nrow(data[!is.na(species)])/nrow(data)
data[is.na(species), table(genus)] |> sort()

## How much is one percent of the number of sites? 
occurrence_threshold <- 1# lapply(data.ls, function(x) round(0.01 * nrow(x)))


# - identify rare taxa and drop rare taxa. Store results in list "data2"
data2 <- list()
for (i in seq_along(data.ls)){
        i.rare      <- which(occurrence_frequencies[[i]] <= occurrence_threshold)
        i.remove <- names(i.rare)
        i.remove <- unique(i.remove)
        if (length(i.remove) == 0) {
                next()
                rm(list = ls()[grep(pattern = "^i\\.", x = ls())])
                }
        data2[[i]] <- data.ls[[i]][, (i.remove) := NULL]
       
}
#data2 <- data.ls
# analyze --------------------------------------------

## Remaining number of taxa species
data2[[1]] |> ncol() |> {\(x) x - 10}()
## Remaining number of taxa genera
data2[[2]] |> ncol() |> {\(x) x - 10}()
## Remaining number of taxa families
data2[[3]] |> ncol() |> {\(x) x - 10}()
## Remaining number of taxa orders
data2[[4]] |> ncol() |> {\(x) x - 10}()


rowSums(data2[[1]][, -c(1:10)]) |> summary()
rowSums(data2[[2]][, -c(1:10)]) |> summary()
rowSums(data2[[3]][, -c(1:10)]) |> summary()
rowSums(data2[[4]][, -c(1:10)]) |> summary()

data2[[1]] <- data2[[1]][which(rowSums(data2[[1]][, -c(1:10)]) > 2), ]
data2[[2]] <- data2[[2]][which(rowSums(data2[[1]][, -c(1:10)]) > 2), ]
data2[[3]] <- data2[[3]][which(rowSums(data2[[1]][, -c(1:10)]) > 2), ]
data2[[4]] <- data2[[4]][which(rowSums(data2[[1]][, -c(1:10)]) > 2), ]

# save data -------------------------------------------
saveRDS(data2, file = paste0("01_data/003_macrophytes/002_combined_data/03_no_rare_taxa.rds"))
