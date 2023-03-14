### ---------------------- ###
### --- Zeta diversity --- ### 
### ---------------------- ###

# -------------------------------
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: compute zeta diversities 
# Notes: 
# -------------------------------

# setup -----------------------------------------------

## drop typology systems 
drop_id1 <- which(names(data[[1]]) %in% c("gr_sample_id", "brt", "ife", "bgr", "few", "enz", "null_model1_type", "null_model2_type", "null_model3_type","null_model4_type", "NA"))
drop_id2 <- which(names(data[[2]]) %in% c("gr_sample_id", "brt", "ife", "bgr", "few", "enz", "null_model1_type", "null_model2_type", "null_model3_type","null_model4_type", "NA"))
drop_id3 <- which(names(data[[3]]) %in% c("gr_sample_id", "brt", "ife", "bgr", "few", "enz", "null_model1_type", "null_model2_type", "null_model3_type","null_model4_type", "NA"))
drop_id4 <- which(names(data[[4]]) %in% c("gr_sample_id", "brt", "ife", "bgr", "few", "enz", "null_model1_type", "null_model2_type", "null_model3_type","null_model4_type", "NA"))
sxs <- copy(data)
sxs[[1]] <- sxs[[1]][, (drop_id1) := NULL]
sxs[[2]] <- sxs[[2]][, (drop_id2) := NULL]
sxs[[3]] <- sxs[[3]][, (drop_id3) := NULL]
sxs[[4]] <- sxs[[4]][, (drop_id4) := NULL]

## loop variables 
# orders in zeta decline 
v.orders <- 10


## loop over typology systems 
for (t in c("brt", "ife", "bgr", "few", "enz")){
        
        if (t == "brt")  ls.rs <- list()
        
        ##extract types 
        types <- data[[1]][[t]]
        unique_types <- unique(types)
        
        ## loop over types 
        for (i in seq_along(unique_types)){
                
                if (!any(types == unique_types[i])) next()
                
                ## loop over taxonomic resolutions 
                for (k in 1:4){
                        k.x <- 
                                Zeta.decline.ex(
                                        sxs[[k]][which(types == unique_types[i]),],
                                        orders = 1:v.orders
                                        )   
                        k.x2 <- k.x$zeta.val
                        k.x2 <- k.x2 / k.x2[1]
                        k.auc.var <- c()
                        for (auc in 1:(v.orders-1)){
                                k.auc.var[auc] <- (k.x2[auc] + k.x2[auc + 1])/2
                        }
                        k.auc.var <- sum(k.auc.var)
                        ls.rs[[length(ls.rs) + 1]] <- data.table(
                                order = k.x$zeta.order,
                                zeta_diversity = k.x2,
                                auc            = k.auc.var,
                                taxonomic_resolution = k,
                                type = unique_types[i], 
                                typology = t
                        )
                        rm(list = ls()[grepl(pattern = "^k\\.", x = ls())])
                } ## END LOOP k taxonomic resolution
        } ## END LOOP i types
} ## END LOOP t typology systems  

zeta <- rbindlist(ls.rs)



## plot 
# 
# zeta |>
#         unique(by = c("type", "taxonomic_resolution")) |>
#         ggplot(
#                aes(x = typology,
#                    y = auc)) +
#         geom_violin(draw_quantiles = 0.5) +
#         geom_jitter(width = 0.1, alpha = 0.4, aes(col = type)) +
#         facet_wrap(taxonomic_resolution ~ . )

