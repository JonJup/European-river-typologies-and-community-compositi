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
drop_id <- which(names(data) %in% c("gr_sample_id", "brt", "ife", "bgr", "few", "enz", "null_model1_type", "null_model2_type", "null_model3_type","null_model4_type", "NA"))
sxs <- copy(data)
sxs <- sxs[, (drop_id) := NULL]

## loop variables 
# orders in zeta decline 
v.orders <- 10

## loop over typology systems 
for (t in c("brt", "ife", "bgr", "few", "enz")){
        
        if (t == "brt")  ls.rs <- list()
        
        ##extract types 
        types <- data[[t]]
        unique_types <- unique(types)
        
        ## loop over types 
        for (i in seq_along(unique_types)){
                
                if (!any(types == unique_types[i])) next()
                
                ## loop over taxonomic resolutions 
                #for (k in 1:4){
                        x <- 
                                Zeta.decline.ex(
                                        sxs[which(types == unique_types[i]),],
                                        orders = 1:v.orders
                                        )   
                        x2 <- x$zeta.val
                        x2 <- x2 / x2[1]
                        auc.var <- c()
                        for (auc in 1:(v.orders-1)){
                                auc.var[auc] <- (x2[auc] + x2[auc + 1])/2
                        }
                        auc.var <- sum(auc.var)
                        ls.rs[[length(ls.rs) + 1]] <- data.table(
                                order = x$zeta.order,
                                zeta_diversity = x2,
                                auc  = auc.var,
                                type = unique_types[i], 
                                typology = t
                        )
                        rm(list = ls()[grepl(pattern = "^k\\.", x = ls())])
        } ## END LOOP i types
} ## END LOOP t typology systems  

zeta <- rbindlist(ls.rs)


