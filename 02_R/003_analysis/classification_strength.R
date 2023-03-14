## -- classification strength analysis 

# purpose : Conduct classification strength analysis

# setup -----------------------------------------------------------------------------
pacman::p_load(
        data.table, 
        magrittr,
        parallelDist
        )

# functions -------------------------------------------------------------------------
compute_cs <- function (dist, grouping, typology) 
{
        grouping.u <- unique(grouping)
        dist2 <- as.matrix(dist)
        dist2 <- 1 - dist2
        for (k in seq_along(grouping.u)) {
                if (k == 1) 
                        wts <- bts <- c()
                k.id1 <- which(grouping == grouping.u[k])
                k.id.n1 <- which(grouping != grouping.u[k])
                k.sim1 <- dist2[k.id1, k.id1]
                k.sim.n1 <- dist2[k.id1, k.id.n1]
                k.ut <- k.sim1[upper.tri(k.sim1)]
                k.lt <- k.sim1[lower.tri(k.sim1)]
                k.ut.n <- k.sim.n1[upper.tri(k.sim.n1)]
                k.lt.n <- k.sim.n1[lower.tri(k.sim.n1)]
                wts[k] <- mean(append(k.ut, k.lt), na.rm = T)
                bts[k] <- mean(append(k.ut.n, k.lt.n), na.rm = T)
                rm(list = ls()[grepl(x = ls(), pattern = "^k\\.")])
                rm(k)
        }
        props <- round(proportions(table(grouping)), 2)
        props <- data.frame(type = names(props), proportion = c(props))
        csj <- data.frame(within_type = wts, between_type = bts, 
                          type = grouping.u, typlogy = typology)
        if (any(is.na(csj$within_type))){
                csj <- csj[-which(is.na(csj$within_type)), ]
        }
        csj$type = as.character(csj$type)
        csj <- dplyr::left_join(x = csj, y = props, by = "type")
        csj <- dplyr::mutate(dplyr::mutate(dplyr::mutate(csj, within_weighted = within_type * 
                                                                 proportion, between_type_mean = mean(csj$between_type)), 
                                           within_weighted_sum = sum(within_weighted)), classification_strength = within_weighted_sum - 
                                     between_type_mean)
        return(csj)
}



# load data -------------------------------------------------------------------------
diatoms     <- readRDS("01_data/001_diatoms/002_combined_data/03_no_rare_taxa.rds")
fishes      <- readRDS("01_data/002_fish/002_combined_data/03_no_rare_taxa.rds")
macrophytes <- readRDS("01_data/003_macrophytes/002_combined_data/03_no_rare_taxa.rds")

# prepare data ----------------------------------------------------------------------

# - drop non-taxa columns
diat.bio <- lapply(diatoms    , function(x) x[, -c(1:10)])
fish.bio <- lapply(fishes     , function(x) x[, -c(1:10)])
macr.bio <- lapply(macrophytes, function(x) x[, -c(1:10)])

diat.bio <- lapply(diat.bio, function(x) if ("NA" %in% names(x)){x[, NA := NULL]}else{x})
fish.bio <- lapply(fish.bio, function(x) if ("NA" %in% names(x)){x[, NA := NULL]}else{x})
macr.bio <- lapply(macr.bio, function(x) if ("NA" %in% names(x)){x[, "NA" := NULL]}else{x})

# - turn into matrix - necessary to compute distance table
diat.bio %<>% lapply(as.matrix)
fish.bio %<>% lapply(as.matrix)
macr.bio %<>% lapply(as.matrix)


# - create jaccard distance tables
diat.dist <- lapply(diat.bio, parallelDist, method = "binary")
fish.dist <- lapply(fish.bio, parallelDist, method = "binary")
macr.dist <- lapply(macr.bio, parallelDist, method = "binary")

# ——— compute classification strengths ——— # 

# - list that hold all three data sets. This makes it easier to loop
distance_list   <- list(diat.dist, fish.dist, macr.dist)
taxon_list      <- list(diatoms, fishes, macrophytes)
results_list    <- list()

# - loop over taxa
for (i in 1:3){
        print(paste("i = ", i))
        
        # - loop over taxonomic resolutions 
        for(j in 1:3) {
                print(paste("  j = ", j))
                
                # - loop over typology systems
                for (k in 1:9){
                        print(paste("    k = ", k))
                        k.res <- compute_cs(
                                dist = distance_list[[i]][[j]],
                                grouping = taxon_list[[i]][[j]][[1 + k]], 
                                typology = c("brt", "ife", "bgr", "few", "enz", "null_model1_type", "null_model2_type", "null_model3_type", "null_model4_type")[k]
                        )
                        k.res$resolution <- c("species", "genus", "family", "order")[j]  
                        k.res$taxon      <- c("diatom", "fish", "macrophytes")[i]  
                        results_list[[length(results_list) + 1 ]]  <-   k.res
                        rm(k.res)
                }
        }
        
}

res <- rbindlist(results_list)

saveRDS(res, "01_data/004_results/classification_strength.rds")
        