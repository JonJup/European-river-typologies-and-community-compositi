## -- ANOSIM script     --- ### 


# -------------------------------
# date written: 07.03.22
# date last modified: 10.03.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: conduct Analysis of Similarities on all data 
# Notes: 
# -------------------------------

anosim.brt <- lapply(data, function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$brt, parallel = 7))
anosim.ife <- lapply(data, function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$ife, parallel = 7))
anosim.bgr <- lapply(data, function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$bgr, parallel = 7))
anosim.few <- lapply(data, function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$few, parallel = 7))
anosim.enz <- lapply(data, function(x) anosim(x[,-c(1:10)], distance = "jaccard", grouping = x$enz, parallel = 7))

anosim_fun1 <- function(x, typology){
        y <- purrr::transpose(x)
        z <- data.table(
                statistic = unlist(y$statistic),
                p.value   = unlist(y$signif),
                taxonomic.resolution = c("species", "genus", "family", "order"),
                typology = typology,
                taxon = taxon
        )
        z
}

anosim_result <- 
        rbindlist(
        list(
                anosim_fun1(anosim.brt, "brt"),
                anosim_fun1(anosim.ife, "ife"),
                anosim_fun1(anosim.bgr, "bgr"),   
                anosim_fun1(anosim.few, "feow"),   
                anosim_fun1(anosim.enz, "enz")  
        )
)



