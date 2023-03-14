## -- ANOSIM script     --- ### 


# -------------------------------
# date written: 07.03.22
# date last modified: 10.03.22
# Project: Evaluating European Broad River Types for Diatoms, Fish and Macrophytes
# Purpose: conduct Analysis of Similarities on all data 
# Notes: 
# -------------------------------

anosim.brt <- anosim(data[,-c(1:10)], distance = "jaccard", grouping = data$brt, parallel = 7)
anosim.ife <- anosim(data[,-c(1:10)], distance = "jaccard", grouping = data$ife, parallel = 7)
anosim.bgr <- anosim(data[,-c(1:10)], distance = "jaccard", grouping = data$bgr, parallel = 7)
anosim.few <- anosim(data[,-c(1:10)], distance = "jaccard", grouping = data$few, parallel = 7)
anosim.enz <- anosim(data[,-c(1:10)], distance = "jaccard", grouping = data$enz, parallel = 7)

anosim_fun1 <- function(x, typology){
        z <- data.table(
                statistic = x$statistic,
                p.value   = x$signif,
                typology = typology,
                taxon = taxon
        )
        return(z)
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



