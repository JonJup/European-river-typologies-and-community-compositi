### ------------------------------------------- ###
# -------- ADD LEAST IMPACTED TO LEMM ----------- # 
### ------------------------------------------- ###

# ____________________________
# Purpose: Define least impacted rivers catchments based on the data of Lemm, J. U., M. Venohr, L. Globevnik, K. Stefanidis, Y. Panagopoulos, J. Gils, L. Posthuma, P. Kristensen, C. K. Feld, J. Mahnkopf, D. Hering, and S. Birk. 2021. Multiple stressors determine river ecological status at the European scale: Towards an integrated understanding of river status deterioration. Global Change Biology 27:1962–1975.
# The MultipleStress_RiverEcoStatus.shp file is availalble under https://zenodo.org/record/4322819
#___________________________

# setup -----------------------------------------------------------------------------
library(pacman)
p_load(sf,
       ggplot2,
       data.table,
       magrittr,
       mapview,
       dplyr,
       rstudioapi,
       tidyr)

x<-getActiveDocumentContext()
sink(file = paste0("R/identification_of_least_impaired_sites/log_files/001","_", Sys.Date(), "_", "log.txt"))
Sys.Date()
x$path
sessionInfo()
sink(file = NULL)
rm(x)
# load data -------------------------------------------------------------------------
data  <- st_read("01_data/Lemm_2021/MultipleStress_RiverEcoStatus.shp")
# prepare data -----------------------------------------------------------------------
# - backup 
data2 <- data
setDT(data2)

#- set negative Nitrogen concentrations to zero
data2[LoadTN_Are < 0, LoadTN_Are := 0]


# find optimal threshold -------------------------------------------------------------
theshold_vector <- seq(0.05, 1, by = 0.0125)
max_quantile =1
save_list <- list()
for (i in seq_along(theshold_vector)){
        threshold <- theshold_vector[i]
        
        data2 |>
                group_by(mars_bt12) |>
                summarize(
                        maximum.TP = quantile(LoadTPArea, max_quantile),
                        minimum.TP = min(LoadTPArea),
                        maximum.TN = quantile(LoadTN_Are, max_quantile),
                        minimum.TN = min(LoadTN_Are),
                        maximum.ub = quantile(lu_r_urb, max_quantile),
                        minimum.ub = min(lu_r_urb),
                        maximum.ag = quantile(lu_r_agr, max_quantile),
                        minimum.ag = min(lu_r_agr),
                        maximum.ma = quantile(hy_maf_abs, max_quantile),
                        minimum.ma = min(hy_maf_abs),
                        maximum.bf = quantile(hy_bfi_abs, max_quantile),
                        minimum.bf = min(hy_bfi_abs),
                        maximum.hc = quantile(msPAFP5EC5, max_quantile),
                        minimum.hc = min(msPAFP5EC5),
                        
                ) |>
                mutate(
                        range.TP = maximum.TP - minimum.TP,
                        range.TN = maximum.TN - minimum.TN,
                        range.ub = maximum.ub - minimum.ub,
                        range.ag = maximum.ag - minimum.ag,
                        range.ma = maximum.ma - minimum.ma,
                        range.bf = maximum.bf - minimum.bf,
                        range.hc = maximum.hc - minimum.hc,
                        
                ) |>
                right_join(data2) |>
                mutate(
                        TP.scale = (LoadTPArea - minimum.TP) / range.TP,
                        TN.scale = (LoadTN_Are - minimum.TP) / range.TP,
                        ub.scale = (lu_r_urb - minimum.ub) / range.ub,
                        ag.scale = (lu_r_agr - minimum.ag) / range.ag,
                        ma.scale = (hy_maf_abs - minimum.ma) / range.ma,
                        bf.scale = (hy_bfi_abs - minimum.bf) / range.bf,
                        hc.scale = (msPAFP5EC5 - minimum.hc) / range.hc
                ) |>
                mutate(
                        TP.bool = TP.scale <= threshold,
                        TN.bool = TN.scale <= threshold,
                        ub.bool = ub.scale <= threshold,
                        ag.bool = ag.scale <= threshold,
                        ma.bool = ma.scale <= threshold,
                        bf.bool = bf.scale <= threshold,
                        hc.bool = hc.scale <= threshold
                ) |> 
                #- create least.impacted.score. An integer, which is the sum of var.bool variables for each FEC. 
                mutate(
                        least.impacted.score = TP.bool + TN.bool + ub.bool + ag.bool + ma.bool + bf.bool + hc.bool    
                ) |> 
                #- create least.impacted. A binary variable that indicates whether a FEC is least impacted
                #- (1) or not (0). 
                #- A FEC is considered least impacted (least.imapacted == 1) if it leas.impacted.score is equal to 7. 
                #- This means that all variables are in the third of its type's range.  
                mutate(
                        least.impacted = least.impacted.score >= 7
                ) ->
                data3 
        
        frequencies <- data.table(table(data3$eco_stat_2, data3$least.impacted))
        target <- c("good","high")
        good <- frequencies[V1 %in% target & V2 == TRUE, sum(N)]/(frequencies[V1 %in% target & V2 == TRUE, sum(N)] + frequencies[V1 %in% target & V2 == FALSE, sum(N)])
        target <- c("bad", "moderate","poor")
        bad <- frequencies[V1 %in% target & V2 == TRUE, sum(N)]/(frequencies[V1 %in% target & V2 == TRUE, sum(N)] + frequencies[V1 %in% target & V2 == FALSE, sum(N)])
        
        percentage <- c(good, bad)
        quality <- c("good", "bad")
        
        
        save_list[[i]] <-
                data.table(threshold,
                           percentage,
                           quality)
        
        
}

sensitivity_results <- rbindlist(save_list)

ggplot(sensitivity_results, aes(x = threshold, y = percentage, col = quality)) + 
        geom_line(size = 1) +
        ylab("percentage of sites catregorized as least disturbed") 

ggsave(filename = "fig/least_impacted/precent_least_disturbed.png", width = 4, height = 4)

sensitivity_results_wide <- pivot_wider(sensitivity_results, id_cols = "threshold", names_from = "quality", values_from = percentage)
sensitivity_results_wide$difference <- sensitivity_results_wide$good - sensitivity_results_wide$bad

ggplot(sensitivity_results_wide, aes(x = threshold, y = difference)) + 
        geom_line(lwd = 0.6) + 
        geom_vline(xintercept = .24, lty = 2) + 
        ylab("%good - %bad")
ggsave(filename = "fig/least_impacted/difference_good_bad.png", width = 4, height = 4)


# delineate least disturbed sites with optimal theshold -----------------------------
threshold = 0.24

data2 |>
        group_by(mars_bt12) |>
        summarize(
                maximum.TP = quantile(LoadTPArea, 1),
                minimum.TP = min(LoadTPArea),
                maximum.TN = quantile(LoadTN_Are, 1),
                minimum.TN = min(LoadTN_Are),
                maximum.ub = quantile(lu_r_urb, 1),
                minimum.ub = min(lu_r_urb),
                maximum.ag = quantile(lu_r_agr, 1),
                minimum.ag = min(lu_r_agr),
                maximum.ma = quantile(hy_maf_abs, 1),
                minimum.ma = min(hy_maf_abs),
                maximum.bf = quantile(hy_bfi_abs, 1),
                minimum.bf = min(hy_bfi_abs),
                maximum.hc = quantile(msPAFP5EC5, 1),
                minimum.hc = min(msPAFP5EC5),
                
        ) |>
        mutate(
                range.TP = maximum.TP - minimum.TP,
                range.TN = maximum.TN - minimum.TN,
                range.ub = maximum.ub - minimum.ub,
                range.ag = maximum.ag - minimum.ag,
                range.ma = maximum.ma - minimum.ma,
                range.bf = maximum.bf - minimum.bf,
                range.hc = maximum.hc - minimum.hc,
                
        ) |>
        right_join(data2) |>
        mutate(
                TP.scale = (LoadTPArea - minimum.TP) / range.TP,
                TN.scale = (LoadTN_Are - minimum.TP) / range.TP,
                ub.scale = (lu_r_urb - minimum.ub) / range.ub,
                ag.scale = (lu_r_agr - minimum.ag) / range.ag,
                ma.scale = (hy_maf_abs - minimum.ma) / range.ma,
                bf.scale = (hy_bfi_abs - minimum.bf) / range.bf,
                hc.scale = (msPAFP5EC5 - minimum.hc) / range.hc
        ) |>
        mutate(
                TP.bool = TP.scale <= threshold,
                TN.bool = TN.scale <= threshold,
                ub.bool = ub.scale <= threshold,
                ag.bool = ag.scale <= threshold,
                ma.bool = ma.scale <= threshold,
                bf.bool = bf.scale <= threshold,
                hc.bool = hc.scale <= threshold
        ) |> 
        #- create least.impacted.score. An integer, which is the sum of var.bool variables for each FEC. 
        mutate(
                least.impacted.score = TP.bool + TN.bool + ub.bool + ag.bool + ma.bool + bf.bool + hc.bool    
        ) |> 
        #- create least.impacted. A binary variable that indicates whether a FEC is least impacted
        #- (1) or not (0). 
        #- A FEC is considered least impacted (least.imapacted == 1) if it leas.impacted.score is equal to 7. 
        #- This means that all variables are in the third of its type's range.  
        mutate(
                least.impacted = least.impacted.score >= 7
              ) ->
        data3 


# derive summary statistics ---------------------------------------------------------
setDT(data3)

data.mean <- data3[least.impacted==TRUE,lapply(.SD, mean), by = "mars_bt12", .SDcols = c(26:32)]
data.sd   <- data3[least.impacted==TRUE,lapply(.SD, sd), by = "mars_bt12", .SDcols = c(26:32)]

data.mean %<>% pivot_longer(cols = !mars_bt12, values_to = "mean")
data.sd %<>% pivot_longer(cols = !mars_bt12, values_to = "sd")

data.mean_sd <- left_join(data.mean, 
          data.sd, 
          by = c("mars_bt12", "name"))

data.ld <- data3[least.impacted==TRUE]
data.plot <- pivot_longer(data.ld[, c(1,26:32)], cols = !mars_bt12)

ggplot(data.plot, aes(x = mars_bt12, y = value)) +
        geom_boxplot() + 
        facet_grid(name~., scales = "free")

#- reformat to final data set 
data4 <- 
        data3 |> 
        st_as_sf() |> 
        select(least.impacted)

# mapview(data4, zcol = "least.impacted")
# 
# sum(data4$least.impacted)
data4 <- st_make_valid(data4)

# save to file  ---------------------------------------------------------------------
st_write(data4, "01_data/lemm_least_impacted.gpkg")
