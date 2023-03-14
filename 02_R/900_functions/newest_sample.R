newest_sample <- function(x, season_available = TRUE) {
        
        if (season_available == TRUE){
        spring <- x[season == "spring"]
        if (nrow(spring) > 0){
                spring[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
                spring[, newest_date := max(date), by = "site_id"]
                spring <- spring[sampling.events == 1 | date == newest_date]                
        }
        summer <- x[season == "summer"]
        if (nrow(summer) > 0){
                summer[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
                summer[, newest_date := max(date), by = "site_id"]
                summer <- summer[sampling.events == 1 | date == newest_date]                
        }
        autumn <- x[season == "autumn"]
        if (nrow(autumn) > 0){
                autumn[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
                autumn[, newest_date := max(date), by = "site_id"]    
                autumn <- autumn[sampling.events == 1 | date == newest_date]                
        }
        winter <- x[season == "winter"]
        if (nrow(winter) > 0){
                winter[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
                winter[, newest_date := max(date), by = "site_id"]
                winter <- winter[sampling.events == 1 | date == newest_date]                
        }
        
        
        out <- rbindlist(list(spring, summer, autumn,winter), fill = TRUE)
        out[, newest_date := NULL]
        return(out)
        } else {
                y <- copy(x)
                y[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
                y[, newest_date := max(date), by = "site_id"]
                y <- y[sampling.events == 1 | date == newest_date]  
                y[, newest_date := NULL]
                return(y)
        }
        
        
        
}


        
 