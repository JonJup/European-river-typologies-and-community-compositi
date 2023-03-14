### ------------------------------ ###
### --- Denmark   Macrophytes  --- ### 
### ------------------------------ ###

# -------------------------------
# Purpose: Clean macrophyte data for Sweden from https://miljodata.slu.se/MVM 
# -------------------------------

# setup -----------------------------------------------
library(pacman)
p_load(
        data.table,
        magrittr,
        jjmisc,
        mapview,
        ggplot2,
        mapview,
        readxl,
        lubridate,
        stringdist,
        sf,
        dplyr,
        stringr,
        units
)
# load data -------------------------------------------------------------------------

data    <- read_excel("01_data/003_macrophytes/001_original_data/sweden_monitoring/raw/slu_mvm_220516_163417489_data.xlsx", sheet = 2)
harmonization_table <- readRDS("01_data/003_macrophytes/harmonization_table_macrophytes.rds")
typologies <- readRDS("01_data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------

#- remove old protocols 
setDT(data)
data<-data[!Undersökningstyp %in% c("Annan", "Makrofyter i sjöar v1")]


data2 <- data.table(
        original_site_name = data$`Nationellt övervakningsstations-ID`,
        date               = ymd(data$`Provtagningens startdatum`),
        taxon              = data$Taxonnamn,
        abundance          = data$Frekvens,
        y.coord            = data$`Stationskoordinat N/X`,
        x.coord            = data$`Stationskoordinat E/Y`,
        EPSG               = 3006,
        data.set           = "sweden_monitoring_macrophytes"
)

sites <- unique(data2, by = "original_site_name")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = data2$EPSG[1])
mapview(sites)
rm(sites, data)

data2[,c("season", "year") := .(case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                          month(date) %in% c(3,4,5)   ~ "spring",
                                          month(date) %in% c(6,7,8)   ~ "summer",
                                          month(date) %in% c(9,10,11) ~ "autumn"), 
                                year(date))]

data2 <- data2[!taxon %in% c("Algae", "Chlorophyta")]
data2 <- rename(data2, original_name = taxon)
data2 <- taxontable[data2, on = "original_name"]  
rm(taxontable, data, TU)

data2[, site_id := .GRP, by = "original_site_name"]
data2[, date_id := .GRP, by = "date"]

## add leading zeros
data2[, site_id := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data2[, date_id := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000",  date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00",   date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0",    date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(        date_id))]

## add gr_sample_id
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_sweden_monitoring_macrophytes")]

## reshape data
data3 <- data2[, list(
        gr_sample_id,
        original_site_name,
        date,
        year,
        season,
        site_id,
        date_id,
        original_name,
        species,
        genus,
        family,
        order,
        class,
        phylum,
        kingdom,
        abundance,
        x.coord,
        y.coord,
        EPSG,
        data.set
)]

## combine entries of same taxon
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

data3[, abundance := as.numeric(abundance)]
data3 <- unique(data3, by = c("gr_sample_id", "lowest.taxon"))
data4 <- copy(data3)
data5 <- add_typologies(data4)

# - subset to least impacted catchments 
data6 <- data5[least.impacted == TRUE]
sites <- unique(data6, by = "site_id") |> st_as_sf(coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])
## look for sites with different ID but same coordinates 
distances  <- st_distance(sites)
distances2 <- as.matrix(distances)
diag(distances2) <- 999
(duplicate_sites <- which(distances2 < units::as_units(1, "m")))

# - drop sites with very few species 
data6[, richness := uniqueN(lowest.taxon), by = "gr_sample_id"]
data7 <- data6#[richness > 10]

# - drop sites far removed from ECRINS river network 
data8 <- data7[distance < 300]

# - visually check the assignment of sites 
rt <- 
        data8 |> 
        unique(by = "site_id") |> 
        st_as_sf(coords = c("x.coord", "y.coord"), 
                 crs = data5$EPSG[1])

plot_typology <- st_crop(typologies, st_transform(sites, crs = st_crs(typologies)))

updated_type <- data.table(site_id = rt$site_id)

for (i in 1:nrow(rt)){
        i.rt <- rt[i, ]
        i.plot_typology <- st_crop(plot_typology, st_buffer(st_transform(i.rt, crs = st_crs(typologies)), dist =  2000))
        x <- mapview(i.plot_typology, 
                     zcol = "brt12"#,
                     #map.type = "OpenStreetMap.DE"
        ) + mapview(i.rt, color = "red")
        print(x)
        i.bool <- "n"
        i.bool <- readline(paste(i, ":"))
        if (i.bool == "break"){
                break()
        }
        if (i.bool == "n"){
                # remove_list[length(remove_list) + 1] <- i.rt$site_id 
                updated_type[site_id == i.rt$site_id, new_type := "drop"]
        } else if (i.bool == "c"){
                i.towhat <- readline("change to:")
                updated_type[site_id == i.rt$site_id, new_type := i.towhat]
        } else {
                updated_type[site_id == i.rt$site_id, new_type := i.rt$brt12]
        }
        
        rm(list = ls()[grepl("i\\.", ls())])
}
data9 <- updated_type[data8, on = "site_id"]

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

data9 <- data9[species != ""]
data9$brt12 |> unique()
data9[brt12 == "RT7", brt12 := "RT07"]
data9[brt12 == "RT8", brt12 := "RT08"]

# temporal aggregation --------------------------------------------------------------
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data9, season_available = FALSE)
saveRDS(data10, "01_data/003_macrophytes/001_original_data/czech_chmi/final_aggregated.rds")