### --------------------------- ###
### --- Poland Macrophytes  --- ### 
### --------------------------- ###

# -------------------------------
# Purpose: Clean macrophyte data provided by Piotr Panek for Poland
# -------------------------------

# setup -----------------------------------------------
library(data.table)
library(magrittr)
library(mapview)
library(ggplot2)
library(readxl)
library(lubridate)
library(stringdist)
library(sf)
library(dplyr)

source("02_R/900_functions/add_typologies.R")
# load data -------------------------------------------------------------------------

bio <- read_excel("data/diatoms/original_data/poland_monitoring/raw/FB MF PL RW 2016-2020.xlsx")
taxontable <- readRDS("data/macrophytes/2022-06-13_taxontable_macrophytes.rds")
typologies <- readRDS("data/all_typologies.rds")

# prepare data ----------------------------------------------------------------------
bio2 <- bio

names(bio2) <- c("sample_id", "water_body", "remove1", "remove4", "y.coord", "x.coord", "date", "remove2", "remove3", "taxon", "remove5", "remove6", "abundance")

bio2 <- bio2 |> 
        select(!contains("remove")) |> 
        filter(!is.na(abundance)) |> 
        mutate(date = ymd_hm(date),
                 EPSG = 4326, 
                 data.set = "poland_monitoring_macrophytes") |> 
        mutate(date = date(date))
setDT(bio2)
bio2[,c("season", "year") := .(case_when(month(date) %in% c(12,1,2)  ~ "winter",
                                             month(date) %in% c(3,4,5)   ~ "spring",
                                             month(date) %in% c(6,7,8)   ~ "summer",
                                             month(date) %in% c(9,10,11) ~ "autumn"), 
                                year(date))]

sites <- unique(bio2, by = "sample_id")
sites <- st_as_sf(sites, coords = c("x.coord", "y.coord"), crs = "EPSG:4326")
mapview(sites)


data <- bio2 
rm(bio, bio2); gc()

TU <- data$taxon |>  unique()
TU <- setdiff(TU, taxontable$original_name)

# - need to call function from dfm script 
# taxontable <- dfm$update_taxonomy_macrophytes(TU, taxontable_arg = taxontable)
# taxontable[original_name == "Polygonium mite", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(
#         "Polygonium mite", "Polygonium", "Polygonaceae", "Caryophyllales", NA, "Magnoliopsida", "Tracheophyta", "Plantae"
# )]
# 
# taxontable[original_name == "Zbiorowisko z Leptodictyum riparium", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(
#         "Leptodictyum riparium", "Leptodictyum", "Amblystegiaceae", "Hypnales", NA, "Bryopsida", "Bryophyta", "Plantae"
# )]
# taxontable[original_name == "Zbiorowisko z Platyhypnidium riparioides", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(
#         "Platyhypnidium riparioides", "Platyhypnidium", "Brachytheciaceae", "Hypnales", NA, "Bryopsida", "Bryophyta", "Plantae"
# )]
# taxontable[original_name == "Cordiophorus sp. (Rhacomitrium sp.)", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(
#         NA, "Racomitrium", "Grimmiaceae", "Grimmiales", NA, "Bryopsida", "Bryophyta", "Plantae"
# )]
# 
# taxontable[genus == "green_algae"]
# 
# taxontable[original_name == "", taxon_state := "drop"]
# taxontable[original_name =="Myosotis palustris",                                        taxon_state := "drop"]
# taxontable[original_name =="Callitriche sp.",                                           taxon_state := "hydrophytes"]
# taxontable[original_name =="Vaucheria sp.",                                             taxon_state := "yellow_green_algae"]
# taxontable[original_name =="Zbiorowisko z Leptodictyum riparium",                       taxon_state := "moss"]
# taxontable[original_name =="Oedogonium sp.",                                            taxon_state := "green algae"]
# taxontable[original_name =="Rhizoclonium sp.",                                          taxon_state := "green_algae"]
# taxontable[original_name =="Cladophora sp.",                                            taxon_state := "green_algae"]
# taxontable[original_name =="Nitella sp.",                                               taxon_state := "green_algae"]
# taxontable[original_name =="Petasites sp.",                                             taxon_state := "drop"]
# taxontable[original_name =="Salix sp.",                                                 taxon_state := "drop"]
# taxontable[original_name =="Phormidium sp.",                                            taxon_state := "cyanobacteria"]
# taxontable[original_name =="Lyngbya sp.",                                               taxon_state := "cyanobacteria"]
# taxontable[original_name =="Mougeotia sp.",                                             taxon_state := "green_algae"]
# taxontable[original_name =="Spirogyra sp.",                                             taxon_state := "green_algae"]
# taxontable[original_name =="Ulothrix sp.",                                              taxon_state := "green_algae"]
# taxontable[original_name =="Fissidens sp.",                                             taxon_state := "moss"]
# taxontable[original_name =="Zbiorowisko z Platyhypnidium riparioides",                  taxon_state := "moss"]
# taxontable[original_name =="Schistidium sp.",                                           taxon_state := "moss"]
# taxontable[original_name =="Bryum sp.",                                                 taxon_state := "moss"]
# taxontable[original_name =="Sciuro-hypnum plumosum (Brachythecium plumosum)",           taxon_state := "moss"]
# taxontable[original_name =="Pellia sp.",                                                taxon_state := "liverwort"]
# taxontable[original_name =="Chiloscyphus sp.",                                          taxon_state := "liverwort"]
# taxontable[original_name =="Stigeoclonium sp.",                                         taxon_state := "green_algae"]
# taxontable[original_name =="Hygroamblystegium sp.",                                     taxon_state := "moss"]
# taxontable[original_name =="Hygrohypnum sp.",                                           taxon_state := "moss"]
# taxontable[original_name =="Cratoneuron sp.",                                           taxon_state := "moss"]
# taxontable[original_name =="Rhizoclonium",                                              taxon_state := "green algae"]
# taxontable[original_name =="Spirogyra",                                                 taxon_state := "green algae"]
# taxontable[original_name =="Armoracia sp.",                                             taxon_state := "drop"]
# taxontable[original_name =="Palustriella communata",                                    taxon_state := "moss"]
# taxontable[original_name =="Chara sp.",                                                 taxon_state := "green_algae"]
# taxontable[original_name =="Juncus sp.",                                                taxon_state := "drop"]
# taxontable[original_name =="Audouinella sp.",                                           taxon_state := "red_algae"]
# 
# missing_ts <- unique(taxontable[taxon_state == ""]$original_name)
# 
# for (i in seq_along(missing_ts)){
#         i.miss <- missing_ts[i]
#         i.genu <- taxontable[original_name == i.miss]$genus
#         i.state <- taxontable[genus == i.genu, unique(taxon_state)]
#         i.state <- i.state[-which(i.state == "")]
#         if (uniqueN(i.state) == 1){
#                 taxontable[original_name == i.miss, taxon_state := i.state]
#         }
#         rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
# }
# taxontable[genus == "Polygonum", taxon_state := "drop"]
# taxontable[original_name =="Zygnema sp.",                                               taxon_state := "green algae"]
# taxontable[original_name =="Oscillatoriales",                                           taxon_state := "cyanobacteria"]
# taxontable[original_name =="Cordiophorus sp. (Rhacomitrium sp.)",                       taxon_state := "moss"]
# taxontable[original_name =="Nitzschia subtilis",                                        taxon_state := "diatoms"]
# taxontable[original_name =="Ranunculus sp.",                                            taxon_state := "drop"]
# taxontable[original_name =="Polygonum nodosum",                                         taxon_state := "drop"]
# taxontable[original_name =="Juncus tenageia",                                           taxon_state := "drop"]
# taxontable[original_name =="Epilobium sp.",                                             taxon_state := "drop"]
# taxontable[original_name =="Equisetum limosum",                                         taxon_state := "helophyte"]
# taxontable[original_name =="Polygonium mite",                                           taxon_state := "drop"]
# taxontable[original_name =="Angelica palustris",                                        taxon_state := "drop"]
# taxontable[original_name =="Drepanaldia sp.",                                           taxon_state := "green algae"]
# taxontable[original_name =="Carex appropinquata",                                       taxon_state := "drop"]

#saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))

data <- rename(data, original_name = taxon)
data2 <- taxontable[data, on = "original_name"]  
rm(taxontable, data, TU)

data2[, date_id := .GRP, by = "date"]

## add site and date ids for this I need to round coordinates because some samples are
## categorized as from different sites even though they are from the same.
data2[, c("x.coord_round", "y.coord_round") := .(round(x.coord, 5), 
                                                 round(y.coord, 5))]

data2[, site_id := .GRP, by = c("x.coord_round", "y.coord_round")]
data2[, site_id := as.numeric(site_id)]

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
data2[,gr_sample_id := paste0("site_", site_id, "_date_", date_id, "_poland_monitoring_macrophytes")]

# - reshape data
data3 <- data2[, list(
        gr_sample_id,
        original_site_name = sample_id,
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

# - combine entries of same taxon
data3[, lowest.taxon := ifelse(!is.na(species), species,
                               ifelse(!is.na(genus), genus,
                                      ifelse(!is.na(family), family,
                                             ifelse(!is.na(order), order,
                                                    ifelse(!is.na(class), class,
                                                           ifelse(!is.na(phylum), phylum, kingdom))))))]

data3[, abundance := as.numeric(abundance)]
data3[, abundance := sum(abundance), by = c("gr_sample_id", "lowest.taxon")]
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
        x <- mapview::mapview(i.plot_typology, 
                              zcol = "brt"#,
                              #map.type = "OpenStreetMap.DE"
                              ) + mapview::mapview(i.rt, color = "red")
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


data9 <- left_join(data8, 
                   updated_type, 
                   by = "site_id")

#- drop "drop" rows determined in for-loop      
data9 <- data9[new_type != "drop"]
data9[, brt12 := NULL]
data9 <- rename(data9, brt12 = new_type)

data9 <- data9[species != ""]
data9$brt12 |> unique()
# temporal aggregation --------------------------------------------------------------
data10 <- data9[month(date) %in% 5:9]
source("02_R/900_functions/newest_sample.R")
data10 <- newest_sample(data10, season_available = FALSE) 
saveRDS(data10, "01_data/003_macrophytes/001_original_data/poland_monitoring/final_aggregated.rds")
