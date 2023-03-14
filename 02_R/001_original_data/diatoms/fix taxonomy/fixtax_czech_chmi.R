## -- fix tax CHMI CZECH 

source("R/functions/harmonize diatoms.R")
library(stringr)

TU <- unique(data$taxon) |> sort()
TU <- TU[which(!TU %in% taxontable$original_name)]

strdist_id  <- stringdist::amatch(TU, taxontable$original_name, maxDist = 100000)
strdist_tbl <- 
        data.table(taxontable_name = taxontable$original_name[strdist_id], 
                   data_set_name = TU) |> 
        d$filter(!is.na(taxontable_name)) 

# create taxontable for taxa that are added in this data set. 
taxontable_add <- copy(taxontable)
taxontable_add <- taxontable_add[1,]

# try to find similar entries in the existing data.table 
for (i in 1:nrow(strdist_tbl)){
        
        tt_name <- d$pull(strdist_tbl[i,1])
        tu_name <- d$pull(strdist_tbl[i,2])
        
        print(paste("data:", tu_name))
        print(paste("taxt:", tt_name))
        
        i.bool <- readline("match?:")
        
        if (i.bool == "y"){
                taxontable_add <- dfm$append_to_tt(fixed_name = tt_name, original_name = tu_name, data_from = taxontable, data_to = taxontable_add)
        } else if (i.bool == "n") {
                next()
        }
        rm(list = ls()[grepl("^i", ls())])
}

drop_id <- which(TU %in% c("Bacillariophyceae", "Bacillariophyceae centricae", "Bacillariophyceae pennatae"))
TU <- TU[-drop_id]

taxontable_add <- taxontable_add[!original_name %in% c("Cymbopleura apiculata",
                                    "Navicula mitigata",
                                    "Staurosira leptostauron v.leptostauron")]

taxontable_add <- taxontable_add[-1, ]
TU <- TU[which(!TU %in% taxontable_add$original_name)]
## check against fwb table from Kahlert et al 2020 
for (i in seq_along(TU)) {
        #if (i<8)
                #next()
        i.tu  <- TU[i]
        i.det <- stringr::str_detect(dia1$taxon_old, i.tu)
        if (any(i.det)) {
                i.id <- which(i.det)
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia1$taxon_old[i.id]))
                i.rl <- readline()
        } else {
                i.id <- stringdist::amatch(i.tu, dia1$taxon_old, maxDist = 100000) 
                if (is.null(i.id)){
                        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
                        next()
                }
                print(paste("-------name:", i.tu))
                print(paste("suggestions:", dia1$taxon_old[i.id]))
                i.rl <- readline()
        }       
        if (i.rl == "break"){
                break()
        } else if (i.rl == "n"){
                rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
                next()
        } else {
                i.id <- i.id[as.numeric(i.rl)]
                print(paste("Final name: ", dia1$taxon_new[i.id]))
                i.final <- readline()
                ## check that against fwb
                if (i.final == ""){
                        i.final <- dia1$taxon_new[i.id]      
                } 
                i.final <- stringr::str_trim(i.final)
                i.species <- ifelse(stringr::str_detect(i.final, "\\ "), i.final, NA)
                i.genus   <- stringr::word(i.final, 1)
                if (i.genus %in% unique(taxontable$genus)){
                        i.family <- taxontable[genus  == i.genus  , unique(family)] 
                        i.order  <- taxontable[family == i.family , unique(order)] 
                        i.class  <- taxontable[order  == i.order  , unique(class)] 
                        i.phylum <- taxontable[class  == i.class  , unique(phylum)] 
                        i.kingdom <- taxontable[phylum == i.phylum , unique(kingdom)] 
                        if (length(i.family) != 1){
                                print("mutliple families")
                                break()
                        }
                        if (length(i.order) != 1){
                                print("mutliple orders")
                                break()
                        }
                        if (length(i.class) != 1){
                                print("mutliple classes")
                                break()
                        }
                        if (length(i.phylum) != 1){
                                print("mutliple phyla")
                                break()
                        }
                        if (length(i.kingdom) != 1){
                                print("mutliple kingdoms")
                                break()
                        }
                } else {
                        print(paste(i.genus, "////////\\\\\\is a new genus////////\\\\\\"))
                        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
                        next()
                }
                
                i.add <- data.table(original_name = i.tu,
                                    fixed_name = i.final,
                                    species = i.species,
                                    genus = i.genus,
                                    family = i.family,
                                    order = i.order,
                                    class = i.class,
                                    phylum = i.phylum,
                                    kingdom = i.kingdom)
                taxontable_add <- rbindlist(list(taxontable_add, i.add))
        }
        #rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}
TU <- TU[which(!TU %in% taxontable_add$original_name)]

for (i in seq_along(TU)) {
        
        #if(i<300) next()
        
        i.tu  <- TU[i]
        i.det <- stringr::str_detect(dia2$taxon, i.tu)
        if (any(i.det)) {
                i.id <- which(i.det)
                print(paste("-------name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
        } else {
                i.id <- stringdist::amatch(i.tu, dia2$taxon, maxDist = 100000) 
                if (is.null(i.id))
                        next()
                print(paste("-------name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
        } 
        if (i.rl == "break"){
                break()
        }
        if (i.rl != "n") {
                i.id <- i.id[as.numeric(i.rl)]
                ## is it a synonym?
                if (!is.na(dia2$new[i.id])) {
                        print(paste("new code:",
                                    dia2$new[i.id]))
                        ## enter new code
                        i.rl2 <- readline()
                        if (i.rl2 != ""){
                                i.id <- which(dia2$code == i.rl2)
                        }
                        
                }
                print(paste("Final name: ", dia2$taxon[i.id]))
                i.final <- readline()
                if (i.final == "") {
                        i.final <- dia2$taxon[i.id]
                }
                ## check against fwb
                if (i.final %in% dia1$taxon_old){
                        
                } else {
                        i.fwbid <- stringdist::amatch(i.final, dia1$taxon_old, maxDist = 100000) 
                        print(paste("closest fwb entry:", dia1$taxon_old[i.fwbid]))
                        i.rl3 <- readline()
                        if (i.rl3 == "y"){
                                i.final <- dia1$taxon_new[i.fwbid]
                        }
                }
                
                
                i.final <- stringr::str_trim(i.final)
                i.species <- ifelse(stringr::str_detect(i.final, "\\ "), i.final, NA)
                i.genus   <- stringr::word(i.final, 1)
                if (i.genus %in% unique(taxontable$genus)){
                        i.family <- taxontable[genus  == i.genus  , unique(family)] 
                        i.order  <- taxontable[family == i.family , unique(order)] 
                        i.class  <- taxontable[order  == i.order  , unique(class)] 
                        i.phylum <- taxontable[class  == i.class  , unique(phylum)] 
                        i.kingdom <- taxontable[phylum == i.phylum , unique(kingdom)] 
                        if (length(i.family) != 1){
                                print("mutliple families")
                                break()
                        }
                        if (length(i.order) != 1){
                                print("mutliple orders")
                                break()
                        }
                        if (length(i.class) != 1){
                                print("mutliple classes")
                                break()
                        }
                        if (length(i.phylum) != 1){
                                print("mutliple phyla")
                                break()
                        }
                        if (length(i.kingdom) != 1){
                                print("mutliple kingdoms")
                                break()
                        }
                } else {
                        print(paste(i.genus, "is a new genus"))
                        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
                        next()
                }
                
                i.add <- data.table(original_name = i.tu,
                                    fixed_name = i.final,
                                    species = i.species,
                                    genus = i.genus,
                                    family = i.family,
                                    order = i.order,
                                    class = i.class,
                                    phylum = i.phylum,
                                    kingdom = i.kingdom)
                taxontable_add <- rbindlist(list(taxontable_add, i.add))
                
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}
TU <- TU[which(!TU %in% taxontable_add$original_name)]
TU

taxontable[original_name %in% c("Brachysira cf neoexilis Lange-Bertalot", 
                                "Brachysira neoexilis Lange-Bertalot"), 
           c("fixed_name", "species") := .("Brachysira neoexilis")]
taxontable[original_name == "Cymatopleura elliptica (Brebisson)W.Smith var.hibernica (W.Sm.) Van Heurc", 
           c("fixed_name", "species") := .("Surirella hibernica")]
View(taxontable_add)
taxontable <- rbindlist(list(taxontable, taxontable_add))
taxontable <- append_to_tt("Brachysira neoexilis", "Anomoeoneis neoexilis")
taxontable <- append_to_tt("Iconella hibernica", "Campylodiscus noricus v.hibernicus")
taxontable <- new_entry(ori = "Cyclotella quadriiuncta", fix = "Puncticulata comta")
taxontable <- new_entry(ori = "Cymatopleura elliptica v.nobilis", fix = "Surirella hibernica")
taxontable <- new_entry(ori = "Diatoma elongata"          , fix = "Diatoma elongata")
taxontable <- new_entry(ori = "Diatoma vulgaris mt.brevis", fix = "Diatoma ehrenbergii")
taxontable <- new_entry(ori = "Diatoma vulgaris v.grandis", fix = "Diatoma vulgaris")
taxontable <- new_entry(ori = "Eunotia monodon v.bidens"  , fix = "Eunotia monodon")
taxontable <- new_entry(ori = "Fragilaria angustissima"  , fix = "Fragilaria tenera complex")
taxontable <- new_entry(ori = "Frustulia pangeopsis"  , fix = "Frustulia pangaeopsis")
taxontable <- new_genus(ori = "Gomphocymbella sp.", fix = "Gomphocymbella", spe = NA, gen = "Gomphocymbella", fam = "Cymbellaceae", ord = "Cymbellales", cla = "Bacillariophyceae")
taxontable <- new_entry(ori = "Gomphonema exilis"  , fix = "Gomphonema parvulum Complex")
taxontable <- append_to_tt("Halamphora", "Halamphora sp.")
taxontable <- append_to_tt("Navicula", "Navicula axilis")
taxontable <- append_to_tt("Navicula", "Navicula axillis")
taxontable <- append_to_tt("Navicula phyllepta", "Navicula hyllepta")
taxontable <- append_to_tt("Navicula rhynchocephala", "Navicula rhynchocephala v.elongata")
taxontable <- new_entry("Navicula sociabilis", "Pinnularia socialis")
taxontable <- new_entry("Nitzschia kuetzingii", "Nitzschia kuetzingii")
taxontable <- append_to_tt("Orthoseira roeseana", "Orthoseira roeseana v.dendroteres")
taxontable <- append_to_tt("Pinnunavis", "Pinnunavis sp.")
taxontable <- new_entry("Stauroneis hyalina", "Stauroneis hyalina")
taxontable <- new_entry("Staurosira construens v.triundulata", "Staurosira construens complex")
taxontable <- new_genus(ori = "Surirella gemma", fix = "Petrodictyon gemma", spe = "Petrodictyon gemma", 
                        gen = "Petrodictyon", fam = "Surirellaceae", ord = "Surirellales", cla = "Bacillariophyceae")
taxontable <- new_entry(ori = "Surirella robusta v.splendida", fix = "Surirella splendida-tenera")
taxontable <- new_entry("Ulnaria ulna v.claviceps", fix = "Ulnaria ulna complex")
taxontable <- new_entry("Surirella terrestris", "Surirella")
TU <- unique(data$taxon) |> sort()
TU <- TU[which(!TU %in% taxontable$original_name)]

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))

