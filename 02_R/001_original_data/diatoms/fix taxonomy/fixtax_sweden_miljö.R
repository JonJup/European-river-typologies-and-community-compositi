## -- fix tax Croatia
source("R/functions/harmonize diatoms.R")

TU <- unique(data2$taxon) |> sort()
TU <- TU[which(!TU %in% taxontable$original_name)]

strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 100000)
strdist_tbl <- 
        data.table(taxontable_name = taxontable$original_name[strdist_id], 
                   data_set_name = TU) |> 
        filter(!is.na(taxontable_name)) 

# create taxontable for taxa that are added in this data set. 
taxontable_add <- copy(taxontable)
taxontable_add <- taxontable_add[1,]

# try to find similar entries in the existing data.table 
for (i in 1:nrow(strdist_tbl)){
        
        tt_name <- pull(strdist_tbl[i,1])
        tu_name <- pull(strdist_tbl[i,2])
        
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
taxontable_add <- taxontable_add[-1, ]
TU <- TU[which(!TU %in% taxontable_add$original_name)]
## check against fwb table from Kahlert et al 2020 
for (i in seq_along(TU)) {
        i.tu  <- TU[i]
        i.det <- str_detect(dia1$taxon_old, i.tu)
        if (any(i.det)) {
                i.id <- which(i.det)
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia1$taxon_old[i.id]))
                i.rl <- readline()
        } else {
                i.id <- amatch(i.tu, dia1$taxon_old, maxDist = 100000) 
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
                i.final <- str_trim(i.final)
                i.species <- ifelse(str_detect(i.final, "\\ "), i.final, NA)
                i.genus   <- word(i.final, 1)
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

taxontable_add[fixed_name == "Eunotia Complex", c("fixed_name", "species") := .("Eunotia", NA)]
taxontable_add[fixed_name == "Navicula complex", c("fixed_name", "species") := .("Navicula", NA)]
taxontable_add[fixed_name == "Orthoseira Complex", c("fixed_name", "species") := .("Orthoseira", NA)]

taxontable <- rbindlist(list(taxontable, taxontable_add), fill = TRUE)

taxontable <- new_entry(ori = "Aulacoseira pseudoislandica", fix = "Aulacoseira pseudoislandica")
taxontable <- new_genus(ori = "Belonastrum berolinense", fix = "Belonastrum berolinense", spe = "Belonastrum berolinense", gen = "Belonastrum", fam = "Bacillariophyta familia incertae sedis", ord = "Bacillariophyta ordo incertae sedis", cla = "Bacillariophyta classis incertae sedis", phy = "Bacillariophyta", kin = "Chromista")                      
taxontable <- new_entry("Caloneis minuscula", fix = "Caloneis minuscula")
taxontable <- append_to_tt("Cymbella proxima", "Cymbella proxima var. bottnica")
taxontable <- append_to_tt("Cymbella subaspera","Cymbella subaspera var. subaspera")
taxontable <- append_to_tt("Encyonopsis cesatii", "Encyonopsis cesatii var. geitleri")
taxontable <- new_entry("Eunotia bertrandii", fix = "Eunotia bertrandii")
taxontable <- new_entry("Eunotia minutula", fix = "Eunotia minutula")
taxontable <- new_entry("Fragilaria sp. Iconogr. 2, Taf. 8:15-16", fix = "Fragilaria", spe = NA)     
taxontable <- new_entry("Gomphonella", fix = "Gomphonella", spe = NA)                                
taxontable <- append_to_tt("Gomphoneis calcifuga", "Gomphonella calcifuga")
taxontable <- append_to_tt("Gomphonema linearoides", "Gomphonella linearoides")
taxontable <- append_to_tt("Gomphoneis olivaceoides", "Gomphonella olivaceoides")
taxontable <- new_entry("Navicula antonioides", fix = "Navicula antonioides")
taxontable <- new_entry("Navicula sp. SWF 2/4 Taf. 30:32-35", fix = "Navicula", spe = NA)
taxontable <- append_to_tt("Navicula elaphros", "Naviculadicta elaphros")
taxontable <- new_entry("Naviculadicta Iconogr. 2, Taf. 104:33-36", fix = "Naviculadicta", spe = NA)
taxontable <- new_entry("Naviculadicta Iconogr. 2, Taf. 27:17-18", fix = "Naviculadicta", spe = NA)
taxontable <- new_entry("Naviculadicta Iconogr. 2, Taf. 28:21-23", fix = "Naviculadicta", spe = NA)
taxontable <- new_entry("Naviculadicta Iconogr. 2, Taf. 28:6-9", fix = "Naviculadicta", spe = NA)
taxontable <- new_entry("Nitzschia sp. Iconogr. 2. Taf. 70:21a-b", fix = "Naviculadicta", spe = NA)
taxontable <- new_entry("Pantocsekiella rossii", fix = "Pantocsekiella rossii")                      
taxontable <- new_entry("Pantocsekiella schumannii", fix = "Pantocsekiella schumannii")
taxontable <- new_entry("Pantocsekiella tripartita", fix = "Pantocsekiella tripartita")
taxontable <- append_to_tt("Pinnularia biceps", "Pinnularia biceps var. gibberula")
taxontable <- append_to_tt("Pinnularia divergentissima", "Pinnularia divergentissima var. triundulata")
taxontable <- append_to_tt("Pinnularia interrupta","Pinnularia interrupta var. minutissima")    
taxontable <- append_to_tt("Psammothidium sacculum","Psammothidium saccula")
taxontable <- new_entry("Sellaphora medioconvexa", fix = "Sellaphora medioconvexa")
taxontable <- new_entry("Sellaphora multiconfusa", fix = "Sellaphora multiconfusa")
taxontable <- append_to_tt("Sellaphora schadei (Krasske) C.E.Wetzel, L.Ector, B.Van de Vijver, Compère & D.G.Mann", "Sellaphora schadei")
taxontable <- append_to_tt("Navicula stroemii","Sellaphora submuralis")                      
taxontable <- new_entry("Tryblionella plana var. fennica", fix = "Tryblionella plana")

TU <- TU[which(!TU %in% taxontable$original_name)]
taxontable <- new_genus(ori = "Bacillariophyta", fix = "Bacillariophyta", gen = NA, fam = NA, ord =NA, cla = NA, phy = "Bacillariophyta", kin = "Chromista")
taxontable <- new_genus(ori = "Coscinodiscophyceae", fix = "Coscinodiscophyceae", gen = NA, fam = NA, ord =NA, cla = "Coscinodiscophyceae", phy = "Bacillariophyta", kin = "Chromista")

if (any(duplicated(taxontable$original_name))){
        
        dup_id <- which(duplicated(taxontable$original_name))
        taxontable <- taxontable[- dup_id]
        rm(dup_id)
        
}

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))

rm(taxontable_add, strdist_id, strdist_tbl, i, tt_name, tu_name, TU)
