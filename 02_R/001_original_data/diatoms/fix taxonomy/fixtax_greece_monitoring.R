## fix tax diatoms monitoring Portugal 

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


taxontable_add <- taxontable_add[-1, ]
#quicksave
saveRDS(taxontable_add, "data/diatoms/original_data/greece_monitoring/220304_quicksave_taxontable_add.rds")
taxontable_add <- readRDS("data/diatoms/original_data/greece_monitoring/220304_quicksave_taxontable_add.rds")

TU <- TU[which(!TU %in% taxontable_add$original_name)]

## check against fwb table from Kahlert et al 2020 
for (i in seq_along(TU)) {
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
                print(paste("name:", i.tu))
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
                if (i.final == "") i.final <- dia1$taxon_new[i.id]
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
                        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
                        print(paste(i.genus, "is a new genus"))
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

for (i in seq_along(TU)) {
        i.tu  <- TU[i]
        i.det <- stringr::str_detect(dia2$taxon, i.tu)
        if (any(i.det)) {
                i.id <- which(i.det)
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
        } else {
                i.id <- stringdist::amatch(i.tu, dia2$taxon, maxDist = 100000) 
                if (is.null(i.id))
                        next()
                print(paste("name:", i.tu))
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
                        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
                        print(paste(i.genus, "is a new genus"))
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

taxontable_add <- new_entry(ori = "Cymbella affinis var. angusta", fix = "Cymbella affinis", tt = taxontable, tt_a = taxontable_add)
taxontable_add <- new_entry(ori = "Cymbella affinis var. subcapitata", fix = "Cymbella affinis", tt = taxontable_add)
taxontable_add <- new_entry(ori = "Mastogloia albertii A.Pavlov, E.Jovanovska, C.E.Wetzel, L.Ector & Z.Levkov", fix = "Mastogloia albertii", tt = taxontable_add)
taxontable_add <- new_entry(ori = "Psammothidium sacculus", fix = "Psammothidium sacculus", tt = taxontable, tt_a = taxontable_add)
taxontable <- new_entry(ori = "Brachysira brebissonii Ross in Hartley", fix = "Brachysira zellensis", tt = taxontable, tt_a = taxontable)


TU <- setdiff(TU, taxontable_add$original_name)


## now check the new taxon table visually for inconsistencies and fix them 
View(taxontable_add)

## drop duplicates 
if (any(duplicated(taxontable_add$original_name))) 
        taxontable_add <- taxontable_add[- which(duplicated(taxontable_add$original_name)), ]
## any cross over between taxontable and taxontable_add? 
any(taxontable_add$original_name %in% taxontable$original_name)

## bind taxontables 
taxontable <- rbindlist(list(taxontable, taxontable_add))

any(duplicated(taxontable$original_name))

taxontable <- taxontable[-duplicated(taxontable$original_name), ]

taxontable <- new_entry(ori = "Brachysira brebissonii Ross in Hartley", fix = "Brachysira zellensis", tt = taxontable, tt_a = taxontable)

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))

