## fix tax diatoms monitoring poland 

taxontable <- readRDS("data/diatoms/2022-02-15_taxontable_diatoms.rds")

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)


strdist_id  <- stringdist::amatch(TU, taxontable$original_name, maxDist = 100000)
strdist_tbl <- 
        data.table(taxontable_name = taxontable$original_name[strdist_id], 
                   data_set_name = TU) |> 
        d$filter(!is.na(taxontable_name)) 

# create taxontable for taxa that are added in this data set. 
taxontable_add <- copy(taxontable)
taxontable_add <- taxontable_add[1,]

# try to find similar entries in the exisiting data.table 
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


taxontable_add
#quicksave 
saveRDS(taxontable_add, "data/diatoms/original_data/poland_monitoring/220216_quicksave_taxontable_add.rds")
taxontable_add <- readRDS("data/diatoms/original_data/poland_monitoring/220216_quicksave_taxontable_add.rds")

TU <- setdiff(TU, taxontable_add$original_name)


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
                if (is.null(i.id))
                        next()
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia1$taxon_old[i.id]))
                i.rl <- readline()
        }        
        if (i.rl != "break") {
                i.id <- i.id[as.numeric(i.rl)]
                print(paste("Final name: ", dia1$taxon_new[i.id]))
                i.final <- readline()
                ## check that against fwb
                if (i.final == "") i.final <- dia1$taxon_new[i.id]
                taxontable_add <- dfm$append_to_tt(fixed_name = i.final, original_name = i.tu, data_from = taxontable, data_to = taxontable_add)
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

TU <- setdiff(TU, taxontable_add$original_name)

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
        if (i.rl != "break") {
                i.id <- i.id[as.numeric(i.rl)]
                ## is it a synonym?
                if (!is.na(dia2$new[i.id])) {
                        print(paste("new code:",
                                    dia2$new[i.id]))
                        ## enter new code
                        i.rl2 <- readline()
                        i.id <- which(dia2$code == i.rl2)
                }
                print(paste("Final name: ", dia2$taxon[i.id]))
                i.final <- readline()
                if (i.final == "")
                        i.final <- dia2$taxon[i.id]
                taxontable_add <- dfm$append_to_tt(fixed_name = i.final, original_name = i.tu, data_from = taxontable, data_to = taxontable_add)
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

TU <- setdiff(TU, taxontable_add$original_name)

taxontable_add <- new_entry(ori = "Caloneis fontinalis + lancettula", fix = "Caloneis fontinalis")
taxontable_add <- new_entry(ori = "Campylodiscus noricus", fix = "Campylodiscus noricus")
taxontable_add <- new_entry(ori = "Cymatopleura solea (wraz z odmianami)"      , fix = "Cymatopleura solea")   
taxontable_add <- new_entry(ori = "Cymbella tumidula var. tumidula"            , fix = "Cymbella tumidula")         
taxontable_add <- new_entry(ori = "Diatoma polonica"                           , fix = "Diatoma polonica")                      
taxontable_add <- new_entry(ori = "Didymosphenia tatrensis"                    , fix = "Didymosphenia tatrensis")                
taxontable_add <- new_entry(ori = "Fragilaria leptostauron (wraz z odmianami)" , fix = "Staurosirella leptostauron complex")
taxontable_add <- new_entry(ori = "Hantzschia amphioxys + abundans"            , fix = "Hantzschia amphioxys")       
taxontable_add <- new_entry(ori = "Navicula soehrensis var. hassiaca"          , fix = "Chamaepinnularia hassiaca")        
taxontable_add <- new_entry(ori = "Nitzschia levidensis (wraz z odmianami)"    , fix = "Tryblionella", spe = NA, gen = "Tryblionella") 

TU <- setdiff(TU, taxontable_add$original_name)

## remove the first element from the taxontable_add 
taxontable_add <- taxontable_add[-1, ]

## now check the new taxon table visually for inconsistencies and fix them 
View(taxontable_add)

taxontable_add[species == "Craticula ", species := NA]

## drop duplicates 
if (any(duplicated(taxontable_add$original_name))) 
        taxontable_add <- taxontable_add[- which(duplicated(taxontable_add$original_name)), ]
## any cross over between taxontable and taxontable_add? 
any(taxontable_add$original_name %in% taxontable$original_name)

## bind taxontables 
taxontable <- rbindlist(list(taxontable, taxontable_add))

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))

