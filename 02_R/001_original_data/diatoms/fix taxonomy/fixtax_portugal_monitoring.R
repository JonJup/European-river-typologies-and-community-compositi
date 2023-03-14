## fix tax diatoms monitoring Portugal 

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


taxontable_add
#quicksave 
saveRDS(taxontable_add, "data/diatoms/original_data/poland_monitoring/220216_quicksave_taxontable_add.rds")
taxontable_add <- readRDS("data/diatoms/original_data/poland_monitoring/220216_quicksave_taxontable_add.rds")

TU <- setdiff(TU, taxontable_add$original_name)
TU <- TU[-which(TU == "EPSG")]

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

taxontable_add <- new_entry(ori = "Caloneis fontinalis + lancettula", fix = "Caloneis fontinalis", tt = taxontable_add)
taxontable_add <- new_entry(ori = "Campylodiscus noricus", fix = "Campylodiscus noricus", tt = taxontable)
taxontable_add <- new_entry(ori = "Cymatopleura solea (wraz z odmianami)"      , fix = "Cymatopleura solea", tt = taxontable)   
taxontable_add <- new_entry(ori = "Cymbella tumidula var. tumidula"            , fix = "Cymbella tumidula", tt = taxontable)         
taxontable_add <- new_entry(ori = "Diatoma polonica"                           , fix = "Diatoma polonica", tt = taxontable)                      
taxontable_add <- new_entry(ori = "Didymosphenia tatrensis"                    , fix = "Didymosphenia tatrensis", tt = taxontable)                
taxontable_add <- new_entry(ori = "Fragilaria leptostauron (wraz z odmianami)" , fix = "Staurosirella leptostauron complex", tt = taxontable)
taxontable_add <- new_entry(ori = "Hantzschia amphioxys + abundans"            , fix = "Hantzschia amphioxys", tt = taxontable)       
taxontable_add <- new_entry(ori = "Navicula soehrensis var. hassiaca"          , fix = "Chamaepinnularia hassiaca", tt = taxontable)        
taxontable_add <- new_entry(ori = "Nitzschia levidensis (wraz z odmianami)"    , fix = "Tryblionella", spe = NA, gen = "Tryblionella", tt = taxontable) 
taxontable_add <- new_entry(ori = "Achnanthidium caravelense Novais et Ector f. anormale", fix = "Achnanthidium caravelense", tt = taxontable)                                    
taxontable_add <- new_entry(ori = "Achnanthidium duriense Novais & Ector", fix = "Achnanthidium duriense", tt = taxontable)                                                 
taxontable_add <- new_entry(ori = "Achnanthidium nanum (F.Meister) Novais & Jüttner", fix = "Achnanthidium nanum" , tt = taxontable)                                      
taxontable_add <- new_entry(ori = "Achnanthidium nanum (F.Meister) Novais & Jüttner f. anormale", fix = "Achnanthidium nanum"  , tt = taxontable)                        
taxontable_add <- new_entry(ori = "Achnanthidium pseudolineare Van de Vijver, Novais & Ector", fix = "Achnanthidium pseudolineare" , tt = taxontable)                               
taxontable_add <- new_entry(ori = "Actinocyclus normanii (W.Gregory ex Greville) Hustedt", fix = "Actinocyclus normanii" , tt = taxontable)                                  
taxontable_add <- new_entry(ori = "Adlafia neoniana Cantonati", fix = "Adlafia neoniana"  , tt = taxontable)                                                             
taxontable_add <- new_entry(ori = "Caloneis amphisbaena (Bory) Cleve", fix = "Caloneis amphisbaena"  , tt = taxontable)                                                     
taxontable_add <- new_entry(ori = "Diatoma moliniformis Kützing", fix = "Diatoma moniliformis" , tt = taxontable)                                                            
taxontable_add <- new_entry(ori = "Eunotia minor (Kützing) Grunow", fix = "Eunotia pectinalis Complex" , tt = taxontable)                                                         
taxontable_add <- new_entry(ori = "Eunotia pectinalis (Kützing) Rabenhorst", fix = "Eunotia pectinalis Complex"  , tt = taxontable)                                                
taxontable_add <- new_entry(ori = "Fallacia pygmaea (Kützing) A.J.Stickle & D.G.Mann", fix = "Fallacia pygmaea-forcipata" , tt = taxontable)                                      
taxontable_add <- new_entry(ori = "Fragilaria aff. amphicephaloides Lange-Bertalot", fix = "Fragilaria capucina complex" , tt = taxontable)                                         
taxontable_add <- new_entry(ori = "Fragilaria amphicephaloides Lange-Bertalot", fix = "Fragilaria capucina complex"  , tt = taxontable)                                            
taxontable_add <- new_entry(ori = "Fragilaria cf. parva Tuiji & D.M.Williams", fix = "Fragilaria parva" , tt = taxontable)                                               
taxontable_add <- new_entry(ori = "Fragilaria famelica (Kützing) Lange-Bertalot", fix = "Fragilaria capucina complex"        , tt = taxontable)                                    
taxontable_add <- new_entry(ori = "Fragilaria misarelensis Almeida, C.Delgado, Novais & S.Blanco", fix = "Fragilaria misarelensis", tt = taxontable)                            
taxontable_add <- new_entry(ori = "Fragilaria pararumpens Lange-Bertalot, Hofmann & Werum in Hofmann & al.", fix = "Fragilaria capucina complex", tt = taxontable)              
taxontable_add <- new_entry(ori = "Fragilaria parva (Grunow in Van Heurck) Tuiji & Williams f. anormale", "Fragilaria parva", tt = taxontable)                     
taxontable_add <- new_entry(ori = "Fragilaria parva Tuiji & D.M.Williams", fix = "Fragilaria parva", tt = taxontable)                                                   
taxontable_add <- new_entry(ori = "Fragilaria candidagilae Almeida, C.Delgado, Novais & S.Blanco", "Fragilaria candidagilae", tt = taxontable)                            
taxontable_add <- new_entry(ori = "Fragilaria rinoi Almeida & C.Delgado", fix = "Fragilaria rinoi", tt = taxontable)                                                    
taxontable_add <- new_entry(ori = "Gomphonema elegantissimum E.Reichardt & Lange-Bertalot", fix = "Gomphonema elegantissimum", tt = taxontable)                                   
taxontable_add <- new_entry(ori = "Gomphonema sp. (GOMS4 Bey & Ector)", fix = "Gomphonema", tt = taxontable)                                                      
taxontable_add <- new_entry(ori = "Gomphonema truncatum Ehrenberg", fix =  "Gomphonema constrictum complex", tt = taxontable)                                                          
taxontable_add <- new_entry(ori = "Gomphonema bourbonense E.Reichardt", fix = "Gomphonema bourbonense", tt = taxontable)                                                      
taxontable_add <- new_entry(ori = "Karayevia ploenensis var. gessneri (Hustedt) Bukhtiyarova", fix = "Karayevia ploenensis", tt = taxontable)                                
taxontable_add <- new_entry(ori = "Navicula cincta (Ehrenberg) Ralfs", "Navicula cincta-heufleri", tt = taxontable)                                                       
taxontable_add <- new_entry(ori = "Navicula viridulacalcis Lange-Bertalot", fix = "Navicula viridula complex" , tt = taxontable)                                                
taxontable_add <- new_entry(ori = "Nitzschia aff. fonticola Grunow", fix = "Nitzschia fonticola Complex", tt = taxontable)                                                         
taxontable_add <- new_entry(ori = "Nitzschia aff. pusilla Grunow", fix = "Nitzschia pusilla Complex", tt = taxontable)                                                            
taxontable_add <- new_entry(ori = "Nitzschia dissipata (Kützing) Grunow", fix = "Nitzschia dissipata-recta Complex", tt = taxontable)                                                    
taxontable_add <- new_entry(ori = "Nitzschia frustulum (Kützing) Grunow", fix = "Nitzschia frustulum Complex", tt = taxontable)                                                     
taxontable_add <- new_entry(ori = "Nitzschia frustulum (Kützing)Grunow var inconspicua (Grunow) Grunow", fix = "Nitzschia frustulum Complex", tt = taxontable)                         
taxontable_add <- new_entry(ori = "Nitzschia linearis (C.Agardh) W.Smith", fix = "Nitzschia pura-linearis Complex", tt = taxontable)                                                    
taxontable_add <- new_entry(ori = "Nitzschia pusilla Grunow", fix = "Nitzschia pusilla Complex", tt = taxontable)
taxontable_add <- new_entry(ori = "Pinnularia microstauron (Ehrenberg) Cleve", fix = "Pinnularia microstauron Complex", tt = taxontable)                                                
taxontable_add <- new_entry(ori = "Pinnularia microstauron (Ehrenberg) Cleve", fix = "Pinnularia microstauron Complex", tt = taxontable)                                                

taxontable_add <- new_entry(ori = "Pseudostaurosira aff. alvareziae Cejudo-Figueras, E.Morales & Ector", fix = "Pseudostaurosira alvareziae", tt = taxontable)                     
taxontable_add <- new_entry(ori = "Pseudostaurosira alvareziae Cejudo-Figueras, E.Morales & Ector", fix = "Pseudostaurosira alvareziae", tt = taxontable)                           
taxontable_add <- new_entry(ori = "PSEUDOSTAUROSIROPSIS E.A. Morales", fix = "Pseudostaurosiropsis", tt = taxontable)                                                       
taxontable_add <- new_entry(ori = "Rhopalodia gibba (Ehrenberg) O. Muller", fix = "Rhopalodia gibba Complex", tt = taxontable)                                                   
taxontable_add <- new_entry(ori = "Sellaphora schadei (Krasske) C.E.Wetzel, L.Ector, B.Van de Vijver, Compère & D.G.Mann", fix = "Sellaphora schadei", tt = taxontable)   
taxontable_add <- new_entry(ori = "Stauroforma atomus (Hust.) Talgatti, C.E.Wetzel, E.Morales & Torgan", fix = "Martyana atomus", tt = taxontable)                      
taxontable_add <- new_entry(ori = "Staurosira binodis Lange-Bertalot", fix = "Staurosira binodis-robusta", tt = taxontable)                                                       
taxontable_add <- new_entry(ori = "Tryblionella tryblio Cantonati & Lange-Bertalot in Cantonati, M.G.Kelly & Lange-Bertalot", fix = "Tryblionella hantzschiana", tt = taxontable) 
taxontable_add <- new_entry(ori = "Ulnaria sp.2 (Hlúbiková et Ector)", fix = "Ulnaria", tt = taxontable) 

"Pinnularia microstauron (Ehrenberg) Cleve" %in% taxontable_add$original_name


taxontable[original_name %in% c("Fragilaria famelica", "Synedra famelica"), c("fixed_name", "species", "genus") := .("Fragilaria capucina complex", "Fragilaria capucina complex", "Fragilaria")]
taxontable[original_name %in% c("Gomphonema truncatum var. capitatum", "Gomphonema truncatum + pala + italicum + capitatum"), c("fixed_name", "species") := .("Gomphonema constrictum complex", "Gomphonema constrictum complex")]
taxontable[original_name %in% c("Navicula viridulacalcis"), c("fixed_name", "species") := .("Navicula viridula complex", "Navicula viridula complex")]
taxontable[original_name %in% c("Pinnularia microstauron (Ehr.) Cleve f.diminuta Matvienko"), c("fixed_name", "species") := .("Pinnularia microstauron Complex")]


TU <- setdiff(TU, taxontable_add$original_name)
TU <- TU[-which(TU %in% taxontable_add$original_name)]

## remove the first element from the taxontable_add 
taxontable_add <- taxontable_add[-1, ]

## now check the new taxon table visually for inconsistencies and fix them 
View(taxontable_add)

taxontable_add[species == "Gomphonema ", species := NA]
taxontable_add[species == "Navicula complex ", species := NA]
taxontable_add[species == "Thalassiosira Complex ", species := NA]

## drop duplicates 
if (any(duplicated(taxontable_add$original_name))) 
        taxontable_add <- taxontable_add[- which(duplicated(taxontable_add$original_name)), ]
## any cross over between taxontable and taxontable_add? 
any(taxontable_add$original_name %in% taxontable$original_name)

taxontable_add$original_name[which(taxontable_add$original_name %in% taxontable$original_name)]

## bind taxontables 
taxontable <- rbindlist(list(taxontable, taxontable_add))

taxontable <- taxontable[-duplicated(taxontable$original_name), ]


saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))

