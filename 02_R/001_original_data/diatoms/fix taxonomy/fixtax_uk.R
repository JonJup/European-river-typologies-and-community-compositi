## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name))

for (i in 1:nrow(strdist_tbl)){
        
        i.1 <- pull(strdist_tbl[i,1])
        i.2 <- pull(strdist_tbl[i,2])
        
        print(i.1)
        print(i.2)
        
        i.bool <- readline("match?:")
        
        if (i.bool == "y"){
                taxontable <- append_to_tt(i.1,i.2)
        } else if (i.bool == "n") {
                next()
        }
        rm(list = ls()[grepl("^i", ls())])
}

taxontable <- append_to_tt("Achnanthes", "Achnanthes (Other)")
taxontable <- append_to_tt("Achnanthes lanceolata var.rostrata" ,"Achnanthes lanceolata var. rostrata")
taxontable <- append_to_tt("Achnanthes lauenburgiana", "Achnanthes lauenbergiana")
taxontable <- append_to_tt("Achnanthidium minutissimum Gruppe", "Achnanthidium minutissimum type")
taxontable <- append_to_tt("Amphora pediculus", "Amphora pediculus type")
taxontable <- append_to_tt("Brachysira vitrea", "Brachysira vitrea type")
taxontable <- append_to_tt("Cocconeis placentula", "Cocconeis placentula agg.")
taxontable <- append_to_tt("Cocconeis placentula var.klinoraphis", "Cocconeis placentula var. klinoraphis")
taxontable <- append_to_tt("Cyclotella distinguenda var.unipunctata", "Cyclotella distinguenda var. unipunctata")
taxontable <- append_to_tt("Diatoma moniliformis"," Diatoma moniliforme")
taxontable <- append_to_tt("Diatoma vulgaris", "Diatoma vulgare")
taxontable <- append_to_tt("Diatoma vulgaris", "Diatoma vulgare agg.")
taxontable <- append_to_tt("Diatoma vulgaris f.breve", "Diatoma vulgare var. breve")
taxontable <- append_to_tt("Encyonema minutum", "Encyonema - minutum-type")
taxontable <- append_to_tt("Encyonema minutum", "Encyonema minutum-type")
taxontable <- append_to_tt("Encyonema prostratum", "Encyonema prostratrum")
taxontable <- append_to_tt("Eunotia pectinalis var.minor", "Eunotia pectinalis var. minor")
taxontable <- append_to_tt("Fragilaria capucina 70", "Fragilaria capucina agg.")
taxontable <- append_to_tt("Fragilaria capucina var.capitellata", "Fragilaria capucina var. amphicephala")
taxontable <- append_to_tt("Fragilaria capucina var.capitellata", "Fragilaria capucina var. capitellata")
taxontable <- append_to_tt("Fragilaria capucina var.distans", "Fragilaria capucina var. distans")
taxontable <- append_to_tt("Fragilaria construens f. binodis", "Fragilaria construens var. binodis")
taxontable <- append_to_tt("Fragilariforma virescens var.exigua", "Fragilariforma virescens var. exigua")
taxontable <- append_to_tt("Frustulia crassinervia", "Frustulia crassinveria")
taxontable <- append_to_tt("Gomphonema angusticephalum", "Gomphonema angustum/pumilum type")
taxontable <- append_to_tt("Gomphonema olivaceum", "Gomphonema olivaceum agg.")
taxontable <- append_to_tt("Gomphonema olivaceum var. minutissimum", "Gomphonema olivaceum var. minutissima")
taxontable <- append_to_tt("Gomphonema parvulum var.exilis", "Gomphonema parvulum var. exilis")
taxontable <- append_to_tt("Gomphosphenia grovei var.lingulata", "Gomphosphenia grovei var. lingulata")
taxontable <- append_to_tt("Kobayasiella subtilissima", "Kobaysiella subtilissima")
taxontable <- append_to_tt("Navicula capitata var. lueneburgensis", "Navicula capitata var. luneburgensis")
taxontable <- append_to_tt("Navicula capitatoradiata", "Navicula capitoradiata")
taxontable <- append_to_tt("Navicula cryptotenella", "Navicula cryptotenella agg.")
taxontable <- append_to_tt("Navicula tripunctata var.schizomenoides", "Navicula tripunctata var. schizomenoides")
taxontable <- append_to_tt("Nitzschia sigmoidea", "Nitzschia - section sigmoidea")
taxontable <- append_to_tt("Nitzschia epithemoides var.disputata", "Nitzschia epithemoides var. disputata")
taxontable <- append_to_tt("Planothidium lanceolatum", "Planothidium - lanceolatum-type")
taxontable <- append_to_tt("Planothidium hauckianum", "Planothidium haukianum")
taxontable <- append_to_tt("Planothidium lanceolatum", "Planothidium lanceolatum-type")
taxontable <- append_to_tt("Psammothidium grishunum f.daonensis", "Psammothidium grischunum f. daonensis")
taxontable <- append_to_tt("Psammothidium grischunum", "Psammothidium grishunum")
taxontable <- append_to_tt("Psammothidium lauenburgianum", "Psammothidium lauenburgianaum")
taxontable <- append_to_tt("Reimeria sinuata var. sinuata", "Reimeria sinuata form antiqua")
taxontable <- append_to_tt("Staurosira construens var.binodis", "Staurosira construens var. binodis")
taxontable <- append_to_tt("Staurosira construens var.venter", "Staurosira construens var. venter")
taxontable <- append_to_tt("Synedra ulna var.danica", "Synedra ulna var. danica")
taxontable <- append_to_tt("Thalassiosira weissflogii", "Thalassiosira weissfloggii")

taxontable <- new_genus("Petroneis humerosa", "Petroneis humerosa", "Petroneis humerosa", "Petroneis", "Lyrellaceae", "Lyrellales", "Bacillariophyceae", "Bacillariophyta", "Chromista")

## check against fwb table 
for (i in seq_along(TU)){
        i.tu  <- TU[i]
        if(check_fwb(i.tu)){
                x <- get_fwb(i.tu)
                taxontable <- add_entry_tt(x)
        }
        rm(i.tu)
}; rm(i)

TU <- setdiff(TU, taxontable$original_name)

for (i in seq_along(TU)) {
        i.tu  <- TU[i]
        i.det <- str_detect(dia2$taxon, i.tu)
        if (any(i.det)) {
                i.id <- which(i.det)
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
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
                        print(
                                paste("Final name: ", dia2$taxon[i.id])
                        )
                        i.final <- readline()
                        ## check that against fwb
                        if (check_fwb(i.final)) {
                                i.final <- get_fwb(i.final)
                        }
                        taxontable <- add_entry_tt(i.final)
                }
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

TU <- unique(data$taxon) |> sort()
(TU <- setdiff(TU, taxontable$original_name))

taxontable[,c("fix", "spe", "gen", "fam") := NULL]
taxontable[original_name == "Achnanthes microcephala", c("fixed_name", "species", "genus", "family") := .("Achnanthidium minutissimum", "Achnanthidium minutissimum", "Achnanthidium", "Achnanthidiaceae")]
taxontable[original_name == "Cymbella microcephala var.crassa", c("fixed_name", "species", "genus") := .("Encyonopsis descripta/falaisensis/microcephala", "Encyonopsis descripta/falaisensis/microcephala", "Encyonopsis")]


taxontable <- new_genus(ori = "Achnanthales", fix = "Achnanthales", spe = NA, gen = NA, fam = NA, ord = "Achnanthales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista")                               
taxontable <- new_entry(ori = "Achnanthes delicatula var. hauckiana", fix = "Planothidium delicatulum", spe = "Planothidium delicatulum", gen = "Planothidium")
taxontable <- new_entry(ori = "Achnanthes minutissima var. saprophila", "Achnanthidium saprophilum", "Achnanthidium saprophilum", "Achnanthidium")  
taxontable <- new_entry(ori = "Achnanthidium microcephalum forma scotica", fix = "Achnanthidium neomicrocephalum", spe = "Achnanthidium neomicrocephalum", gen = "Achnanthidium") 
taxontable <- new_entry(ori = "Adlafia brockmannii", fix = "Adlafia brockmannii", spe = "Adlafia brockmannii", gen = "Adlafia")                        
taxontable <- new_entry(ori = "Amphora aliformis", fix = "Amphora aliformis", spe = "Amphora aliformis", gen = "Amphora")                         
taxontable <- new_entry(ori = "Amphora waldeniana", fix = "Amphora waldeniana", spe = "Amphora waldeniana", gen = "Amphora")                         
taxontable <- new_genus(ori = "Ardissonea baculus", fix = "Ardissonea baculus", spe = "Ardissonea baculus", gen = "Ardissonea", fam = "Ardissoneaceae", ord = "Toxariales", cla = "Mediophyceae", phy = "Bacillariophyta", kin = "Chromista")                        
taxontable <- new_genus(ori = "Asteromphalus", fix = "Asteromphalus", spe = NA, gen = "Asteromphalus", fam = "Asterolampraceae", ord = "Asterolamprales", cla = "Coscinodiscophyceae", phy = "Bacillariophyta", kin = "Chromista")                            
taxontable <- new_genus(ori = "Bacterosira constricta", fix = "Bacterosira constricta", spe = "Bacterosira constricta", gen = "Bacterosira", fam = "Thalassiosiraceae", ord = "Thalassiosirales", cla = "Mediophyceae", phy = "Bacillariophyta", kin = "Chromista")                    
taxontable <- new_entry(ori = "Biremis", fix = "Biremis", spe = NA, gen = "Biremis")                                    
taxontable <- new_entry(ori = "Caloneis bacillum var. lancettula", fix = "Caloneis bacillum Complex", spe = "Caloneis bacillum Complex", gen = "Caloneis")        
taxontable <- new_entry(ori = "Campylodiscus marginatus", "Iconella marginata", "Iconella marginata", "Iconella")
taxontable <- new_genus(ori = "Climaconeis riddleae", fix = "Climaconeis riddleae", spe = "Climaconeis riddleae", gen = "Climaconeis", fam = "Berkeleyaceae", ord = "Naviculales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista")                       
taxontable <- new_genus(ori = "Climacosphenia", fix = "Climacosphenia", spe = NA, gen = "Climacosphenia", fam = "Climacospheniaceae", ord = "Toxariales", cla = "Mediophyceae", phy = "Bacillariophyta", kin = "Chromista")                
taxontable <- new_entry(ori = "Cocconeis (Other)", fix = "Cocconeis", spe = NA, gen = "Cocconeis")                          
taxontable <- new_genus(ori = "Coscinodiscus", fix = "Coscinodiscus", spe = NA, gen = "Coscinodiscus", fam = "Coscinodiscaceae", ord = "Coscinodiscales", cla = "Coscinodiscophyceae", phy = "Bacillariophyta", kin = "Chromista")                            
taxontable <- new_entry(ori = "Coscinodiscus concinnus", fix = "Coscinodiscus concinnus", spe = "Coscinodiscus concinnus", gen = "Coscinodiscus")                    
taxontable <- new_entry(ori = "Coscinodiscus jonesianus", fix = "Coscinodiscus jonesianus", spe = "Coscinodiscus jonesianus", gen = "Coscinodiscus")                  
taxontable <- new_entry(ori = "Coscinodiscus radiatus", fix = "Coscinodiscus radiatus", spe = "Coscinodiscus radiatus", gen = "Coscinodiscus")                     
taxontable <- new_genus(ori = "Cyclophora", fix = "Cyclophora", spe = NA, gen = "Cyclophora", fam = "Cyclophoraceae", ord = "Cyclophorales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista")                                
taxontable <- new_entry(ori = "Cyclophora tenuis", fix = "Cyclophora tenuis", spe = "Cyclophora tenuis", gen = "Cyclophora")                          
taxontable <- new_entry(ori = "Cymbella (Large)", fix = "Cymbella", spe = NA, gen = "Cymbella")                          
taxontable <- new_entry(ori = "Cymbella janischii", fix = "Cymbella janischii", spe = "Cymbella janischii", gen = "Cymbella")                         
taxontable <- new_entry(ori = "Cymbella microcephala form microcephala", fix = "Encyonopsis descripta/falaisensis/microcephala", spe = "Encyonopsis descripta/falaisensis/microcephala", gen = "Encyonopsis")   
taxontable <- new_entry(ori = "Cymbella stuxbergia", fix = "Cymbella stuxbergia", spe = "Cymbella stuxbergia", gen = "Cymbella")                        
taxontable <- new_genus(ori = "Detonula confervacaea", fix = "Detonula confervacaea", spe = "Detonula confervacaea", gen = "Detonula", fam = "Thalassiosiraceae", ord = "Thalassiosirales", cla = "Mediophyceae", phy = "Bacillariophyta", kin = "Chromista")                     
taxontable <- new_entry(ori = "Diatoma moniliforme", fix = "Diatoma moniliformis/tenuis", spe = "Diatoma moniliformis/tenuis", gen = "Diatoma")                        
taxontable <- new_entry(ori = "Epithemia contorta", fix = "Epithemia contorta", spe = "Epithemia contorta", gen = "Epithemia")                         
taxontable <- new_entry(ori = "Eunotia exigua var. tridentula", fix = "Eunotia  exigua/elegans Complex", spe = "Eunotia  exigua/elegans Complex", gen = "Eunotia")            
taxontable <- new_entry(ori = "Eunotia parallela var. densestriata", fix = "Eunotia parallela Complex", spe = "Eunotia parallela Complex", gen = "Eunotia")        
taxontable <- new_entry(ori = "Fistulifera solaris", fix = "Fistulifera solaris", spe = "Fistulifera solaris", gen = "Fistulifera")                        
taxontable <- new_entry(ori = "Fragilaria (Other)", fix = "Fragilaria", spe = NA, gen = "Fragilaria")                         
taxontable <- new_entry(ori = "Fragilaria vaucheriae var. capitellata", fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry(ori = "Fragilaria virescens var. exigua", fix = "Fragilaria virescens complex", spe = "Fragilaria virescens complex", gen = "Fragilaria")           
taxontable <- new_entry(ori = "Frustulia gondwana", fix = "Frustulia gondwana", spe = "Frustulia gondwana", gen = "Frustulia")                         
taxontable <- new_entry(ori = "Gomphonema (Other)", fix = "Gomphonema", spe = NA, gen = "Gomphonema")                         
taxontable <- new_entry(ori = "Gomphonema acuminatum var. coronatum", fix = "Gomphonema acuminatum Complex", spe = "Gomphonema acuminatum Complex", gen = "Gomphonema")     
taxontable <- new_entry(ori = "Gomphonema angustatum var. productum", fix = "Gomphonema angustatum Complex", spe = "Gomphonema angustatum Complex", gen = "Gomphonema")       
taxontable <- new_entry(ori = "Gomphonema angustatum var. sarcophagus", fix = "Gomphonema angustatum Complex", spe = "Gomphonema angustatum Complex", gen = "Gomphonema")         
taxontable <- new_entry(ori = "Gomphonema grovei", fix = "Gomphosphenia", spe = NA, gen = "Gomphosphenia")                          
taxontable <- new_entry(ori = "Gomphonema intricatum var. pumilum", fix = "Gomphonema pumilum complex", spe = "Gomphonema pumilum complex", gen = "Gomphonema")        
taxontable <- new_entry(ori = "Gomphonema narodoense", fix = "Gomphonema narodoense", spe = "Gomphonema narodoense", gen = "Gomphonema")                      
taxontable <- new_entry(ori = "Gomphonema truncatum var. capitatum", fix = "Gomphonema pala", spe = "Gomphonema pala", gen = "Gomphonema")       
taxontable <- new_entry(ori = "Halamphora caribaea", fix = "Halamphora caribaea", spe = "Halamphora caribaea", gen = "Halamphora")                        
taxontable <- new_entry(ori = "Halamphora subtropica", fix = "Halamphora subtropica", spe = "Halamphora subtropica", gen = "Halamphora")                                             
taxontable <- new_entry(ori = "Halamphora woelfeliae", fix = "Halamphora woelfeliae", spe = "Halamphora woelfeliae", gen = "Halamphora")                      
taxontable <- new_entry(ori = "Hantzschia amphioxys var. major", fix = "Hantzschia amphioxys", spe = "Hantzschia amphioxys", gen = "Hantzschia")           
taxontable <- new_entry(ori = "Haslea", fix = "Haslea", spe = NA, gen = "Haslea")                                     
taxontable <- new_entry(ori = "Lemnicola", fix = "Lemnicola", spe = NA, gen = "Lemnicola")                                 
taxontable <- new_entry(ori = "Licmophora peragallioides", fix = "Licmophora peragallioides", spe = "Licmophora peragallioides", gen = "Licmophora")                  
taxontable <- new_entry(ori = "Luticola sparsipunctata", fix = "Luticola sparsipunctata", spe = "Luticola sparsipunctata", gen = "Luticola")                   
taxontable <- new_entry(ori = "Mayamaea terrestris", fix = "Mayamaea terrestris", spe = "Mayamaea terrestris", gen =  "Mayamaea")                       
taxontable <- new_genus(ori = "Microfissurata", fix = "Microfissurata", spe = NA, gen = "Microfissurata", fam = "Naviculales incertae sedis", ord = "Naviculales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista")                            
taxontable <- new_entry(ori = "Navicula - small forms", fix = "Navicula", spe = NA, gen = "Navicula")                     
taxontable <- new_entry(ori = "Navicula (Small)", fix = "Navicula", spe = NA, gen = "Navicula")                          
taxontable <- new_entry(ori = "Navicula [small species]", fix = "Navicula", spe = NA, gen = "Navicula")                   
taxontable <- new_entry(ori = "Navicula porifera var. opportuna", fix = "Placoneis", spe = NA, gen = "Placoneis")          
taxontable <- new_entry(ori = "Navicula pseudacceptata", fix = "Navicula pseudacceptata", spe = "Navicula pseudacceptata", gen = "Navicula")                    
taxontable <- new_entry(ori = "Neidium fossum", fix = "Neidium fossum", spe = "Neidium fossum", gen = "Neidium")                            
taxontable <- new_genus(ori = "Neodelphineis", fix = "Neodelphineis", spe = NA, gen = "Neodelphineis", fam = "Rhaphoneidaceae", ord = "Rhaphoneidales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista")                              
taxontable <- new_entry(ori = "Nitzschia (Other)", fix = "Nitzschia", spe = NA, gen = "Nitzschia")                         
taxontable <- new_entry(ori = "Nitzschia ardua", fix = "Tryblionella ardua", spe = "Tryblionella ardua", gen = "Tryblionella")                            
taxontable <- new_entry(ori = "Pinnularia brebissonii var. acuta", fix = "Pinnularia brebissonii Complex", spe = "Pinnularia brebissonii Complex", gen = "Pinnularia")         
taxontable <- new_genus(ori = "Plagiostriata goreensis", fix = "Plagiostriata goreensis", spe = "Plagiostriata goreensis", gen = "Plagiostriata", fam = "Staurosiraceae", ord = "Fragilariales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista")                    
taxontable <- new_entry(ori = "Planothidium caputium", fix = "Planothidium victorii", spe = "Planothidium victorii", gen = "Planothidium")                     
taxontable <- new_genus(ori = "Rossia", fix = "Rossia", spe = NA, gen = "Rossia", fam = "Sellaphoraceae", ord = "Naviculales", cla = "Bacillarioophyceae", phy = "Bacillariophyta", kin = "Chromista")                                     
taxontable <- new_entry(ori = "Sellaphora pupula var. pseudopupula", fix = "Sellaphora pupula Complex", spe = "Sellaphora pupula Complex", gen = "Sellaphora")       
taxontable <- new_entry(ori = "Simonsenia aveniformis", fix = "Simonsenia aveniformis", spe = "Simonsenia aveniformis", gen = "Simonsenia")                     
taxontable <- new_entry(ori = "Stauroneis anceps form gracilis", fix = "Stauroneis anceps Complex", spe = "Stauroneis anceps Complex", gen = "Stauroneis")            
taxontable <- new_entry(ori = "Stauroneis schmidiae", fix = "Stauroneis schmidiae", spe = "Stauroneis schmidiae", gen = "Stauroneis")                       
taxontable <- new_entry(ori = "Staurosirella - type", fix = "Staurosirella", spe = NA, gen = "Staurosirella")                      
taxontable <- new_entry(ori = "Stephanodiscus - type", fix = "Stephanodiscus", spe = NA, gen = "Stephanodiscus")                      
taxontable <- new_entry(ori = "Surirella iconella", fix =  "Surirella iconella", spe = "Surirella iconella", gen = "Surirella")                       
taxontable <- new_entry(ori = "Surirella lineopunctata", fix = "Iconella lineopunctata", spe = "Iconella lineopunctata", gen = "Iconella")                    
taxontable <- new_entry(ori = "Synedra (Other)", fix = "Synedra", spe = NA, gen = "Synedra")                           
taxontable <- new_entry(ori = "Synedra acus var. angustissima", fix = "Fragilaria tenera complex", spe = "Fragilaria tenera complex", gen = "Fragilaria")             
taxontable <- new_entry(ori = "Synedra fragilarioides", fix = "Synedra fragilarioides", spe = "Synedra fragilarioides", gen = "Synedra")                    
taxontable <- new_entry(ori = "Synedra parasitica var. subconstricta", fix = "Pseudostaurosira parasitica complex", spe = "Pseudostaurosira parasitica complex", gen = "Pseudostaurosira")      
taxontable <- new_entry(ori = "Synedra ulna var. biceps", fix = "Ulnaria ulna complex", spe = "Ulnaria ulna complex", gen = "Ulnaria")                  
taxontable <- new_genus(ori = "Synedropsis recta", fix = "Synedropsis recta", spe = "Synedropsis recta", gen = "Synedropsis", fam = "Fragilariaceae", ord = "Fragilariales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista")                           
taxontable <- new_entry(ori = "Thalassiosira guiard", fix = "Thalassiosira guillardii", spe = "Thalassiosira guillardii", gen = "Thalassiosira")                      
taxontable <- new_entry(ori = "Thalassiosira nodulolineata", fix = "Thalassiosira nodulolineata", spe = "Thalassiosira nodulolineata", gen = "Thalassiosira")                
taxontable <- new_genus(ori = "Toxarium hennedyanum", fix = "Toxarium hennedyanum", spe = "Toxarium hennedyanum", gen = "Toxarium", fam = "Toxariaceae", ord = "Toxariales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista" )                      
taxontable <- new_entry(ori = "Toxarium undulatum", fix = "Taxarium undulatum", spe = "Toxarium undulatum", gen = "Toxarium")


taxontable <- new_entry("Acanthoceras madeburgensis", fix = "Acanthoceras zachariasii", spe = "Acanthoceras zachariasii", gen = "Acanthoceras")
taxontable <- new_entry("Achnanthidium ventrale", fix = "Psammothidium ventrale", spe = "Psammothidium ventrale", gen = "Psammothidium")
taxontable <- new_entry("Actinocyclus normanii f. subsalsa", fix = "Actinocyclus normanii", spe = "Actinocyclus normanii", gen = "Actinocyclus")
taxontable <- new_genus("Amphiprora", fix = "Amphiprora", spe = NA, gen =  "Amphiprora", fam = "Amphipleuraceae", ord = "Naviculales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista" )                     
taxontable <- new_entry("Amphora (Other)", fix = "Amphora", spe = NA, gen =  "Amphora")
taxontable <- new_entry("Aulacoseira italica subsp. subarctica", fix = "Aulacoseira italica complex", spe = "Aulacoseira italica complex", gen =  "Aulacoseira")
taxontable <- new_entry("Cyclotella bodanica var. affinis", fix = "Lindavia bodanica complex", spe = "Lindavia bodanica complex", gen =  "Lindavia")
taxontable <- new_entry("Cymatopleura elliptica var. constricta", fix = "Cymatopleura elliptica", spe = "Cymatopleura elliptica", gen =  "Cymatopleura")
taxontable <- new_entry("Cymbella (Other)", fix = "Cymbella", spe = NA, gen =  "Cymbella")
taxontable <- new_entry("Diatoma (Other)", fix = "Diatoma", spe = NA, gen =  "Diatoma")
taxontable <- new_entry("Diatoma vulgare var. ovalis", fix = "Diatoma vulgare", spe = "Diatoma vulgare", gen = "Diatoma")
taxontable <- new_entry("Eunotia curvata var. linearis", fix = "Eunotia arcus/mucophila/bilunaris Complex", spe = "Eunotia arcus/mucophila/bilunaris Complex" , gen = "Eunotia")
taxontable <- new_entry("Eunotia pectinalis var. ventricosa", fix = "Eunotia pectinalis Complex", spe = "Eunotia pectinalis Complex", gen =  "Eunotia")
taxontable <- new_entry("Eunotia serra var. tetraodon", fix = "Eunotia serra Complex", spe = "Eunotia serra Complex", gen = "Eunotia" )
taxontable <- new_entry("Fragilaria constricta fo. constricta", fix = "Fragilariforma constricta-lata", spe = "Fragilariforma constricta-lata", gen =  "Fragilariforma")
taxontable <- new_entry("Fragilaria constricta fo. stricta", fix = "Fragilariforma constricta-lata", spe = "Fragilariforma constricta-lata", gen =  "Fragilariforma")
taxontable <- new_entry("Fragilaria virescens var. capitata", fix = "Fragilaria virescens complex", spe = "Fragilaria virescens complex", gen = "Fragilaria")
taxontable <- new_entry("Fragilariforma constricta var. trinodis", fix = "Fragilariforma constricta-lata", spe = "Fragilariforma constricta-lata", gen = "Fragilariforma" )
taxontable <- new_entry("Fragilariforma virescens var. mesolepta", fix = "Fragilaria virescens complex", spe = "Fragilaria virescens complex", gen = "Fragilaria" )
taxontable <- new_entry("Frustulia rhomboides form undulata", fix = "Frustulia rhomboides Complex", spe = "Frustulia rhomboides Complex", gen =  "Frustulia")
taxontable <- new_entry("Gomphonema parvulum var. micropus", fix = "Gomphonema parvulum Complex", spe = "Gomphonema parvulum Complex", gen = "Gomphonema" )
taxontable <- new_entry("Gomphonema vibrio var. pumilum", fix = "Gomphonema vibrio", spe = "Gomphonema vibrio", gen = "Gomphonema" )
taxontable <- new_entry("Kolbesia ploenensis var. gessneri", fix = "Karayevia ploenensis", spe = "Karayevia ploenensis", gen = "Karayevia" )
taxontable <- new_entry("Luticola mutica form intermedia", fix = "Luticola", spe = NA, gen = "Luticola")
taxontable <- new_entry("Navicula (Other)", fix = "Navicula", spe = NA, gen = "Navicula")
taxontable <- new_entry("Navicula digitoradiata form linearis", fix = "Navicula cryptotenella/cryptotenelloides", spe = "Navicula cryptotenella/cryptotenelloides", gen = "Navicula" )
taxontable <- new_entry("Navicula digitoradiata var. minima",   fix = "Navicula cryptotenella/cryptotenelloides", spe = "Navicula cryptotenella/cryptotenelloides", gen = "Navicula" )
taxontable <- new_entry("Navicula genustriata", fix = "Pinnunavis genustriata", spe = "Pinnunavis genustriata", gen = "Pinnunavis")
taxontable <- new_entry("Navicula ignota var. palustris", fix = "Geissleria ignota complex", spe = "Geissleria ignota complex", gen = "Geissleria" )
taxontable <- new_entry("Navicula placentula var. apiculata", fix = "Placoneus", spe = NA, gen = "Placoneis" )
taxontable <- new_entry("Navicula pseudolanceolata var. densilineolata", fix = "Navicula lanceolata complex", spe = "Navicula lanceolata complex", gen = "Navicula" )
taxontable <- new_entry("Navicula recondita var. recondita", fix = "Mayamaea recondita", spe = "Mayamaea recondita", gen = "Mayamaea" )
taxontable <- new_entry("Nitzschia acuta", fix = "Nitzschia acuta", spe = "Nitzschia acuta", gen = "Nitzschia" )
taxontable <- new_entry("Nitzschia incurva var. subtilis", fix = "Nitzschia incurva", spe = "Nitzschia incurva", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia tryblionella var. debilis", fix = "Nitzschia", spe = NA, gen =  "Nitzschia")
taxontable <- new_entry("Planothidium haynaldii var. elliptico-lanceolata", fix = "Planothidium", spe = NA, gen = "Planothidium")
taxontable <- new_entry("Planothidium subatomoides", fix = "Planothidium subatomoides", spe = "Planothidium subatomoides", gen = "Planothidium" )
taxontable <- new_entry("Stauroneis phoenicenteron form gracilis", fix = "Stauroneis phoenicenteron Complex", spe = "Stauroneis phoenicenteron Complex", gen =  "Stauroneis")
taxontable <- new_entry("Synedra amphicephala var. austriaca", fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen =  "Fragilaria")
taxontable <- new_entry("Synedra robusta", fix = "Ardissonea robusta", spe = "Ardissonea robusta", gen = "Ardissonea" )
taxontable <- new_entry("Tabellaria flocculosa var. asterionelloides", fix = "Tabellaria flocculosa Complex", spe = "Tabellaria flocculosa Complex", gen =  "Tabellaria")
taxontable <- new_entry("Thalassionema", fix = "Thalassionema", spe = NA, gen =  "Thalassionema")


taxontable <- taxontable[!duplicated(taxontable$original_name)]

TU <- unique(data$taxon) |> sort()
(TU <- setdiff(TU, taxontable$original_name))

## The two last entries include " as element and are thus difficult to refer to. 
# "Encyonema \"\"\"\"ventricosum\"\"\"\" agg."
taxontable <- new_entry(ori = TU[1], fix = "Gomphonema ventricosum", spe = "Gomphonema ventricosum", gen = "Gomphonema")
# "Gomphonema \"\"\"\"intricatum\"\"\"\" type"
taxontable <- new_entry(ori = TU[2], fix = "Gomphonema vibrio", spe = "Gomphonema vibrio", gen = "Gomphonema")

check_taxon_table(taxontable)

taxontable[class == "Bacillarioophyceae", class := "Bacillariophyceae"]
taxontable[order == "Coscinodiscales", class := "Coscinodiscophyceae"]
taxontable[order == "Toxariales", class := "Mediophyceae"]