## -- harmonize diatom names - Finland Janne Soininen 


## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 100)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

# manual additions to taxontable  
taxontable <- append_to_tt("Achnanthes lanceolata var.rostrata", "Achnanthes lanceolata rostrata")
taxontable <- append_to_tt("Achnanthes", "Achnanthes sp1")
taxontable <- append_to_tt("Achnanthes", "Achnanthes sp2")
taxontable <- append_to_tt("Achnanthes", "Achnanthes sp3")
taxontable <- append_to_tt("Amphora", "Amphora sp")
taxontable <- append_to_tt("Aulacoseira", "Aulacoseira sp1")
taxontable <- append_to_tt("Caloneis", "Caloneis sp")
taxontable <- append_to_tt("Caloneis pulchra", "Caloneis pulchra wide")
taxontable <- append_to_tt("Cavinula", "Cavinula sp")
taxontable <- append_to_tt("Cyclotella", "Cyclotella sp")
taxontable <- append_to_tt("Cymbella", "Cymbella sp1")
taxontable <- append_to_tt("Diatoma tenuis", "Diatoma tenuis var2")
taxontable <- append_to_tt("Epithemia adnata", "Epithemia adnata wide")
taxontable <- append_to_tt("Eunotia", "Eunotia sp1")
taxontable <- append_to_tt("Eunotia", "Eunotia sp2")
taxontable <- append_to_tt("Eunotia", "Eunotia sp3")
taxontable <- append_to_tt("Eunotia", "Eunotia sp4")
taxontable <- append_to_tt("Eunotia meisteri","Eunotia meisterii")
taxontable <- append_to_tt("Eunotia serra","Eunotia serra diadon")
taxontable <- append_to_tt("Eunotia serra","Eunotia serra triodon")
taxontable <- append_to_tt("Fragilaria","Fragilaria sp1")
taxontable <- append_to_tt("Fragilaria capucina","Fragilaria capucina 70")
taxontable <- append_to_tt("Fragilaria capucina var.rumpens","Fragilaria capucina var rumpens")
taxontable <- append_to_tt("Fragilaria capucina var.rumpens","Fragilaria capucina var rumpens wide")
taxontable <- append_to_tt("Fragilaria capucina var.gracilis","Fragilaria capucina gracilis")
taxontable <- append_to_tt("Fragilaria capucina var.mesolepta","Fragilaria capucina mesolepta")
taxontable <- append_to_tt("Fragilaria capucina var.vaucheriae","Fragilaria capucina vaucheriae")
taxontable <- append_to_tt("Frustulia rhomboides var.viridula","Frustulia rhomboides var viridula")
taxontable <- append_to_tt("Gomphonema","Gomphonema sp1")
taxontable <- append_to_tt("Melosira","Melosira sp")
taxontable <- append_to_tt("Navicula","Navicula jo3")
taxontable <- append_to_tt("Navicula","Navicula sp1")
taxontable <- append_to_tt("Navicula","Navicula sp2")
taxontable <- append_to_tt("Navicula","Navicula sp2 small")
taxontable <- append_to_tt("Navicula begeri","Navicula begerii")
taxontable <- append_to_tt("Navicula detenta","Navicula detenta 2")
taxontable <- append_to_tt("Navicula detenta","Navicula detenta wide")
taxontable <- append_to_tt("Navicula minima","Navicula minima 2")
taxontable <- append_to_tt("Navicula schroeteri","Navicula schroeterii")
taxontable <- append_to_tt("Navicula soehrensis","Navicula sorensis")
taxontable <- append_to_tt("Neidium","Neidium sp")
taxontable <- append_to_tt("Nitzschia","Nitzschia sp1")
taxontable <- append_to_tt("Nitzschia acidoclinata","Nitzschia acidoclinata 1")
taxontable <- append_to_tt("Nitzschia acidoclinata","Nitzschia acidoclinata 2")
taxontable <- append_to_tt("Nitzschia angustata","Nitzschia angusta")
taxontable <- append_to_tt("Nitzschia homburgiensis","Nitzschia homburgensis")
taxontable <- append_to_tt("Nitzschia sigmoidea","Nitzschia sigmoidea long")
taxontable <- append_to_tt("Pinnularia","Pinnularia sp1")
taxontable <- append_to_tt("Stenopterobia","Stenopterobia sp")
taxontable <- append_to_tt("Surirella angusta","Surirella angusta 2")

TU <- setdiff(sort(unique(data$taxon)),taxontable$original_name)

## check against fwb table 

for (i in seq_along(TU)){
        i.tu  <- TU[i]
        if(check_fwb(i.tu)){
                x <- get_fwb(i.tu)
                taxontable <- add_entry_tt(x)
        }
        rm(i.tu)
}; rm(i)

setorderv(taxontable, "original_name")

TU <- setdiff(sort(unique(data$taxon)),taxontable$original_name)

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
setorderv(taxontable, "original_name")
taxontable[original_name == "Fragilaria construens", c("fixed_name", "species") := .("Fragilaria construens-pseudoconstruens", 
                                                                                     "Fragilaria construens-pseudoconstruens")]
taxontable[original_name == "Staurosirella pinnata", c("fixed_name", "species") := .("Staurosirella pinnata complex",
                                                                                     "Staurosirella pinnata complex")]

taxontable <- new_entry("Achnanthes clementis", "Achnanthes clementis") # not found anywhere 
taxontable <- new_entry("Achnanthes kriegerii", "Achnanthidium kriegeri") 
taxontable <- new_entry("Achnanthes kriegerii big", "Achnanthidium kriegeri") 
taxontable <- new_entry("Aulacoseira distans lirata", "Aulacoseira distans complex")
taxontable <- new_entry("Aulacoseira italica valida", "Aulacoseira italica complex")          
taxontable <- new_entry("Cyclotella paucistriata", "Cyclotella paucistriata") # not found anywhere        
taxontable <- new_entry("Cymbella microcephala major", "Encyonopsis descripta/falaisensis/microcephala" )        
taxontable <- new_entry("Cymbella minuta var silesiaca", "Encyonema silesicacum/minutum/lange-bertalotii" )      
taxontable <- new_entry("Cymbella naviculiformis var 2", "Cymbopleura", spe = NA, gen = "Cymbopleura" )     
taxontable <- new_entry("Cymbella silesiaca var silesiaca", "Encyonema silesicacum/minutum/lange-bertalotii")    
taxontable <- new_entry("Eunotia sorensis","Eunotia sorensis")    
taxontable <- new_entry("Eunotia subtilissima wide", "Eunotia subtilissima")   
taxontable <- new_entry("Eunotia turgidula", "Eunotia turgidula")    
taxontable <- new_entry("Fragilaria capucina paucistriata", "Fragilaria capucina complex")    
taxontable <- new_entry("Fragilaria constricta 2", "Fragilariforma constricta-lata")   
taxontable <- new_entry("Fragilaria tenella", "Fragilaria tenella")    
taxontable <- new_entry("Fragilaria ulna var acus", "Ulnaria ulna var.acus")    
taxontable <- new_entry("Fragilaria venter","Fragilaria venter")   
taxontable <- new_entry("Gomphonema ventricosa", "Gomphonema ventricosa")    
taxontable <- new_entry("Navicula agrestis wide", "Mayamaea", spe = NA, gen = "Mayamaea")    
taxontable <- new_entry("Navicula atomus var permitis", "Mayamaea", spe = NA, gen = "Mayamaea"    )   
taxontable <- new_entry("Navicula concerta", "Navicula concerta")    
taxontable <- new_entry("Navicula gallica capitata", "Diadesmis gallica")    
taxontable <- new_entry("Navicula jarnefeltii", "Navicula jarnefeltii")   
taxontable <- new_entry("Navicula kuelpsii", "Navicula kuelpsi")    
taxontable <- new_entry("Navicula schmasmannii", "Humidophila schmassmannii ")    
taxontable <- new_entry("Navicula tripartita","Navicula tripartita")   
taxontable <- new_entry("Navicula uniseriata","Navicula uniseriata")    
taxontable <- new_entry("Pinnularia microcephala", "Pinnularia microcephala")    
taxontable <- new_entry("Pinnularia subcapitata var hilseana", "Pinnularia subcapitata Complex")

taxontable[fixed_name == "Pinnularia subcapitata", c("fixed_name", "species") := "Pinnularia subcapitata Complex"]

TU <- setdiff(sort(unique(data$taxon)),taxontable$original_name)

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))