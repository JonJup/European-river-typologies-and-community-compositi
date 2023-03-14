# fixtax spain duerto


TU <- unique(data$taxon)|> sort()
TU <- setdiff(TU, taxontable$original_name)

strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 100000)
strdist_tbl <- 
        data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        dp$filter(!is.na(taxontable_name)) 

for (i in 1:nrow(strdist_tbl)){
        
        i.1 <- dp$pull(strdist_tbl[i,1])
        i.2 <- dp$pull(strdist_tbl[i,2])
        
        print(i.1)
        print(i.2)
        
        i.bool <- readline("match?:")
        
        if (i.bool == "y"){
                taxontable <- dfm$append_to_tt(i.1,i.2,data = taxontable)
        } else if (i.bool == "n") {
                next()
        }
        rm(list = ls()[grepl("^i", ls())])
}

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

for (i in seq_along(TU)) {
        i.tu  <- TU[i]
        i.det <- stringr::str_detect(dia2$taxon, i.tu)
        if (any(i.det)) {
                i.id <- which(i.det)
                print(paste("name:", i.tu))
                print(paste("suggestions:", dia2$taxon[i.id]))
                i.rl <- readline()
        } else {
                i.id <- amatch(i.tu, dia2$taxon, maxDist = 100000) 
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
                taxontable <- dfm$add_entry_tt(i.final, data = taxontable, data2 = i.tu)
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

taxontable <- new_entry(ori = "Fallacia subpygmaea", fix = "Fallacia pygmaea-forcipata")
taxontable <- new_entry("Gyrosigma acuminatum var. curta" , fix = "Gyrosigma")
taxontable <- new_entry("Hippodonta hungarica var. obtusa", fix = "Hippodonta hungarica")  
taxontable <- new_entry("Luticola quinquenodis", fix = "Luticola quinquenodis")
taxontable <- new_entry("Nitzschia frustulum var. subsalina", fix = "Nitzschia frustulum Complex")
taxontable <- new_entry("Surirella elliptica", fix = "Surirella elliptica")
taxontable[fixed_name == "Nitzschia frustulum", c("fixed_name", "species") := "Nitzschia frustulum Complex"]                        

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)
     
dfm$check_taxon_table(taxontable)

taxontable <- taxontable[-duplicated(taxontable$original_name)]
rm(strdist_id, strdist_tbl, TU, dia1, dia2);gc()
