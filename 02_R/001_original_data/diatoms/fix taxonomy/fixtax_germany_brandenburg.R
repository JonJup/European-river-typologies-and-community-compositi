## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name))

taxontable <- append_to_tt("Eunotia nymanniana Lectotypus", "Eunotia nymanniana non Lectotypus")
taxontable <- append_to_tt("Karayevia clevei var.rostrata", "Karayevia clevei var. rostrata")
taxontable <- append_to_tt("Karayevia ploenensis var.gessneri", "Karayevia ploenensis var. gessneri")
taxontable <- append_to_tt("Navicula gottlandica", "Navicula gotlandica")
taxontable <- append_to_tt("Pinnularia divergens var.decrescens", "Pinnularia divergens var. decrescens")
taxontable <- append_to_tt("Pinnularia microstauron var.angusta", "Pinnularia microstauron var. angusta")
taxontable <- append_to_tt("Pinnularia rhombarea var.halophila", "Pinnularia rhombarea var. halophila")

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

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
TU <- setdiff(TU, taxontable$original_name)

taxontable <- new_entry("Adlafia langebertalotii", "Adlafia langebertalotii", "Adlafia langebertalotii", "Adlafia")
taxontable <- new_entry("Amphora lange-bertalotii var. tenuis" , "Amphora lange-bertalotii", "Amphora lange-bertalotii", "Amphora")
taxontable <- new_entry("Cymbella hustedtii var. crassipunctata", "Cymbella", NA, "Cymbella")
taxontable <- new_entry("Cymbopleura incerta var. incerta", "Cymbopleura incerta complex", "Cymbopleura incerta complex", "Cymbopleura")
taxontable <- new_entry("Encyonema silesiacum var. altense", "Encyonema silesicacum/minutum/lange-bertalotii", "Encyonema silesicacum/minutum/lange-bertalotii", "Encyonema")
taxontable <- new_entry("Epithemia westermannii", "Epithemia westermanni", "Epithemia westermanni", "Epithemia")
taxontable <- new_entry("Eunotia fallax var. groenlandica", "Eunotia Complex", NA, "Eunotia")
taxontable <- new_entry("Gomphonema pseudointermedia", "Gomphonema pseudointermedium", "Gomphonema pseudointermedium", "Gomphonema")
taxontable <- new_entry("Hippodonta olofjarlmannii", "Hippodonta olofjarlmannii", "Hippodonta olofjarlmannii", "Hippodonta")
taxontable <- new_entry("Navicula riparia var. mollenhaueri", "Craticula Complex", NA, "Craticula")
taxontable <- new_entry("Pinnularia esoxiformis var. angusta", "Pinnularia esoxiformis", "Pinnularia esoxiformis", NA)
taxontable <- new_entry("Pinnularia mesolepta var. gibberula", "Pinnularia mesolepta Complex", "Pinnularia mesolepta Complex", "Pinnularia")
taxontable <- new_entry("Planothidium robustum var. abbreviatum", "Planothidium robustum", "Planothidium robustum", "Planothidium")
taxontable <- new_entry("Planothidium vanheurckii", "Planothidium vanheurckii", "Planothidium vanheurckii", "Planothidium")
taxontable <- new_entry("Pinnularia esoxiformis var. angusta", "Pinnularia esoxiformis")

taxontable[original_name == "Cymbella incerta", c("fixed_name", "species") := 
                   .("Cymbopleura incerta complex", "Cymbopleura incerta complex")]

## no taxa missing 
TU <- unique(data$taxon) |> setdiff(taxontable$original_name) |> sort()
## taxontabe ok 
check_taxon_table(taxontable)

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))
