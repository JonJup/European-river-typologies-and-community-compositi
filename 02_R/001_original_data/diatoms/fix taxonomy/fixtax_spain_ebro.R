TU <- unique(data$taxon)|> sort()
TU <- setdiff(TU, taxontable$original_name)

## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 100000)
strdist_tbl <- 
        data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
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

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

for (i in seq_along(TU)) {
        i.tu  <- TU[i]
        i.det <- str_detect(dia2$taxon, i.tu)
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
                ## check that against fwb
                if (check_fwb(i.final)) {
                        i.final <- get_fwb(i.final)
                }
                taxontable <- add_entry_tt(i.final)
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
}

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

taxontable <- new_entry(ori = "Achnanthidium petersenii", fix = "Achnanthidium petersenii")
taxontable <- new_genus(ori = "Delicatophycus delicatulus", fix = "Delicatophycus delicatulus", spe = "Delicatophycus delicatulus", gen = "Delicatophycus", fam = "Cymbellaceae", ord = "Cymbellales", cla = "Bacillariophyceae")
taxontable <- new_genus(ori = "Dorofeyukea kotschyi", fix = "Dorofeyukea kotschyi", spe = "Dorofeyukea kotschyi", gen = "Dorofeyukea", fam = "Stauroneidaceae", ord = "Naviculales", cla = "Bacillariophyceae")
taxontable <- new_entry(ori = "Gomphonella olivacea", fix = "Gomphonella olivacea")
taxontable <- new_entry(ori = "Haslea duerrenbergiana", fix = "Haslea duerrenbergiana")
taxontable <- new_entry(ori = "Lindavia comta", fix = "Lindavia comta")
taxontable <- new_entry(ori = "Nitzschia tenuis var. sigmoidea", fix = "Nitzschia pura-linearis Complex")
taxontable <- new_entry(ori = "Paraplaconeis minor", fix = "Paraplaconeis minor")
taxontable <- new_entry(ori = "PROSCHKINIA sp.", fix = "Proschkinia", spe = NA, gen = "Proschkinia")
taxontable <- new_entry(ori = "Punctastriata subconstricta", fix = "Pseudostaurosira subconstricta")
taxontable <- new_entry(ori = "Sellaphora raederae", fix = "Sellaphora raederae")
taxontable <- new_entry(ori = "Staurosirella neopinnata", fix = "Staurosirella neopinnata")
taxontable <- new_entry(ori = "Ulnaria sp.", fix = "Ulnaria", spe = NA, gen = "Ulnaria")

taxontable

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

check_taxon_table(taxontable)

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))