## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 100)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

# manual additions to taxontable  
taxontable <- append_to_tt("Nitzschia acicularis", "Nitzschia acicularis -Formenkreis")
taxontable <- append_to_tt("Fragilaria ulna var. ulna", "Fragilaria ulna angustissima - Sippen")

taxontable[genus == "Ulnaria", c("family", "order") := .("Ulnariaceae", "Licmophorales")]


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

taxontable <- new_genus(ori = "Acanthoceras zachariasii",
                        fix = "Acanthoceras zachariasii",
                        spe = "Acanthoceras zachariasii",
                        gen = "Acanthoceras",
                        fam = "Chaetocerotaceae",
                        ord = "Chaetocerotales",
                        cla = "Mediophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Rhizosolenia",
                        fix = "Rhizosolenia",
                        spe = NA,
                        gen = "Rhizosolenia",
                        fam = "Rhizosoleniaceae",
                        ord = "Rhizosoleniales",
                        cla = "Coscinodiscophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Rhizosolenia longiseta",
                        fix = "Urosolenia longiseta",
                        spe = "Urosolenia longiseta",
                        gen = "Urosolenia",
                        fam = "Rhizosoleniaceae",
                        ord = "Rhizosoleniales",
                        cla = "Coscinodiscophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Stephanocostis",
                        fix = "Stephanocostis",
                        spe = NA,
                        gen = "Stephanocostis",
                        fam = "Stephanodiscaceae",
                        ord = "Stephanodiscales",
                        cla = "Mediophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)



setorderv(taxontable, "original_name")

TU <- setdiff(sort(unique(data$taxon)),taxontable$original_name)

## check that the remaining taxa are all non diatoms 
for (i in seq_along(TU)){
        if (i == 1) looplog <- c()
        i.tu <- TU[i]
        i.res <- taxize::classification(i.tu, db = "gbif")
        if (all(is.na(i.res[[1]])))
                next()
        if (i.res[[1]][which(i.res[[1]]$rank == "phylum"), 1] == "Bacillariophyta")
                looplog[length(looplog)+1] <- print(i)
}
## -> none 
non_diatom_algae <- append(non_diatom_algae, TU)
saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))
saveRDS(non_diatom_algae, paste0("data/diatoms/",Sys.Date(),"_non_diatom_algae.rds"))

data <- data[!taxon %in% non_diatom_algae]