## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

## manual additions to taxon table 
taxontable <- append_to_tt("Aulacoseira granulata morphotype curvata","Aulacoseira granulata Morphotyp curvata")
taxontable <- append_to_tt("Aulacoseira granulata var.angustissima","Aulacoseira granulata var. angustissima")
taxontable <- append_to_tt("Nitzschia acicularis -Formenkreis","Nitzschia acicularis - Formenkreis")

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

TU <- setdiff(TU, taxontable$original_name)

taxontable <- new_genus(ori = "Cylindrotheca",
                        fix = "Cylindrotheca",
                        spe = NA,
                        gen = "Cylindrotheca",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Nitzschia calida var. salinarum",
                        fix = "Tryblionella gracilis ",
                        spe = "Tryblionella gracilis" ,
                        gen = "Tryblionella",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")

## check that TU is empty now
TU <- setdiff(TU, taxontable$original_name)

check_taxon_table(taxontable)

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))