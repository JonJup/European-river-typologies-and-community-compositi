### --- harmonize taxa Jenny Jyrkänkallio Mikkola

## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

## manual additions to taxon table 
taxontable <- append_to_tt("Adlafia minuscula var. muralis","Adlafia minuscula var muralis")
taxontable <- append_to_tt("Adlafia aquaeductae","Adlafia aquaductae")
taxontable <- append_to_tt("Aneumastus tusculus","Aneumastus tuscula")
taxontable <- append_to_tt("Eunotia pectinalis var.undulata","Eunotia pectinalis var undulata")
taxontable <- append_to_tt("Eunotia sp.","Eunotia sp")
taxontable <- append_to_tt("Fragilaria capucina var.capitellata","Fragilaria capucina var capitellata")
taxontable <- append_to_tt("Fragilaria sp.","Fragilaria sp")
taxontable <- append_to_tt("Fragilaria tenera var.nanana","Fragilaria tenera var nanana")
taxontable <- append_to_tt("Fragilariforma virescens var.subsalina","Fragilariforma virenscens var subsalina")
taxontable <- append_to_tt("Gomphosphenia grovei var.lingulata","Gomphospenia grovei var lingulata")
taxontable <- append_to_tt("Gomphosphenia tackei","Gomphospenia tackei")
taxontable <- append_to_tt("Gomphonema sp.","Gompohonema sp")
taxontable <- append_to_tt("Hantzschia sp.","Hantzschia sp")
taxontable <- append_to_tt("Mayamaea fossalis var.obsidialis","Mayamaea fossalis var obsidialis")
taxontable <- append_to_tt("Navicula difficillima","Navicula dificillima")
taxontable <- append_to_tt("Navicula menisculus","Navicula meninsculus")
taxontable <- append_to_tt("Navicula rhynchocephala","Navicula rhyncocephala")
taxontable <- append_to_tt("Navicula slesvicensis","Navicula slevicensis")
taxontable <- append_to_tt("Navicula sp.","Navicula sp")
taxontable <- append_to_tt("Navicula decussis","Navigeia decussis")
taxontable <- append_to_tt("Neidium longiceps","Neidium longiseps")
taxontable <- append_to_tt("Nitzschia epithemoides var.disputata","Nitzschia epithemoides var disputata")
taxontable <- append_to_tt("Nitzschia sp.","Nitzschia sp")
taxontable <- append_to_tt("Pinnularia sp.","Pinnularia sp")
taxontable <- append_to_tt("Staurosira construens var.exigua","Staurosira construens var exigua")

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

taxontable[,clean := TRUE]

taxontable <- new_genus(ori = "Nitzschia incurva var lorenziana",
                        fix = "Nitzschia incurva",
                        spe = "Nitzschia incurva",
                        gen = "Nitzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Syndera famelica",
                        fix = "Syndera famelica",
                        spe = "Syndera famelica",
                        gen = "Syndera",
                        fam = "Fragilariaceae",
                        ord = "Fragilariales", 
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")


## check that TU is empty now
TU <- setdiff(TU, taxontable$original_name)
saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))