## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

taxontable <- append_to_tt("Achnanthes biasolettiana var. biasolettiana","Achnanthes biasolettiana var. Biasolettiana")
taxontable <- append_to_tt("Achnanthes lanceolata var.frequentissima var.magna","Achnanthes lanceolata ssp. frequentissima var. magna")
taxontable <- append_to_tt("Aulacoseira granulata Morphotyp curvata","Aulacoseira granulata var. curvata")
taxontable <- append_to_tt("Caloneis schumanniana var.biconstricta","Caloneis schumanniana var. biconstricta")
taxontable <- append_to_tt("Fragilaria ulna acus-Sippen","Fragilaria ulna acus - Sippen")
taxontable <- append_to_tt("Navicula soehrensis var.muscicola","Navicula soehrensis var. muscicola")
taxontable <- append_to_tt("Navicula viridula var.linearis","Navicula viridula var. linearis")

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

## ok? Achnanthes lanceolata

taxontable[species == "Achnanthes lanceolata", c("fixed_name", "species", "genus", "family") := 
                   .("Planothidium lanceolatum", "Planothidium lanceolatum", "Planothidium", "Achnanthidiaceae")]


TU <- setdiff(TU, taxontable$original_name)

taxontable <- new_entry(ori = "Achnanthes minutissima var. jackii"     , fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Cymbella amphicephala var. amphicephala" , fix = "Cymbopleura amphicephala-naviculiformis-anglica", spe = "Cymbopleura amphicephala-naviculiformis-anglica", gen = "Cymbopleura")
taxontable <- new_entry(ori = "Cymbella perpusilla var. striatior"     , fix = "Encyonema gaeumannii/perpusillum", spe = "Encyonema gaeumannii/perpusillum", gen = "Encyonema")
taxontable <- new_entry(ori = "Fragilaria leptostauron var. martyi"    , fix = "Staurosirella leptostauron complex", spe = "Staurosirella leptostauron complex", gen = "Staurosirella")
taxontable <- new_entry(ori = "Gomphonema lingulatiforme"              , fix = "Gomphosphenia lingulatiformis", spe = "Gomphosphenia lingulatiformis", gen = "Gomphosphenia")
taxontable <- new_entry(ori = "Navicula ignota var. ignota"            , fix = "Geissleria ignota complex", spe = "Geissleria ignota complex", gen = "Geissleria")
taxontable <- new_entry(ori = "Navicula radiosa var. tenella"          , fix = "Navicula cryptotenella/cryptotenelloides", spe = "Navicula cryptotenella/cryptotenelloides", gen = "Navicula")
taxontable <- new_entry(ori = "Navicula soehrensis var. soehrensis"    , fix = "Chamaepinnularia soehrensis Complex", spe = "Chamaepinnularia soehrensis Complex", gen = "Chamaepinnularia")
taxontable <- new_entry(ori = "Nitzschia compressa var. balatonis" , fix = "Tryblionella balatonis", spe = "Tryblionella balatonis", gen = "Tryblionella")

taxontable[genus == "Staurosirella", family := "Fragilariaceae"]

taxontable[species == "Achnanthidium minutissimum var. jackii", c("fixed_name", "species") := 
                   .("Achnanthidium minutissimum", "Achnanthidium minutissimum")]
taxontable[original_name %in% c(
        "Fragilaria leptostauron", 
        "Fragilaria leptostauron var. dubia",
        "Fragilaria leptostauron var. leptostauron"
), c("fixed_name", "species", "genus", "family") := 
        .(
                "Staurosirella leptostauron complex", 
                "Staurosirella leptostauron complex", 
                "Staurosirella", 
                "Fragilariaceae")
]

TU <- setdiff(unique(data$taxon), taxontable$original_name)

## check that the taxontable is ok (no duplicates and properly nested)
check_taxon_table(taxontable)

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))
