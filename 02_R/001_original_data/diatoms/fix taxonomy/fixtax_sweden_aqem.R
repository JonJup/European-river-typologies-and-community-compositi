# - what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name))

taxontable <- append_to_tt("Achnanthes minutissima var. saprophila", "Achnanthes cf. minutissima var. saprophila Kobayasi & Mayama")
taxontable <- append_to_tt("Achnanthes pseudoswazi", "Achnanthes cf. pseudoswazi Carter")
taxontable <- append_to_tt("Achnanthes minutissima", "Achnanthes minutissima-grupp")
taxontable <- append_to_tt("Achnanthes minutissima", "Achnanthes minutissima (långsmal)")
taxontable <- append_to_tt("Achnanthes minutissima var. saprophila", "Achnanthes minutissima inkl. var. saprophila Kobayasi & Mayama")
taxontable <- append_to_tt("Achnanthes sp.", "Achnanthes spp.")
taxontable <- append_to_tt("Aulacoseira crassipunctata", "Aulacoseira crassipunctata Krammer")
taxontable <- append_to_tt("Aulacoseira sp.", "Aulacoseira species")
taxontable <- append_to_tt("Cocconeis placentula var. placentula", "Cocconeis placentula var. placentula Ehrenberg")
taxontable <- append_to_tt("Cocconeis placentula var. pseudolineata", "Cocconeis placentula var. pseudolineata Geitler")
taxontable <- append_to_tt("Cyclotella distinguenda var.unipunctata", "Cyclotella distinguenda unipunctata")
taxontable <- append_to_tt("Cyclotella distinguenda var.mesoleia", "Cyclotella distinguenda var. mesoleia (Grunow) Håkansson")
taxontable <- append_to_tt("Cyclotella sp", "Cyclotella spp.")
taxontable <- append_to_tt("Cymbella affiniformis", "Cymbella affinis Kützing")
taxontable <- append_to_tt("Cymbella caespitosa", "Cymbella caespitosa (Kützing) Brun")
taxontable <- append_to_tt("Cymbella cymbiformis", "Cymbella cymbiformis Agardh")
taxontable <- append_to_tt("Cymbella delicatula", "Cymbella delicatula Kützing")
taxontable <- append_to_tt("Cymbella gaeumannii", "Cymbella gaeumannii Meister")
taxontable <- append_to_tt("Cymbella helvetica", "Cymbella helvetica Kützing")
taxontable <- append_to_tt("Cymbella incerta", "Cymbella incerta (Grunow) Cleve")
taxontable <- append_to_tt("Cymbella minuta", "Cymbella minuta Hilse")
taxontable <- append_to_tt("Cymbella naviculiformis", "Cymbella naviculiformis (Auerswald) Cleve")
taxontable <- append_to_tt("Cymbella silesiaca", "Cymbella silesiaca Bleisch")
taxontable <- append_to_tt("Cymbella sinuata", "Cymbella sinuata Gregory")
taxontable <- append_to_tt("Cymbella sp.", "Cymbella spp.")
taxontable <- append_to_tt("Diploneis petersenii", "Diploneis peterseni")
taxontable <- append_to_tt("Encyonopsis", "Encyonopsis sp.")
taxontable <- append_to_tt("Eunotia bilunaris var.linearis", "Eunotia bilunaris var. linearis")
taxontable <- append_to_tt("Eunotia meisteri", "Eunotia cf. meisteri Hustedt")
taxontable <- append_to_tt("Eunotia naegelii", "Eunotia naegeli")
taxontable <- append_to_tt("Eunotia pectinalis var.ventralis", "Eunotia pectinalis var. ventralis")
taxontable <- append_to_tt("Eunotia zazuminensis", "Eunotia zasuminensis")
taxontable <- append_to_tt("Fragilaria capucina Gruppe", "Fragilaria capucina group")
taxontable <- append_to_tt("Fragilaria capucina var. gracilis", "Fragilaria capucina var. gracilis (missbildade)")
taxontable <- append_to_tt("Gomphonema clavatum", "Gomphonema clavatum s.l.")
taxontable <- append_to_tt("Gomphonema parvulum var. exilissimum", "Gomphonema parvulum var. exilissimum Grunow")
taxontable <- append_to_tt("Gomphonema pumilum Gruppe", "Gomphonema pumilum group")
taxontable <- append_to_tt("Gomphonema sp.", "Gomphonema spp.")
taxontable <- append_to_tt("Navicula accomoda", "Navicula accomoda Hustedt")
taxontable <- append_to_tt("Navicula bryophila", "Navicula bryophila Petersen")
taxontable <- append_to_tt("Navicula capitata", "Navicula capitata Ehrenberg")
taxontable <- append_to_tt("Navicula costulata", "Navicula costulata Grunow")
taxontable <- append_to_tt("Navicula gallica var. perpusilla", "Navicula gallica var. perpusilla (Grunow) Lange-Bertalot")
taxontable <- append_to_tt("Navicula mutica var. mutica", "Navicula mutica var. mutica Kützing")
taxontable <- append_to_tt("Navicula mutica var. ventricosa", "Navicula mutica var. ventricosa (Kützing) Cleve & Grunow")
taxontable <- append_to_tt("Navicula pseudoscutiformis", "Navicula pseudoscutiformis Hustedt")
taxontable <- append_to_tt("Navicula soehrensis var. muscicola", "Navicula soehrensis var. muscicola (Petersen) Krasske")
taxontable <- append_to_tt("Navicula soehrensis var. soehrensis", "Navicula soehrensis var. soehrensis Krasske")
taxontable <- append_to_tt("Navicula sp.", "Navicula spp.")
taxontable <- append_to_tt("Nitzschia dissipata ssp. dissipata", "Nitzschia dissipata var. dissipata")
taxontable <- append_to_tt("Pinnularia sp.", "Pinnularia spp.")
taxontable <- append_to_tt("Pinnularia viridiformis var. viridiformis", "Pinnularia viridiformis var. viridiformis morphotype 1")
taxontable <- append_to_tt("Stauroneis anceps", "Stauroneis anceps s.l.")
taxontable <- append_to_tt("Stauroneis nobilis", "Stauroneis nobilis Schumann")

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
(TU <- setdiff(TU, taxontable$original_name))


taxontable = new_entry("Achnanthes alpestris (Brun) Lange-Bertalot & Metzeltin", fix = "Eucocconeis laevis/alpestris", spe = "Eucocconeis laevis/alpestris", gen = "Eucocconeis")
taxontable = new_entry("Achnanthes depressa (Cleve) Hustedt", fix = "Eucocconeis", spe = NA, gen = "Eucocconeis")
taxontable = new_entry("Achnanthes lanceolata var. abbreviata", fix = "Planothidium pseudotanense", spe = "Planothidium pseudotanense", gen = "Planothidium")
taxontable = new_entry("Achnanthes stolida  Krasske  alt. Navicula schmassmannii Hustedt (jfr 2/4, T24)", fix = "Achnanthes acares/ricula/carissima", spe = "Achnanthes acares/ricula/carissima", gen = "Achnanthes")
taxontable = new_entry("Brachysira species", fix = "Brachysira", spe = NA, gen = "Brachysira")
taxontable = new_entry("Cocconeis placentula var. euglypta (Ehrenberg) Grunow + var. lineata", fix = "Cocconeis placentula", spe = "Cocconeis placentula", gen = "Cocconeis")
taxontable = new_entry("Cymbella cesatii (Rabenhorst) Grunow", fix = "Encyonopsis cesatii", spe = "Encyonopsis cesatii", gen = "Encyonopsis")
taxontable = new_entry("Cymbella descripta (Hustedt) Krammer & Lange-Bertalot", fix = "Encyonopsis descripta/falaisensis/microcephala", spe = "Encyonopsis descripta/falaisensis/microcephala", gen = "Encyonopsis")
taxontable = new_entry("Cymbella falaisensis (Grunow) Krammer & Lange-Bertalot", fix = "Encyonopsis descripta/falaisensis/microcephala", spe = "Encyonopsis descripta/falaisensis/microcephala", gen = "Encyonopsis")
taxontable = new_entry("Cymbella gracilis (Ehrenberg) Kützing", fix = "Encyonema neogracile", spe = "Encyonema neogracile", gen = "Encyonema")
taxontable = new_entry("Eunotia species", fix = "Eunotia", spe = NA, gen = "Eunotia")
taxontable = new_entry("Fragilaria dilatata (Brébisson) Lange-Bertalot", fix = "Ulnaria ulna complex", spe = "Ulnaria ulna complex", gen = "Ulnaria")
taxontable = new_entry("Fragilaria ulna f. angustissima", fix = "Ulnaria ulna complex", spe = "Ulnaria ulna complex", gen = "Ulnaria")
taxontable = new_entry("Navicula soehrensis var. hassiaca (Krasske) Lange-Bertalot", fix = "Chamaepinnularia hassiaca", spe = "Chamaepinnularia hassiaca", gen = "Chamaepinnularia")
taxontable = new_entry("Nitzschia cf. palea (Kützing) W. Smith", fix = "Nitzschia palea-paleacea", spe = "Nitzschia palea-paleacea", gen = "Nitzschia")
taxontable = new_entry("Nitzschia cf. perminuta (Grunow) M. Peragallo", fix = "Nitzschia perminuta Complex", spe = "Nitzschia perminuta Complex", gen = "Nitzschia")
taxontable = new_entry("Pinnularia mesolepta (Ehrenberg) W. Smith Morphotyp 5", fix = "Pinnularia mesolepta Complex", spe = "Pinnularia mesolepta Complex", gen = "Pinnularia")
taxontable = new_entry("Stauroneis anceps var. hyalina Peragallo & Brun", fix = "Stauroneis anceps Complex", spe = "Stauroneis anceps Complex", gen = "Stauroneis")
taxontable = new_entry("Stauroneis anceps var. siberica Grunow", fix = "Stauroneis anceps Complex", spe = "Stauroneis anceps Complex", gen = "Stauroneis")


taxontable[original_name == "Achnanthes stolida", c("fixed_name", "species") :=
                   .("Achnanthes acares/ricula/carissima", "Achnanthes acares/ricula/carissima")]
taxontable[original_name == "Cymbella cesatii var.paradoxa", c("fixed_name", "species", "genus") :=
                   .("Encyonopsis cesatii", "Encyonopsis cesatii", "Encyonopsis")]
taxontable[original_name == "Cymbella falaisensis var.lanceola", c("fixed_name", "species", "genus") :=
                   .("Encyonopsis descripta/falaisensis/microcephala", "Encyonopsis descripta/falaisensis/microcephala", "Encyonopsis")]
taxontable[original_name == "Pinnularia mesolepta", c("fixed_name", "species") :=
                   .("Pinnularia mesolepta Complex", "Pinnularia mesolepta Complex")]

check_taxon_table(taxontable)
taxontable <- taxontable[!duplicated(taxontable$original_name)]

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))