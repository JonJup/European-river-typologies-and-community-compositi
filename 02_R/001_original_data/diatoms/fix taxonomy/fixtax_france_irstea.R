## -- fix diatom taxonomy for IRSTEA diatoms france


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

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

## open taxontable and dia1 

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

taxontable[fixed_name == "Cymbella affinis var.procera", c("fixed_name", "species") := .("Cymbella affinis", "Cymbella affinis")]
taxontable[fixed_name == "Cymbella affinis Kützing", c("fixed_name", "species") := .("Cymbella affinis", "Cymbella affinis")]
taxontable[fixed_name == "Cymbella kolbei var.angusta", c("fixed_name", "species") := .("Cymbella kolbei", "Cymbella kolbei")]
taxontable[fixed_name == "Cymbella neoleptoceros", c("fixed_name", "species") := .("Cymbella", NA)]
taxontable[fixed_name == "Cymbopleura kuelbsii var.nonfasciata", c("fixed_name", "species") := .("Cymbopleura kuelbsii", "Cymbopleura kuelbsii")]
taxontable[fixed_name == "Aulacoseira granulata var.angustissima", c("fixed_name", "species") := .("Aulacoseira granulata", "Aulacoseira granulata")]
taxontable[fixed_name == "Cavinula lapidosa", c("fixed_name", "species") := .("Cavinula", NA)]
taxontable[fixed_name == "Cavinula variostriata", c("fixed_name", "species") := .("Cavinula", NA)]
taxontable[fixed_name == "Chamaepinnularia evanida", c("fixed_name", "species") := .("Chamaepinnularia", NA)]
taxontable[fixed_name == "Chamaepinnularia mediocris", c("fixed_name", "species") := .("Chamaepinnularia begeri/mediocris", "Chamaepinnularia begeri/mediocris")]
taxontable[fixed_name %in% c("Craticula riparia", "Craticula Complex"), c("fixed_name", "species") := .("Craticula", NA)]
taxontable[fixed_name == "Eunotia curtagrunowii", c("fixed_name", "species") := . ("Eunotia", NA)]
taxontable[fixed_name == "Eunotia formica", c("fixed_name", "species") := .("Eunotia", NA)]
taxontable[fixed_name == "Fallacia insociabilis", c("fixed_name", "species") := .("Fallacia insociabilis complex", "Fallacia insociabilis complex")]
taxontable[fixed_name == "Halamphora normanii", c("fixed_name", "species") := .("Amphora ocellata", "Amphora ocellata") ]
taxontable[fixed_name == "Karayevia suchlandtii", c("fixed_name", "species"):=.("Karayevia amoena/nitidiformis", "Karayevia amoena/nitidiformis")]
taxontable[fixed_name == "Mayamaea fossalis var.obsidialis", c("fixed_name", "species") := .("Mayamaea fossalis", "Mayamaea fossalis")]
taxontable[fixed_name == "Naviculadicta pseudoventralis", c("fixed_name", "species", "genus") := .("Navicula ventralis-medioconvexa++", "Navicula ventralis-medioconvexa++", "Navicula")]
taxontable[fixed_name %in% c("Nitzschia compressa var.elongata", "Nitzschia compressa var.vexans", "Nitzschia compressa"), c("fixed_name", "species") := .("Tryblionella punctata", "Tryblionella punctata")]
taxontable[fixed_name == "Nitzschia fonticola", c("fixed_name", "species") := .("Nitzschia fonticola Complex", "Nitzschia fonticola Complex")]
taxontable[fixed_name == "Pinnularia borealis", c("fixed_name", "species") := .("Pinnularia alpina-lata-borealis complex", "Pinnularia alpina-lata-borealis complex")]
taxontable[fixed_name == "Pinnularia nodosa", c("fixed_name", "species") := .("Pinnularia nodosa Complex", "Pinnularia nodosa Complex")]
taxontable[fixed_name == "Psammothidium helveticum", c("fixed_name", "species") := .("Psammothidium helveticum/chlidanos/daonense", "Psammothidium helveticum/chlidanos/daonense")]
taxontable[fixed_name == "Sellaphora verecundiae", c("fixed_name", "species") := .("Navicula vitabunda-modica-pexa", "Navicula vitabunda-modica-pexa")]
taxontable[fixed_name == "Staurosira robusta", c("fixed_name", "species") := .("Staurosira binodis-robusta", "Staurosira binodis-robusta")]


TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

taxontable <- new_entry(ori ="Achnanthes sp. f. anormale" , fix = "Achnanthes", spe = NA, gen = "Achnanthes")
taxontable <- new_entry(ori ="Achnanthidium exiguum (Grunow)Czarnecki var.elliptica Hustedt" , fix = "Achnanthidium exigua/ziegleri/subexigua", spe = "Achnanthidium exigua/ziegleri/subexigua", gen = "Achnanthidium")
taxontable <- new_entry(ori ="Achnanthidium exile (Kützing) Heiberg" , fix = "Achnanthidium exile", spe = "Achnanthidium exile", gen = "Achnanthidium")
taxontable <- new_entry(ori ="Achnanthidium thienemannii (Hustedt) Lange-Bertalot" , fix = "Achnanthidium thienemannii", spe = "Achnanthidium thienemannii", gen = "Achnanthidium")
taxontable <- new_entry(ori ="Achnanthidium trinode Ralfs in Pritchard" , fix = "Achnanthidium trinode", spe = "Achnanthidium trinode", gen = "Achnanthidium")
taxontable <- new_entry(ori ="Actinocyclus normanii(Greg. ex Grev.) Hustedt morphotype normanii" , fix = "Actinocyclus normanii", spe = "Actinocyclus normanii", gen = "Actinocyclus")
taxontable <- new_entry(ori ="Adlafia parabryophila (Lange-Bertalot) Moser Lange-Bertalot & Metzeltin" , fix = "Adlafia parabryophila", spe = "Adlafia parabryophila", gen = "Adlafia")
taxontable <- new_entry(ori ="Adlafia suchlandtii (Hustedt) Monnier & Ector" , fix = "Adlafia", spe = NA, gen = "Adlafia")
taxontable <- new_entry(ori ="Aulacoseira alpigena(Grunow) Krammer" , fix = "Aulacoseira distans complex", spe = "Aulacoseira distans complex", gen = "Aulacoseira")
taxontable <- new_entry(ori ="Aulacoseira granulata (Ehr.) Simonsen f.curvata (Hustedt) Simonsen" , fix = "Aulacoseira granulata", spe = "Aulacoseira granulata", gen = "Aulacoseira")
taxontable <- new_entry(ori ="Aulacoseira islandica(O.Müller)Simonsen" , fix = "Aulacoseira islandica", spe = "Aulacoseira islandica", gen = "Aulacoseira")
taxontable <- new_entry(ori ="Aulacoseira italica (Ehr.)Simonsen var.tenuissima (Grun.) Simonsen" , fix = "Aulacoseira italica complex", spe = "Aulacoseira italica complex", gen = "Aulacoseira")
taxontable <- new_entry(ori ="Aulacoseira italica (Ehrenb.)Simonsen" , fix = "Aulacoseira italica complex", spe = "Aulacoseira italica complex", gen = "Aulacoseira")
taxontable <- new_entry(ori ="Aulacoseira sphaerica (Heribaud) Simonsen" , fix = "Aulacoseira sphaerica", spe = "Aulacoseira sphaerica", gen = "Aulacoseira")
taxontable <- new_entry(ori ="Berkeleya rutilans (Trentepohl) Grunow in Cleve & Grunow" , fix = "Berkeleya rutilans", spe = "Berkeleya rutilans", gen = "Berkeleya")
taxontable <- new_entry(ori ="Brachysira garrensis (Lange-Bertalot & Krammer) Lange-Bertalot" , fix = "Brachysira garrensis", spe = "Brachysira garrensis", gen = "Brachysira")
taxontable <- new_entry(ori ="Brachysira intermedia (Oestrup)Lange-Bertalot" , fix = "Brachysira intermedia", spe = "Brachysira intermedia", gen = "Brachysira")
taxontable <- new_entry(ori ="Brachysira zellensis (Grunow) Round & Mann" , fix = "Brachysira zellensis", spe = "Brachysira zellensis", gen = "Brachysira")
taxontable <- new_entry(ori ="Caloneis alpestris (Grunow)Cleve" , fix = "Caloneis", spe = NA, gen = "Caloneis")
taxontable <- new_entry(ori ="Caloneis fontinalis (Grunow in Van Heurck) Cleve-Euler" , fix = "Caloneis fontinalis", spe = "Caloneis fontinalis", gen = "Caloneis")
taxontable <- new_entry(ori ="Caloneis lancettula (Schulz) Lange-Bertalot & Witkowski" , fix = "Caloneis lancettula", spe = "Caloneis lancettula", gen = "Caloneis")
taxontable <- new_entry(ori ="Caloneis schumanniana (Grunow in Van Heurck) Cleve" , fix = "Caloneis", spe = NA, gen = "Caloneis")
taxontable <- new_entry(ori ="Caloneis thermalis(Grunow) Krammer" , fix = "Caloneis", spe = NA, gen = "Caloneis")
taxontable <- new_entry(ori ="Cavinula lapidosa (Krasske) Lange-Bertalot" , fix = "Cavinula", spe = NA, gen = "Cavinula")
taxontable <- new_entry(ori ="Cavinula variostriata (Krasske) Mann & Stickle in Round Crawford  & Mann" , fix = "Cavinula", spe = NA, gen = "Cavinula")
taxontable <- new_entry(ori ="Chamaepinnularia evanida (Hustedt) Lange-Bertalot" , fix = "Chamaepinnularia", spe = NA, gen = "Chamaepinnularia")
taxontable <- new_entry(ori ="Chamaepinnularia mediocris (Krasske) Lange-Bertalot in Lange-Bertalot & Metzeltin" , fix = "Chamaepinnularia begeri/mediocris", spe = "Chamaepinnularia begeri/mediocris", gen = "Chamaepinnularia")
taxontable <- new_entry(ori ="Chamaepinnularia muscicola (Petersen) Kulikovskiy, Lange-Bertalot & Witkowski" , fix = "Chamaepinnularia soehrensis Complex", spe = "Chamaepinnularia soehrensis Complex", gen = "Chamaepinnularia")
taxontable <- new_entry(ori ="Chamaepinnularia submuscicola (Krasske) Lange-Bertalot" , fix = "Chamaepinnularia submuscicola", spe = "Chamaepinnularia submuscicola", gen = "Chamaepinnularia")
taxontable <- new_entry(ori ="Craticula dissociata (Reichardt) Reichardt" , fix = "Craticula dissociata", spe = "Craticula dissociata", gen = "Craticula")
taxontable <- new_entry(ori ="Craticula molesta (Krasske) Lange-Bertalot & Willmann in Lange-Bertalot & al." , fix = "Craticula zizix", spe = "Craticula zizix", gen = "Craticula")
taxontable <- new_entry(ori ="Craticula riparia (Hustedt) Lange-Bertalot var.riparia" , fix = "Craticula", spe = NA, gen = "Craticula")
taxontable <- new_entry(ori ="Craticula riparia (Hustedt)Lange-Bertalot var. mollenhaueri Lange-Bertalot" , fix = "Craticula", spe = NA, gen = "Craticula")
taxontable <- new_entry(ori ="Craticula submolesta (Hust.) Lange-Bertalot" , fix = "Craticula", spe = NA, gen = "Craticula")
taxontable <- new_entry(ori ="Cyclostephanos costatilimbus(Kobayasi & Kob.) Stoermer Håkansson & Theriot" , fix = "Cyclostephanos costatilimbus", spe = "Cyclostephanos costatilimbus", gen = "Cyclostephanos")
taxontable <- new_entry(ori ="Cymatopleura solea (Brébisson in Brébisson & Godey) W.Smith var.apiculata (W.Smith) Ralfs in Pritchard" , fix = "Cymatopleura solea", spe = "Cymatopleura solea", gen = "Cymatopleura")
taxontable <- new_entry(ori ="Cymbopleura budayana (Pantocsek) Krammer" , fix = "Cymbopleura", spe = NA, gen = "Cymbopleura")
taxontable <- new_entry(ori ="Cymbopleura inaequalis (Ehrenberg) Krammer" , fix = "Cymbella cistula group", spe = "Cymbella cistula group", gen = "Cymbella")
taxontable <- new_entry(ori ="Denticula mesolepta (Grunow in Van Heurck) Meister" , fix = "Denticula", spe = NA, gen = "Denticula")
taxontable <- new_entry(ori ="Diploneis fontium Reichardt & Lange-Bertalot in Reichardt" , fix = "Diploneis fontium", spe = "Diploneis fontium", gen = "Diploneis")
taxontable <- new_entry(ori ="Diploneis parma Cleve sensu Krammer & Lange-Bertalot" , fix = "Diploneis elliptica/oblongella/ovalis/marginestrata/minuta/modica", spe = "Diploneis elliptica/oblongella/ovalis/marginestrata/minuta/modica", gen = "Diploneis")
taxontable <- new_entry(ori ="Encyonema elginense (Krammer) D.G. Mann in Round Crawford & Mann" , fix = "Encyonema silesicacum/minutum/lange-bertalotii", spe = "Encyonema silesicacum/minutum/lange-bertalotii", gen = "Encyonema")
taxontable <- new_entry(ori ="Encyonema perpusillum (A. Cleve) D.G. Mann" , fix = "Encyonema gaeumannii/perpusillum", spe = "Encyonema gaeumannii/perpusillum", gen = "Encyonema")
taxontable <- new_entry(ori ="Encyonopsis aequalis (W.Smith) Krammer" , fix = "Cymbopleura angustata-descripta++", spe = "Cymbopleura angustata-descripta++", gen = "Cymbopleura")
taxontable <- new_entry(ori ="Encyonopsis falaisensis (Grunow) Krammer" , fix = "Encyonopsis descripta/falaisensis/microcephala", spe = "Encyonopsis descripta/falaisensis/microcephala", gen = "Encyonopsis")
taxontable <- new_entry(ori ="Encyonopsis muscicola (Schoeman) Krammer" , fix = "Encyonopsis muscicola", spe = "Encyonopsis muscicola", gen = "Encyonopsis")
taxontable <- new_entry(ori ="Eolimna muraloides (Hustedt) Lange-Bertalot & Kulikovskiy" , fix = "Navicula soodensis-muraloides", spe = "Navicula soodensis-muraloides", gen = "Navicula")
taxontable <- new_entry(ori ="Eolimna raederae (Lange-Bertalot) Lange-Bertalot & Kulikovskiy" , fix = "Sellaphora raederae", spe = "Sellaphora raederae", gen = "Sellaphora")
taxontable <- new_entry(ori ="Eolimna rotunda (Hustedt) Lange-Bertalot & Kulikovskiy" , fix = "Sellaphora rotunda", spe = "Sellaphora rotunda", gen = "Sellaphora")
taxontable <- new_entry(ori ="Eolimna schaumburgii (Lange-Bertalot & Hofmann) Lange-Bertalot & Kulikovskiy" , fix = "Sellaphora schaumburgii", spe = "Sellaphora schaumburgii", gen = "Sellaphora")
taxontable <- new_entry(ori ="Eolimna silvahercynia (Lange-Bertalot) Lange-Bertalot" , fix = "Eolimna silvahercynia", spe = "Eolimna silvahercynia", gen = "Eolimna")
taxontable <- new_entry(ori ="Eolimna submuralis (Hustedt) Lange-Bertalot & Kulikovskiy" , fix = "Sellaphora submuralis Complex", spe = "Sellaphora submuralis Complex", gen = "Sellaphora")
taxontable <- new_entry(ori ="Eolimna utermoehlii (Hustedt) Lange-Bertalot, Kulikovskiy & Witkowski" , fix = "Sellaphora utermoehlii", spe = "Sellaphora utermoehlii", gen = "Sellaphora")
taxontable <- new_entry(ori ="Epithemia ocellata (Ehrenberg) Kützing" , fix = "Epithemia argus", spe = "Epithemia argus", gen = "Epithemia")
taxontable <- new_entry(ori ="Eucocconeis diluviana (Hustedt) Lange-Bertalot" , fix = "Eucocconeis diluviana", spe = "Eucocconeis diluviana", gen = "Eucocconeis")
taxontable <- new_entry(ori ="Eunotia arcubus Nörpel-Schempp & Lange-Bertalot" , fix = "Eunotia", spe = NA, gen = "Eunotia")
taxontable <- new_entry(ori ="Eunotia arculus Lange-Bertalot & Nörpel in Lange-Bertalot" , fix = "Eunotia arcus/mucophila/bilunaris Complex", spe = "Eunotia arcus/mucophila/bilunaris Complex", gen = "Eunotia")
taxontable <- new_entry(ori ="Eunotia botuliformis Wild, Nörpel-Schempp & Lange-Bertalot" , fix = "Eunotia", spe = NA, gen = "Eunotia")
taxontable <- new_entry(ori ="Eunotia curtagrunowii Nörpel-Schempp & Lange-Bertalot" , fix = "Eunotia", spe = NA, gen = "Eunotia")
taxontable <- new_entry(ori ="Eunotia formica Ehrenberg sensu stricto" , fix = "Eunotia", spe = NA, gen = "Eunotia")
taxontable <- new_entry(ori ="Eunotia pectinalis(Kütz.)Rabenhorst var.recta A.Mayer ex Patrick" , fix = "Eunotia pectinalis Complex", spe = "Eunotia pectinalis Complex", gen = "Eunotia")
taxontable <- new_entry(ori ="Eunotia silvahercynia Nörpel Van Sull & Lange-Bertalot in Alles & al." , fix = "Eunotia septentrionalis", spe = "Eunotia septentrionalis", gen = "Eunotia")
taxontable <- new_entry(ori ="Eunotia tenella (Grunow in Van Heurck) Hustedt in Schmidt & al" , fix = "Eunotia  exigua/elegans Complex", spe = "Eunotia  exigua/elegans Complex", gen = "Eunotia")
taxontable <- new_entry(ori ="Fallacia indifferens (Hustedt) D.G. Mann" , fix = "Navicula vitiosa-indifferens-kuelbsii++", spe = "Navicula vitiosa-indifferens-kuelbsii++", gen = "Navicula")
taxontable <- new_entry(ori ="Fallacia insociabilis (Krasske) D.G. Mann" , fix = "Fallacia insociabilis complex", spe = "Fallacia insociabilis complex", gen = "Fallacia")
taxontable <- new_entry(ori ="Fallacia lucinensis (Hustedt) D.G. Mann" , fix = "Fallacia insociabilis complex", spe = "Fallacia insociabilis complex", gen = "Fallacia")
taxontable <- new_entry(ori ="Fallacia omissa (Hustedt) D.G. Mann in Round Crawford & Mann" , fix = "Fallacia omissa", spe = "Fallacia omissa", gen = "Fallacia")
taxontable <- new_entry(ori ="Fallacia pygmaea ssp.subpygmaea Lange-Bertalot Cavacini Tagliaventi & Alfinito" , fix = "Fallacia pygmaea-forcipata", spe = "Fallacia pygmaea-forcipata", gen = "Fallacia")
taxontable <- new_entry(ori ="Fragilaria amphicephaloides Lange-Bertalot in Hofmann & al." , fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry(ori ="Fragilaria capucina Desm. var.septentrionalis (Oestrup) Lange-Bertalot" , fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry(ori ="Fragilaria capucina Desmazieres f. anormale" , fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry(ori ="Fragilaria radians Lange-Bertalot in Hofmann & al." , fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry(ori ="Fragilaria vaucheriae (Kützing) Petersen" , fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry(ori ="Geissleria paludosa (Hustedt) Lange-Bertalot & Metzeltin" , fix = "Geissleria", spe = NA, gen = "Geissleria")
taxontable <- new_entry(ori ="Genkalia digitulus (Hustedt) Lange-Bertalot & Kulikovskiy" , fix = "Genkalia digitulus", spe = "Genkalia digitulus", gen = "Genkalia")
taxontable <- new_entry(ori ="Gomphoneis calcifuga (Lange-Bertalot & Reichardt)Tuji" , fix = "Gomphoneis calcifuga", spe = "Gomphoneis calcifuga", gen = "Gomphoneis")
taxontable <- new_entry(ori ="Gomphonema elegantissimum Reichardt & Lange-Bertalot in Hofmann & al." , fix = "Gomphonema elegantissimum", spe = "Gomphonema elegantissimum", gen = "Gomphonema")
taxontable <- new_entry(ori ="Gomphonema parvulum Kützing f. anormale" , fix = "Gomphonema parvulum Complex", spe = "Gomphonema parvulum Complex", gen = "Gomphonema")
taxontable <- new_entry(ori ="Gomphonema saprophilum (Lange-Bertalot & Reichardt) Abarca Jahn Zimmermann & Enke" , fix = "Gomphonema saprophilum", spe = "Gomphonema saprophilum", gen = "Gomphonema")
taxontable <- new_entry(ori ="Gomphonema tergestinum (Grunow in Van Heurck) Schmidt in Schmidt & al." , fix = "Gomphonema parvulum Complex", spe = "Gomphonema parvulum Complex", gen = "Gomphonema")
taxontable <- new_entry(ori ="Gomphosphenia grovei (M.Schmidt) Lange-Bertalot" , fix = "Gomphosphenia", spe = NA, gen = "Gomphosphenia")
taxontable <- new_entry(ori ="Gomphosphenia grovei M.Schmidt var.lingulata (Hustedt) Lange-Bertalot" , fix = "Gomphosphenia", spe = NA, gen = "Gomphosphenia")
taxontable <- new_entry(ori ="Gomphosphenia holmquistii (Foged) Lange-Bertalot" , fix = "Gomphosphenia holmquistii", spe = "Gomphosphenia holmquistii", gen = "Gomphosphenia")
taxontable <- new_entry(ori ="Gomphosphenia oahuensis (Hustedt) Lange-Bertalot" , fix = "Gomphosphenia oahuensis", spe = "Gomphosphenia oahuensis", gen = "Gomphosphenia")
taxontable <- new_entry(ori ="Gyrosigma kuetzingii (Grunow) Cleve" , fix = "Gyrosigma kuetzingii", spe = "Gyrosigma kuetzingii", gen = "Gyrosigma")
taxontable <- new_entry(ori ="Gyrosigma nodiferum (Grunow) Reimer f. anormale" , fix = "Gyrosigma sciotense", spe = "Gyrosigma sciotense", gen = "Gyrosigma")
taxontable <- new_entry(ori ="Halamphora normanii (Rabenhorst) Levkov" , fix = "Amphora ocellata", spe = "Amphora ocellata", gen = "Amphora")
taxontable <- new_entry(ori ="Halamphora submontana (Hustedt) Levkov" , fix = "Halamphora submontana", spe = "Halamphora submontana", gen = "Halamphora")
taxontable <- new_entry(ori ="Halamphora thumensis (A.Mayer) Levkov" , fix = "Halamphora thumensis", spe = "Halamphora thumensis", gen = "Halamphora")
taxontable <- new_entry(ori ="Handmannia bodanica (Eulenstein ex Grunow) Kociolek & Khursevich" , fix = "Lindavia bodanica complex", spe = "Lindavia bodanica complex", gen = "Lindavia")
taxontable <- new_entry(ori ="Handmannia comta (Ehrenberg) Kociolek & Khursevich emend. Genkal" , fix = "Lindavia comta", spe = "Lindavia comta", gen = "Lindavia")
taxontable <- new_entry(ori ="Hantzschia amphioxys (Ehr.) Grunow var.linearis (O.Müller) Cleve-Euler" , fix = "Hantzschia amphioxys", spe = "Hantzschia amphioxys", gen = "Hantzschia")
taxontable <- new_entry(ori ="Hippodonta avittata (Cholnoky) Lange-BertalotMetzeltin & Witkowski" , fix = "Hippodonta avittata", spe = "Hippodonta avittata", gen = "Hippodonta")
taxontable <- new_entry(ori ="Hippodonta subcostulata (Hustedt) Lange-Bertalot Metzeltin & Witkowski" , fix = "Hippondonta", spe = NA, gen = "Hippondonta")
taxontable <- new_entry(ori ="Humidophila brekkaensis (Petersen) Lowe, Kociolek, Johansen,Van deVijver, Lange-Bertalot & Kopalová" , fix = "Humidophila brekkaensis", spe = "Humidophila brekkaensis", gen = "Humidophila")
taxontable <- new_entry(ori ="Humidophila contenta (Grunow) Lowe, Kociolek, Johansen, Van de Vijver, Lange-Bertalot & Kopalová" , fix = "Humidophila contenta", spe = "Humidophila contenta", gen = "Humidophila")
taxontable <- new_entry(ori ="Humidophila perpusilla (Grunow) Lowe, Kociolek, Johansen,Van deVijver, Lange-Bertalot & Kopalová" , fix = "Humidophila perpusilla", spe = "Humidophila perpusilla", gen = "Humidophila")
taxontable <- new_entry(ori ="Karayevia clevei (Grun.) Bukht. var.rostrata (Hust.) Bukhtiyarova" , fix = "Karayevia clevei", spe = "Karayevia clevei", gen = "Karayevia")
taxontable <- new_entry(ori ="Karayevia ploenensis (Hustedt) Bukhtiyarova var. gessneri (Hust.) Bukhtiyarova" , fix = "Karayevia ploenensis", spe = "Karayevia ploenensis", gen = "Karayevia")
taxontable <- new_entry(ori ="Karayevia suchlandtii (Hustedt) Bukhtiyarova" , fix = "Karayevia amoena/nitidiformis", spe = "Karayevia amoena/nitidiformis", gen = "Karayevia")
taxontable <- new_entry(ori ="Lemnicola hungarica (Grunow) Round & Basson var.pusilla (Grunow) Bukhtiyarova" , fix = "Lemnicola hungarica", spe = "Lemnicola hungarica", gen = "Lemnicola")
taxontable <- new_entry(ori ="Luticola minor (R.M.Patrick) A. Mayama" , fix = "Luticola minor", spe = "Luticola minor", gen = "Luticola")
taxontable <- new_entry(ori ="Luticola peguana (Grunow in Cl. & Moeller) D.G. Mann" , fix = "Luticola peguana", spe = "Luticola peguana", gen = "Luticola")
taxontable <- new_entry(ori ="Luticola saxophila (Bock ex Hustedt) D.G. Mann in Round Crawford & Mann" , fix = "Luticola", spe = NA, gen = "Luticola")
taxontable <- new_entry(ori ="Luticola sp. (aff. mutica)" , fix = "Luticola", spe = NA, gen = "Luticola")
taxontable <- new_entry(ori ="Mayamaea asellus (Weinhold) Lange-Bertalot" , fix = "Mayamaea asellus", spe = "Mayamaea asellus", gen = "Mayamaea")
taxontable <- new_entry(ori ="Mayamaea excelsa (Krasske) Lange-Bertalot" , fix = "Mayamaea excelsa", spe = "Mayamaea excelsa", gen = "Mayamaea")
taxontable <- new_entry(ori ="Mayamaea fossalis var. obsidialis (Hustedt) Lange-Bertalot" , fix = "Mayamaea fossalis", spe = "Mayamaea fossalis", gen = "Mayamaea")
taxontable <- new_entry(ori ="Mayamaea ingenua (Hustedt) Lange-Bertalot & Hofmann in Hofmann & al." , fix = "Mayamaea ingenua", spe = "Mayamaea ingenua", gen = "Mayamaea")
taxontable <- new_entry(ori ="Melosira lineata (Dillwyn) Agardh" , fix = "Melosira lineata", spe = "Melosira lineata", gen = "Melosira")
taxontable <- new_entry(ori ="Navicula approximata Greville f. morei (O'Meara) Hustedt" , fix = "Lyrella approximata", spe = "Lyrella approximata", gen = "Lyrella")
taxontable <- new_entry(ori ="Navicula difficillima Hustedt" , fix = "Navicula arvensis-difficillima++", spe = "Navicula arvensis-difficillima++", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula erifuga Lange-Bertalot in Krammer & Lange-Bertalot" , fix = "Navicula cari-recens-erifuga++", spe = "Navicula cari-recens-erifuga++", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula escambia (Patrick) Metzeltin & Lange-Bertalot" , fix = "Navicula escambia", spe = "Navicula escambia", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula longa (Gregory) Ralfs in Pritchard" , fix = "Navicula longa", spe = "Navicula longa", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula medioconvexa Hustedt f.simplex Schimanski" , fix = "Navicula ventralis-medioconvexa++", spe = "Navicula ventralis-medioconvexa++", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula salinarum var. rostrata (Hustedt) Lange-Bertalot" , fix = "Navicula trivialis-salinarum", spe = "Navicula trivialis-salinarum", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula striolata (Grun.) Lange-Bertalot in Reichardt" , fix = "Navicula radiosa", spe = "Navicula radiosa", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula vaneei Lange-Bertalot in Witkowski Lange-Bertalot & Stachura" , fix = "Navicula", spe = NA, gen = "Navicula")
taxontable <- new_entry(ori ="Navicula vilaplanii (Lange-Bertalot & Sabater)Lange-Bertalot & Sabater in U.Rumrich Lange-Bertalot & M.Rumrich" , fix = "Navicula vilaplanii", spe = "Navicula vilaplanii", gen = "Navicula")
taxontable <- new_entry(ori ="Naviculadicta laterostrata (Hustedt) Lange-Bertalot in Lange-Bertalot & Moser" , fix = "Naviculadicta laterostrata", spe = "Naviculadicta laterostrata", gen = "Naviculadicta")
taxontable <- new_entry(ori ="Naviculadicta pseudoventralis (Hustedt) Lange-Bertalot in Lange-Bertalot & Moser" , fix = "Navicula ventralis-medioconvexa++", spe = "Navicula ventralis-medioconvexa++", gen = "Navicula")
taxontable <- new_entry(ori ="Naviculadicta seminulum (Grunow) Lange Bertalot f. anormale" , fix = "Naviculadicta seminulum", spe = "Naviculadicta seminulum", gen = "Naviculadicta")
taxontable <- new_entry(ori ="Naviculadicta vitabunda (Hustedt) Lange-Bertalot in Lange-Bertalot & Moser" , fix = "Naviculadicta vitabunda", spe = "Naviculadicta vitabunda", gen = "Naviculadicta")
taxontable <- new_entry(ori ="Navigiolum canoris (Hohn & Hellerman) Lange-Bertalot" , fix = "Navigiolum canoris", spe = "Navigiolum canoris", gen = "Navigiolum")
taxontable <- new_entry(ori ="Neidiomorpha binodeformis (Krammer) Lange-Bertalot & M. Cantonati" , fix = "Neidiomorpha binodeformis", spe = "Neidiomorpha binodeformis", gen = "Neidiomorpha")
taxontable <- new_entry(ori ="Neidiomorpha binodis (Ehrenberg) Lange-Bertalot & M. Cantonati" , fix = "Neidiomorpha binodis", spe = "Neidiomorpha binodis", gen = "Neidiomorpha")
taxontable <- new_entry(ori ="Neidium affine(Ehrenberg)Pfitzer" , fix = "Neidium affine Complex", spe = "Neidium affine Complex", gen = "Neidium")
taxontable <- new_entry(ori ="Neidium ampliatum (Ehrenberg) Krammer in Krammer & Lange-Bertalot" , fix = "Neidium ampliatum Complex", spe = "Neidium ampliatum Complex", gen = "Neidium")
taxontable <- new_entry(ori ="Neidium sp. in Metzeltin & Lange Bertalot" , fix = "Neidium", spe = NA, gen = "Neidium")
taxontable <- new_entry(ori ="Nitzschia acula Hantzsch ex Cleve & Grunow" , fix = "Nitzschia pura-linearis Complex", spe = "Nitzschia pura-linearis Complex", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia bremensis Hustedt in Schmidt & al." , fix = "Nitzschia bremensis", spe = "Nitzschia bremensis", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia brevissima Grunow in Van Heurck" , fix = "Nitzschia brevissima", spe = "Nitzschia brevissima", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia brunoi Lange-Bertalot in Lange-Bertalot & Metzeltin" , fix = "Tryblionella angustata Complex", spe = "Tryblionella angustata Complex", gen = "Tryblionella")
taxontable <- new_entry(ori ="Nitzschia compressa var.elongata (Grunow) Lange-Bertalot" , fix = "Tryblionella punctata", spe = "Tryblionella punctata", gen = "Tryblionella")
taxontable <- new_entry(ori ="Nitzschia dissipata(Kütz.)Grunow var.media (Hantzsch) Grunow in Van Heurck" , fix = "Nitzschia dissipata-recta Complex", spe = "Nitzschia dissipata-recta Complex", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia epithemoides Grunow var.disputata (Carter) Lange-Bertalot" , fix = "Nitzschia epithemoides", spe = "Nitzschia epithemoides", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia fonticola Grunow in Van Heurck" , fix = "Nitzschia fonticola Complex", spe = "Nitzschia fonticola Complex", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia fonticola Grunow var.pelagica Hustedt in Schmidt & al." , fix = "Nitzschia fonticola Complex", spe = "Nitzschia fonticola Complex", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia fossilis (Grunow) Grunow in Van Heurck" , fix = "Nitzschia fossilis", spe = "Nitzschia fossilis", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia labella Moser Lange-Bertalot & Metzeltin" , fix = "Nitzschia labella", spe = "Nitzschia labella", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia pellucida Grunow in Cleve & Grunow" , fix = "Nitzschia", spe = NA, gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia sigma (Kützing) W.M.Smith" , fix = "Nitzschia sigma Complex", spe = "Nitzschia sigma Complex", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia sp. f. anormale" , fix = "Nitzschia", spe = NA, gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia terrestris (Petersen) Hustedt" , fix = "Nitzschia terrestris", spe = "Nitzschia terrestris", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nupela neglecta Ponader, Lowe & Potapova" , fix = "Nupela neglecta", spe = "Nupela neglecta", gen = "Nupela")
taxontable <- new_entry(ori ="Nupela paludigena (Scherer) Lange-Bertalot" , fix = "Nupela", spe = NA, gen = "Nupela")
taxontable <- new_entry(ori ="Nupela wellneri (Lange-Bertalot) Lange-Bertalot" , fix = "Nupela wellneri", spe = "Nupela wellneri", gen = "Nupela")
taxontable <- new_entry(ori ="Parlibellus protractoides (Hustedt) Witkowski & Lange-Bertalot" , fix = "Parlibellus protractoides", spe = "Parlibellus protractoides", gen = "Parlibellus")
taxontable <- new_entry(ori ="Parlibellus protractus (Grunow) Witkowski Lange-Bertalot & Metzeltin" , fix = "Parlibellus", spe = NA, gen = "Parlibellus")
taxontable <- new_entry(ori ="Pinnularia ardnamurchan (Krammer) Kulikovskiy, Lange-Bertalot & Metzeltin" , fix = "Pinnularia ardnamurchan", spe = "Pinnularia ardnamurchan", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia borealis Ehrenberg var.scalaris (Ehr.) Rabenhorst" , fix = "Pinnularia alpina-lata-borealis complex", spe = "Pinnularia alpina-lata-borealis complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia dubitabilis (Hustedt)Hustedt var. dubitabilis" , fix = "Pinnularia dubitabilis", spe = "Pinnularia dubitabilis", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia irrorata (Grunow) Hustedt" , fix = "Pinnularia irrorata", spe = "Pinnularia irrorata", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia nodosa (Ehrenberg) W.Smith var. nodosa" , fix = "Pinnularia nodosa Complex", spe = "Pinnularia nodosa Complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia rabenhorstii (Grunow) Krammer var. rabenhorstii" , fix = "Navicula rabenhorstii", spe = "Navicula rabenhorstii", gen = "Navicula")
taxontable <- new_entry(ori ="Pinnularia saprophila Lange-Bertalot Kobayasi & Krammer" , fix = "Pinnularia saprophila", spe = "Pinnularia saprophila", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia tirolensis (Metzeltin & Krammer) Krammer var.julma Krammer" , fix = "Pinnularia gibba complex", spe = "Pinnularia gibba complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia viridis (Nitzsch) Ehrenberg var.viridis morphotype 1" , fix = "Pinnularia viridis complex", spe = "Pinnularia viridis complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Placoneis hambergii (Hustedt) Bruder & Medlin" , fix = "Placoneis", spe = NA, gen = "Placoneis")
taxontable <- new_entry(ori ="Placoneis ignorata (Schimanski) Lange-Bertalot" , fix = "Placoneis ignorata", spe = "Placoneis ignorata", gen = "Placoneis")
taxontable <- new_entry(ori ="Placoneis lucinensis Lange-Bertalot in Hofmann Werum & Lange-Bertalot" , fix = "Placoneis lucinensis", spe = "Placoneis lucinensis", gen = "Placoneis")
taxontable <- new_entry(ori ="Placoneis minor (Grunow) Lange-Bertalot" , fix = "Placoneis minor", spe = "Placoneis minor", gen = "Placoneis")
taxontable <- new_entry(ori ="Placoneis rostrata (A.Mayer) E.J. Cox" , fix = "Placoneis rostrata", spe = "Placoneis rostrata", gen = "Placoneis")
taxontable <- new_entry(ori ="Placoneis symmetrica (Hustedt) Lange-Bertalot" , fix = "Placoneis symmetrica", spe = "Placoneis symmetrica", gen = "Placoneis")
taxontable <- new_entry(ori ="Placoneis tersa (Hustedt) Metzeltin & Krammer" , fix = "Placoneis tersa", spe = "Placoneis tersa", gen = "Placoneis")
taxontable <- new_entry(ori ="Placoneis undulata (Oestrup) Lange-Bertalot" , fix = "Placoneis undulata", spe = "Placoneis undulata", gen = "Placoneis")
taxontable <- new_entry(ori ="Planothidium holstii (Cleve) Lange-Bertalot" , fix = "Planothidium holstii", spe = "Planothidium holstii", gen = "Planothidium")
taxontable <- new_entry(ori ="Planothidium peragalloi (Brun & Héribaud) Round et Bukhtiyarova var.parvulum (R. Patrick) N.A. Andresen, Stoermer, & Kreis Jr." , fix = "Planothidium peragallii", spe = "Planothidium peragallii", gen = "Planothidium")
taxontable <- new_entry(ori ="Planothidium pseudotanense (Cleve-Euler) Lange-Bertalot" , fix = "Planothidium pseudotanense", spe = "Planothidium pseudotanense", gen = "Planothidium")
taxontable <- new_entry(ori ="Planothidium robustius (Hustedt) Lange-Bertalot" , fix = "Planothidium robustius", spe = "Planothidium robustius", gen = "Planothidium")
taxontable <- new_entry(ori ="Platessa holsatica (Hustedt) Lange-Bertalot" , fix = "Planothidium holstii", spe = "Planothidium holstii", gen = "Planothidium")
taxontable <- new_entry(ori ="Platessa lutheri (Hustedt) Potapova" , fix = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", spe = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", gen = "Psammothidium")
taxontable <- new_entry(ori ="Platessa ziegleri (Lange-Bertalot) Lange-Bertalot" , fix = "Platessa ziegleri", spe = "Platessa ziegleri", gen = "Platessa")
taxontable <- new_entry(ori ="Prestauroneis integra (W.Smith) Bruder in Bruder & Medlin" , fix = "Prestauroneis integra", spe = "Prestauroneis integra", gen = "Prestauroneis")
taxontable <- new_entry(ori ="Psammothidium acidoclinatum (Lange-Bertalot) Lange-Bertalot" , fix = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", spe = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", gen = "Psammothidium")
taxontable <- new_entry(ori ="Psammothidium frigidum (Hustedt) Bukhtiyarova et Round" , fix = "Psammothidium frigidum", spe = "Psammothidium frigidum", gen = "Psammothidium")
taxontable <- new_entry(ori ="Psammothidium helveticum (Hustedt) Bukhtiyarova et Round" , fix = "Psammothidium helveticum/chlidanos/daonense", spe = "Psammothidium helveticum/chlidanos/daonense", gen = "Psammothidium")
taxontable <- new_entry(ori ="Psammothidium levanderi (Hustedt) Bukhtiyarova" , fix = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", spe = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", gen = "Psammothidium")
taxontable <- new_entry(ori ="Psammothidium rechtense (Leclercq) Lange-Bertalot" , fix = "Psammothidium rechtense", spe = "Psammothidium rechtense", gen = "Psammothidium")
taxontable <- new_entry(ori ="Psammothidium rossii (Hustedt) Bukhtiyarova et Round" , fix = "Psammothidium rossii/altaica", spe = "Psammothidium rossii/altaica", gen = "Psammothidium")
taxontable <- new_entry(ori ="Psammothidium sacculum (Carter) Bukhtiyarova et Round" , fix = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", spe = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", gen = "Psammothidium")
taxontable <- new_entry(ori ="Psammothidium scoticum (Flower & Jones) Bukhtiyarova et Round" , fix = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", spe = "Psammothidium marginulatum/scoticum/lacus-vulcani/levenderi", gen = "Psammothidium")
taxontable <- new_entry(ori ="Psammothidium semiapertum (Hustedt) Aboal" , fix = "Psammothidium semiapertum", spe = "Psammothidium semiapertum", gen = "Psammothidium")
taxontable <- new_entry(ori ="Psammothidium ventrale (Krasske) Bukhtiyarova et Round" , fix = "Psammothidium ventrale Complex", spe = "Psammothidium ventrale Complex", gen = "Psammothidium")
taxontable <- new_entry(ori ="Pseudostaurosira (as Punctastriata) subconstricta (Grunow) Kulikovskiy & Genkal" , fix = "Pseudostaurosira subconstricta", spe = "Pseudostaurosira subconstricta", gen = "Pseudostaurosira")
taxontable <- new_entry(ori ="Pseudostaurosira brevistriata (Grun.in Van Heurck) Williams & Round" , fix = "Pseudostaurosira brevistriata complex", spe = "Pseudostaurosira brevistriata complex", gen = "Pseudostaurosira")
taxontable <- new_entry(ori ="Pulchella obsita(Hustedt) Lange-Bertalot" , fix = "Pulchella obsita", spe = "Pulchella obsita", gen = "Pulchella")
taxontable <- new_entry(ori ="Rhopalodia parallela (Grunow) O.Müller var. parallela" , fix = "Rhopalodia parallela", spe = "Rhopalodia parallela", gen = "Rhopalodia")
taxontable <- new_entry(ori ="Rhopalodia rupestris (W.Smith) Krammer in Lange-Bertalot & Krammer" , fix = "Rhopalodia rupestris", spe = "Rhopalodia rupestris", gen = "Rhopalodia")
taxontable <- new_entry(ori ="Rossithidium silvahercynia (Lange-Bertalot) Bucktiyarova" , fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori ="Sellaphora hustedtii (Krasske) Lange-Bertalot & Werum" , fix = "Navicula ventralis-medioconvexa++", spe = "Navicula ventralis-medioconvexa++", gen = "Navicula")
taxontable <- new_entry(ori ="Sellaphora japonica (Kobayasi) H. Kobayasi in Mayama & Kawashima" , fix = "Sellaphora japonica", spe = "Sellaphora japonica", gen = "Sellaphora")
taxontable <- new_entry(ori ="Sellaphora laevissima (Kützing) D.G. Mann" , fix = "Sellaphora laevissima Complex", spe = "Sellaphora laevissima Complex", gen = "Sellaphora")
taxontable <- new_entry(ori ="Sellaphora pseudopupula (Krasske) Lange-Bertalot" , fix = "Sellaphora pupula Complex", spe = "Sellaphora pupula Complex", gen = "Sellaphora")
taxontable <- new_entry(ori ="Sellaphora stroemii (Hustedt) Kobayasi in Mayama Idei Osada & Nagumo" , fix = "Sellaphora submuralis Complex", spe = "Sellaphora submuralis Complex", gen = "Sellaphora")
taxontable <- new_entry(ori ="Sellaphora ventraloconfusa (Lange-Bertalot) Metzeltin & Lange-Bertalot" , fix = "Eolimna minima-seminulum-atomoides", spe = "Eolimna minima-seminulum-atomoides", gen = "Eolimna")
taxontable <- new_entry(ori ="Sellaphora verecundiae Lange-Bertalot" , fix = "Navicula vitabunda-modica-pexa", spe = "Navicula vitabunda-modica-pexa", gen = "Navicula")
taxontable <- new_entry(ori ="Stauroneis legumen (Ehrenberg) Kützing" , fix = "Stauroneis smithii Complex", spe = "Stauroneis smithii Complex", gen = "Stauroneis")
taxontable <- new_entry(ori ="Stauroneis producta Grunow  in Van Heurck" , fix = "Stauroneis smithii Complex", spe = "Stauroneis smithii Complex", gen = "Stauroneis")
taxontable <- new_entry(ori ="Staurosira berolinensis (Lemm.) Lange-Bertalot" , fix = "Staurosirella pinnata complex", spe = "Staurosirella pinnata complex", gen = "Staurosirella")
taxontable <- new_entry(ori ="Staurosira binodis Lange-Bertalot in Hofmann Werum & Lange-Bertalot" , fix = "Staurosira binodis-robusta", spe = "Staurosira binodis-robusta", gen = "Staurosira")
taxontable <- new_entry(ori ="Staurosira oldenburgioides (Lange-Bertalot) Lange-Bertalot, Kulikovskiy & Witkowski" , fix = "Staurosira oldenburgioides", spe = "Staurosira oldenburgioides", gen = "Staurosira")
taxontable <- new_entry(ori ="Staurosira robusta (Fusey) Lange-Bertalot" , fix = "Staurosira binodis-robusta", spe = "Staurosira binodis-robusta", gen = "Staurosira")
taxontable <- new_entry(ori ="Staurosirella leptostauron (Ehr.) Williams & Round" , fix = "Staurosirella leptostauron complex", spe = "Staurosirella leptostauron complex", gen = "Staurosirella")
taxontable <- new_entry(ori ="Staurosirella pinnata (Ehr.) Williams & Round" , fix = "Staurosirella pinnata complex", spe = "Staurosirella pinnata complex", gen = "Staurosirella")
taxontable <- new_entry(ori ="Staurosirella pinnata (Ehr.) Williams & Round  f. anormale" , fix = "Staurosirella pinnata complex", spe = "Staurosirella pinnata complex", gen = "Staurosirella")
taxontable <- new_entry(ori ="Surirella birostrata Hustedt in Schmidt & al." , fix = "Surirella", spe = NA, gen = "Surirella")
taxontable <- new_entry(ori ="Surirella minuta Brebisson ex Kützing" , fix = "Surirella angusta Complex", spe = "Surirella angusta Complex", gen = "Surirella")
taxontable <- new_entry(ori ="Surirella suecica Grunow in Van Heurck" , fix = "Surirella suecica", spe = "Surirella suecica", gen = "Surirella")
taxontable <- new_entry(ori ="Tabularia tabulata (C.A.Agardh) Snoeijs" , fix = "Tabularia tabulata", spe = "Tabularia tabulata", gen = "Tabularia")
taxontable <- new_entry(ori ="Tryblionella coarctata (Grunow in Cl. & Grun.) D.G. Mann" , fix = "Tryblionella coarctata", spe = "Tryblionella coarctata", gen = "Tryblionella")
taxontable <- new_entry(ori ="Tryblionella littoralis (Grunow in Cl. & Grun.) D.G. Mann" , fix = "Tryblionella", spe = NA, gen = "Tryblionella")
taxontable <- new_entry(ori ="Tryblionella salinarum (Grunow in Cleve & Grunow) Pelletan" , fix = "Tryblionella", spe = NA, gen = "Tryblionella")
taxontable <- new_entry(ori ="Ulnaria danica (Kützing) Compère et Bukhtiyarova" , fix = "Ulnaria ulna complex", spe = "Ulnaria ulna complex", gen = "Ulnaria")
taxontable <- new_entry(ori ="Ulnaria delicatissima (W.Smith) Aboal & Silva" , fix = "Fragilaria tenera complex", spe = "Fragilaria tenera complex", gen = "Fragilaria")
taxontable <- new_entry(ori ="Ulnaria lanceolata (Kütz.) Compère" , fix = "Ulnaria lanceolata", spe = "Ulnaria lanceolata", gen = "Ulnaria")


taxontable[species == "Amphora ocellata", c("genus", "family", "order") := .("Amphora","Catenulaceae","Thalassiophysales")]
taxontable[species == "Navicula vitabunda-modica-pexa", c("genus", "family") := .("Navicula", "Naviculaceae")]


check_taxon_table(taxontable)
taxontable <- taxontable[!duplicated(taxontable$original_name)]

TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))