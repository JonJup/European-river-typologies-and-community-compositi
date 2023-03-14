## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

## manual additions to taxon table 
taxontable <- append_to_tt("Achnanthes brevipes var.intermedia","Achnanthes brevipes var. intermedia")
taxontable <- append_to_tt("Achnanthes lanceolata-Sippen","Achnanthes lanceolata - Sippen")
taxontable <- append_to_tt("Achnanthes lanceolata ssp. lanceolata","Achnanthes lanceolata ssp. lanceolata var. lanceolata")
taxontable <- append_to_tt("Amphora veneta var.capitata","Amphora veneta var. capitata")
taxontable <- append_to_tt("Asterionella ralfsii","Asterionella ralfsii var. ralfsii")
taxontable <- append_to_tt("Cocconeis placentula var.tenuistriata","Cocconeis placentula var. tenuistriata")
taxontable <- append_to_tt("Craticula riparia var.mollenhaueri","Craticula riparia var. mollenhaueri")
taxontable <- append_to_tt("Cymatopleura elliptica var.hibernica","Cymatopleura elliptica var. hibernica")
taxontable <- append_to_tt("Dimeregramma minor","Dimeregramma minor var. minor")
taxontable <- append_to_tt("Diploneis aestuari", "Diploneis aestuarii")
taxontable <- append_to_tt("Entomoneis paludosa var.subsalina", "Entomoneis paludosa var. subsalina")
taxontable <- append_to_tt("Eunotia muscicola var.perminuta", "Eunotia muscicola var. perminuta")
taxontable <- append_to_tt("Eunotia muscicola var.tridentula", "Eunotia muscicola var. tridentula")
taxontable <- append_to_tt("Eunotia nymanniana", "Eunotia nymanniana Lectotypus")
taxontable <- append_to_tt("Eunotia pectinalis var.undulata", "Eunotia pectinalis var. undulata")
taxontable <- append_to_tt("Eunotia varioundulata", "Eunotia variundulata")
taxontable <- append_to_tt("Fragilaria capucina capitellata-Sippen", "Fragilaria capucina capitellata - Sippen")
taxontable <- append_to_tt("Fragilaria pinnata var.intercedens", "Fragilaria pinnata var. intercedens")
taxontable <- append_to_tt("Fragilaria ulna Gruppe", "Fragilaria ulna - Sippen")
taxontable <- append_to_tt("Gomphonema grovei var.lingulatum", "Gomphonema grovei var. lingulatum")
taxontable <- append_to_tt("Gomphonema parvulum var.lagenula", "Gomphonema parvulum var. lagenula")
taxontable <- append_to_tt("Gomphonema pumilum var.rigidum", "Gomphonema pumilum var. rigidum")
taxontable <- append_to_tt("Navicula agrestis", "Navicula agrestris")
taxontable <- append_to_tt("Navicula longicephala var. villaplanii", "Navicula longicephala var. vilaplanii")
taxontable <- append_to_tt("Navicula salinarum var.rostrata", "Navicula salinarum var. rostrata")
taxontable <- append_to_tt("Navicula vandamii var.mertensiae", "Navicula vandamii var. mertensiae")
taxontable <- append_to_tt("Nitzschia compressa var.vexans", "Nitzschia compressa var. vexans")
taxontable <- append_to_tt("Nitzschia dissipata var.oligotraphenta", "Nitzschia dissipata ssp. oligotraphenta")
taxontable <- append_to_tt("Nitzschia filiformis var.conferta", "Nitzschia filiformis var. conferta")
taxontable <- append_to_tt("Nitzschia levidensis var.salinarum", "Nitzschia levidensis var. salinarum")
taxontable <- append_to_tt("Nitzschia linearis var.tenuis", "Nitzschia linearis var. tenuis")
taxontable <- append_to_tt("Pinnularia borealis var.scalaris", "Pinnularia borealis var. scalaris")
taxontable <- append_to_tt("Pinnularia divergentissima var.minor", "Pinnularia divergentissima var. minor")
taxontable <- append_to_tt("Pinnularia lundii var.linearis", "Pinnularia lundii var. linearis")
taxontable <- append_to_tt("Pinnularia major", "Pinnularia maior")
taxontable <- append_to_tt("Pinnularia microstauron var.nonfasciata", "Pinnularia microstauron var. nonfasciata")
taxontable <- append_to_tt("Pinnularia microstauron var.rostrata", "Pinnularia microstauron var. rostrata")
taxontable <- append_to_tt("Pinnularia subgibba var.undulata", "Pinnularia subgibba var. undulata")
taxontable <- append_to_tt("Pinnularia viridiformis var.minor","Pinnularia viridiformis var. minor")
taxontable <- append_to_tt("Planothidium frequentissimum var.magnum","Planothidium frequentissimum var. magnum")
taxontable <- append_to_tt("Planothidium frequentissimum var.minus","Planothidium frequentissimum var. minus")
taxontable <- append_to_tt("Planothidium oestrupii", "Planothidium oestrupii var. oestrupii")
taxontable <- append_to_tt("Sellaphora laevissima", "Sellaphora laevissima var. laevissima")
taxontable <- append_to_tt("Stauroneis anceps", "Stauroneis anceps var. anceps")

TU <- setdiff(TU, taxontable$original_name)

taxontable <- new_genus(ori = "Nupela imperfecta",
                        fix = "Achnanthes impexiformis",
                        spe = "Achnanthes impexiformis",
                        gen = "Achnanthes",
                        fam = "Achnanthaceae",
                        ord = "Achnanthales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Plagiogramma",
                        fix = "Plagiogramma",
                        spe = NA,
                        gen = "Plagiogramma",
                        fam = "Plagiogrammaceae",
                        ord = "Plagiogrammales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Rhabdonema minutum",
                        fix = "Rhabdonema",
                        spe = NA,
                        gen = "Rhabdonema",
                        fam = "Rhabdonemataceae",
                        ord = "Rhabdonematales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)


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



taxontable <- new_genus(ori = "Anaulus balticus",
                        fix = "Anaulus balticus",
                        spe = "Anaulus balticus",
                        gen = "Anaulus",
                        fam = "Anaulaceae",
                        ord = "Anaulales",
                        cla = "Mediophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Catenula adhaerens",
                        fix = "Catenula adhaerens",
                        spe = "Catenula adhaerens",
                        gen = "Catenula",
                        fam = "Catenulaceae",
                        ord = "Thalassiophysales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Rhaphoneis surirelloides",
                        fix = "Rhaphoneis surirelloides",
                        spe = "Rhaphoneis surirelloides",
                        gen = "Rhaphoneis",
                        fam = "Rhaphoneidaceae",
                        ord = "Rhaphoneidales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Hantzschia spectabilis",
                        fix = "Hantzschia spectabilis",
                        spe = "Hantzschia spectabilis",
                        gen = "Hantzschia",
                        fam = "Bacillariaceae",
                        ord = "Bacillariales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Microcostatus naumannii",
                        fix = "Microcostatus naumannii",
                        spe = "Microcostatus naumannii",
                        gen = "Microcostatus",
                        fam = "Naviculaceae",
                        ord = "Naviculales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Navicula placenta",
                        fix = "Decussata placenta",
                        spe = "Decussata placenta",
                        gen = "Decussata",
                        fam = "Mastogloiaceae",
                        ord = "Mastogloiales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Pauliella taeniata",
                        fix = "Pauliella taeniata",
                        spe = "Pauliella taeniata",
                        gen = "Pauliella",
                        fam = "Anomoeoneidaceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Rhaphoneis amphiceros",
                        fix = "Rhaphoneis amphiceros",
                        spe = "Rhaphoneis amphiceros",
                        gen = "Rhaphoneis",
                        fam = "Rhaphoneidaceae",
                        ord = "Rhaphoneidales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)

TU <- setdiff(TU, taxontable$original_name)

taxontable[genus == "Achnanthes", order := "Achnanthales"]

taxontable <- new_entry(ori = "Achnanthes biasolettiana var. biasolettiana", fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori = "Achnanthes brevipes var. brevipes", fix = "Achnanthes brevipes", spe = "Achnanthes brevipes", gen = "Achnanthes")
taxontable <- new_entry(ori = "Achnanthes clevei var. clevei", fix = "Karayevia clevei", spe = "Karayevia clevei", gen = "Karayevia")
taxontable <- new_entry(ori = "Achnanthes delicatula ssp. delicatula", fix = "Planothidium delicatulum", spe = "Planothidium delicatulum", gen = "Planothidium")
taxontable <- new_entry(ori = "Achnanthes delicatula ssp. engelbrechtii", fix = "Planothidium engelbrechtii", spe = "Planothidium engelbrechtii", gen = "Planothidium")
taxontable <- new_entry(ori = "Achnanthes laevis var. laevis", fix = "Eucocconeis laevis/alpestris", spe = "Eucocconeis laevis/alpestris", gen = "Eucocconeis")
taxontable <- new_entry(ori ="Achnanthes lanceolata ssp. biporoma", fix = "Planothidium lanceolatum", spe = "Planothidium lanceolatum", gen = "Planothidium")
taxontable <- new_entry(ori ="Achnanthes lanceolata ssp. frequentissima var. frequentissima", fix = "Planothidium lanceolatum", spe = "Planothidium lanceolatum", gen = "Planothidium")
taxontable <- new_entry(ori ="Pinnularia globiceps var. globiceps", fix = "Pinnularia globiceps", spe = "Pinnularia globiceps", gen = "Pinnularia")                   
taxontable <- new_entry(ori ="Achnanthes minutissima var. minutissima", fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori ="Achnanthes ploenensis var. ploenensis", fix = "Kolbesia ploenensis", spe = "Kolbesia ploenensis", gen = "Kolbesia")
taxontable <- new_entry(ori ="Achnanthidium inconspicuum", fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori ="Achnanthidium minutissimum var. jackii", fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry(ori ="Achnanthidium rosenstockii var. rosenstockii", fix = "Achnanthidium rosenstockii", spe = "Achnanthidium rosenstockii", gen = "Achnanthidium")
taxontable <- new_entry(ori ="Amphora coffeaeformis var. coffeaeformis", fix = "Halamphora coffeaeformis", spe = "Halamphora coffeaeformis", gen = "Halamphora")
taxontable <- new_entry(ori ="Amphora veneta var. veneta", fix = "Halamphora veneta", spe = "Halamphora veneta", gen = "Halamphora")
taxontable <- new_entry(ori ="Caloneis amphisbaena f. subsalina", fix = "Caloneis amphisbaena", spe = "Caloneis amphisbaena", gen = "Caloneis")
taxontable <- new_genus(ori ="Campylosira cymbelliformis", fix = "Campylosira cymbelliformis", spe = "Campylosira cymbelliformis", gen = "Campylosira", fam = "Cymatosiraceae", ord = "Cymatosirales", cla = "Mediophyceae", phy = "Bacillariophyta", kin= "Chromista")
taxontable <- new_genus(ori ="Carpatogramma crucicula", fix = "Carpatogramma crucicula", spe = "Carpatogramma crucicula", gen = "Carpatogramma", fam = "Naviculales incertae sedis", ord = "Naviculales", cla = "Bacillariophyce",  phy = "Bacillariophyta", kin= "Chromista")
taxontable <- new_entry(ori ="Cocconeis scutellum var. scutellum", fix = "Cocconeis", spe = NA, gen = "Cocconeis")
taxontable <- new_entry(ori ="Cymbella cymbiformis var. cymbiformis", fix = "Cymbella cistula group", spe = "Cymbella cistula group", gen = "Cymbella")
taxontable <- new_entry(ori ="Cymbella laevis var. laevis", fix = "Cymbella", spe = NA, gen = "Cymbella")
taxontable <- new_entry(ori ="Cymbella perpusilla var. perpusilla", fix = "Encyonema gaeumannii/perpusillum", spe = "Encyonema gaeumannii/perpusillum", gen = "Encyonema")
taxontable <- new_entry(ori ="Cymbopleura florentina var. florentina", fix = "Cymbopleura florentina", spe = "Cymbopleura florentina", gen = "Cymbopleura")
taxontable <- new_entry(ori ="Cymbopleura frequens var. frequens", fix = "Cymbopleura frequens", spe = "Cymbopleura frequens", gen = "Cymbopleura")
taxontable <- new_entry(ori ="Cymbopleura hybrida var. hybrida", fix = "Cymbopleura", spe = NA, gen = "Cymbopleura")
taxontable <- new_entry(ori ="Cymbopleura lata var. lata", fix = "Cymbopleura", spe = NA, gen = "Cymbopleura")
taxontable <- new_entry(ori ="Delphineis surirelloides", fix = "Rhaphoneis surirelloides", spe = "Rhaphoneis surirelloides", gen = "Rhaphoneis")
taxontable <- new_entry(ori ="Diatoma hyemalis var. hyemalis", fix = "Diatoma", spe = NA, gen = "Diatoma")
taxontable <- new_genus(ori ="Dimerogramma minor", fix = "Dimerogramma minor", spe = "Dimerogramma minor", gen = "Dimerogramma", fam = "Plagiogrammaceae", ord = "Plagiogrammales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista")
taxontable <- new_entry(ori ="Diploneis smithii var. smithii", fix = "Diploneis", spe = NA, gen = "Diploneis")

taxontable <- new_entry(ori ="Entomoneis paludosa var. paludosa", fix = "Entomoneis paludosa", spe = "Entomoneis paludosa", gen = "Entomoneis")
taxontable <- new_entry(ori ="Eunotia arcus sensu stricto", fix = "Eunotia arcus/mucophila/bilunaris Complex", spe = "Eunotia arcus/mucophila/bilunaris Complex", gen = "Eunotia")
taxontable <- new_entry(ori ="Eunotia paludosa var. trinacria", fix = "Eunotia Complex", spe = NA, gen = "Eunotia")
taxontable <- new_entry(ori ="Eunotia praerupta var. praerupta", fix = "Eunotia praerupta Complex", spe = "Eunotia praerupta Complex", gen = "Eunotia")
taxontable <- new_entry(ori ="Eunotia rhynchocephala var. rhynchocephala", fix = "Eunotia rhynchocephala", spe = "Eunotia rhynchocephala", gen = "Eunotia")
taxontable <- new_entry(ori ="Eunotia subherkiniensis", fix = "Eunotia subherkiniensis", spe = "Eunotia subherkiniensis", gen = "Eunotia")
taxontable <- new_entry(ori ="Fragilaria capucina distans - Sippen", fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry(ori ="Fragilaria capucina radians - Sippen", fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry(ori ="Fragilaria ulna oxyrhynchus - Sippen", fix = "Ulnaria oxyrhynchus", spe = "Ulnaria oxyrhynchus", gen = "Ulnaria")
taxontable <- new_entry(ori ="Fragilaria ulna var. acus", fix = "Ulnaria ulna var.acus", spe = "Ulnaria ulna var.acus", gen = "Ulnaria")
taxontable <- new_entry(ori ="Frustulia rhomboides var. rhomboides", fix = "Frustulia rhomboides Complex", spe = "Frustulia rhomboides Complex", gen = "Frustulia")
taxontable <- new_entry(ori ="Frustulia rhomboides var. saxonica", fix = "Frustulia rhomboides Complex", spe = "Frustulia rhomboides Complex", gen = "Frustulia")
taxontable <- new_entry(ori ="Gomphonema acuminatum var. pusillum", fix = "Gomphonema acuminatum Complex", spe = "Gomphonema acuminatum Complex", gen = "Gomphonema")
taxontable <- new_entry(ori ="Gomphonema augur var. sphaerophorum", fix = "Gomphonema carolinense", spe = "Gomphonema carolinense", gen = "Gomphonema")
taxontable <- new_entry(ori ="Gomphonema olivaceum var. calcareum", fix = "Gomphonema olivaceum/olivaceoides", spe = "Gomphonema olivaceum/olivaceoides", gen = "Gomphonema")
taxontable <- new_entry(ori ="Hippodonta costulatiformis", fix = "Hippodonta costulatiformis", spe = "Hippodonta costulatiformis", gen = "Hippodonta")
taxontable <- new_entry(ori ="Navicula amphiceros", fix = "Navicula amphiceros", spe = "Navicula amphiceros", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula arvensis var. major", fix = "Navicula arvensis-difficillima++", spe = "Navicula arvensis-difficillima++", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula atomus var. atomus", fix = "Mayamaea Complex", spe = NA, gen = "Mayamaea")
taxontable <- new_entry(ori ="Navicula atomus var. excelsa", fix = "Mayamaea Complex", spe = NA, gen = "Mayamaea")
taxontable <- new_entry(ori ="Navicula bourellyivera", fix = "Navicula bourrellyivera", spe = "Navicula bourrellyivera", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula bryophila var. bryophila", fix = "Kobayasiella Complex", spe = NA, gen = "Kobayasiella")
taxontable <- new_entry(ori ="Navicula capitata var. capitata", fix = "Hippodonta capitata", spe = "Hippodonta capitata", gen = "Hippodonta")
taxontable <- new_entry(ori ="Navicula constans var. constans", fix = "Planothidium calcar", spe = "Planothidium calcar", gen = "Planothidium")
taxontable <- new_entry(ori ="Navicula constans var. symmetrica", fix = "Placoneis symmetrica", spe = "Placoneis symmetrica", gen = "Placoneis")
taxontable <- new_entry(ori ="Navicula cosmopolitana", fix = "Sellaphora cosmopolitana", spe = "Sellaphora cosmopolitana", gen = "Sellaphora")
taxontable <- new_entry(ori ="Navicula crucicula var. crucicula", fix = "Parlibellus crucicula", spe = "Parlibellus crucicula", gen = "Parlibellus")
taxontable <- new_entry(ori ="Navicula elginensis var. cuneata", fix = "Placoneis ignorata", spe = "Placoneis ignorata", gen = "Placoneis")
taxontable <- new_entry(ori ="Navicula elginensis var. elginensis", fix = "Placoneis elginensis", spe = "Placoneis elginensis", gen = "Placoneis")
taxontable <- new_entry(ori ="Navicula exigua var. exigua", fix = "Placoneis Complex", spe = NA, gen = "Placoneis")
taxontable <- new_entry(ori ="Navicula fossalis var. fossalis", fix = "Mayamaea fossalis", spe = "Mayamaea fossalis", gen = "Mayamaea")
taxontable <- new_entry(ori ="Navicula fossalis var. obsidialis", fix = "Mayamaea fossalis", spe = "Mayamaea fossalis", gen = "Mayamaea")
taxontable <- new_entry(ori ="Navicula gallica var. gallica", fix = "Diadesmis gallica", spe = "Diadesmis gallica", gen = "Diadesmis")
taxontable <- new_entry(ori ="Navicula gastrum var. gastrum", fix = "Placoneis", spe = NA, gen = "Placoneis")
taxontable <- new_entry(ori ="Navicula laevissima var. laevissima", fix = "Sellaphora laevissima Complex", spe = "Sellaphora laevissima Complex", gen = "Sellaphora")
taxontable <- new_entry(ori ="Navicula longicephala var. longicephala", fix = "Sellaphora laevissima Complex", spe = "Sellaphora laevissima Complex", gen = "Sellaphora")
taxontable <- new_entry(ori ="Navicula minuscula var. minuscula", fix = "Navicula krasskei-egregia-minuscula", spe = "Navicula krasskei-egregia-minuscula", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula monoculata var. monoculata", fix = "Fallacia insociabilis complex", spe = "Fallacia insociabilis", gen = "Fallacia")
taxontable <- new_entry(ori ="Navicula mutica var. intermedia", fix = "Luticola Complex", spe = NA, gen = "Luticola")
taxontable <- new_entry(ori ="Navicula mutica var. mutica",     fix = "Luticola Complex", spe = NA, gen = "Luticola")
taxontable <- new_entry(ori ="Navicula pseudanglica var. pseudanglica", fix = "Placoneis elliptica", spe = "Placoneis elliptica", gen = "Placoneis")
taxontable <- new_entry(ori ="Navicula pupula var. pupula", fix = "Sellaphora pupula Complex", spe = "Sellaphora pupula Complex", gen = "Sellaphora")
taxontable <- new_entry(ori ="Navicula pusilla var. pusilla", fix = "Cosmioneis pusilla", spe = "Cosmioneis pusilla", gen = "Cosmioneis")
taxontable <- new_entry(ori ="Navicula schroeteri var. schroeteri", fix = "Navicula schroeteri", spe = "Navicula schroeteri", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula schroeteri var. symmetrica", fix = "Navicula simulata", spe = "Navicula simulata", gen = "Navicula")
taxontable <- new_entry(ori ="Navicula seminulum var. seminulum", fix = "Eolimna minima-seminulum-atomoides", spe = "Eolimna minima-seminulum-atomoides", gen = "Eolimna")
taxontable <- new_entry(ori ="Navicula viminoides ssp. cosmomarina", fix = "Navicula viminoides", spe = "Navicula viminoides", gen = "Navicula")
taxontable <- new_entry(ori ="Neidium bisulcatum var. bisulcatum", fix = "Neidium bisulcatum Complex", spe = "Neidium bisulcatum Complex", gen = "Neidium")
taxontable <- new_entry(ori ="Neidium bisulcatum var. subampliatum", fix = "Neidium bisulcatum Complex", spe = "Neidium bisulcatum Complex", gen = "Neidium")
taxontable <- new_entry(ori ="Neidium productum var. productum", fix = "Neidium productum Complex", spe = "Neidium productum Complex", gen = "Neidium")
taxontable <- new_entry(ori ="Nitzschia capitellata var. frequens", fix = "Nitzschia palea complex", spe = "Nitzschia palea complex", gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia compressa var. compressa", fix = "Tryblionella punctata", spe = "Tryblionella punctata", gen = "Tryblionella")
taxontable <- new_entry(ori ="Nitzschia lanceola var. minutula", fix = "Nitzschia", spe = NA, gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia lanceolata var. lanceolata", fix = "Nitzschia", spe = NA, gen = "Nitzschia")
taxontable <- new_entry(ori ="Nitzschia tryblionella var. tryblionella", fix = "Nitzschia", spe = NA, gen = "Nitzschia")
taxontable <- new_entry(ori ="Pinnularia angusta var. rostrata", fix = "Pinnularia", spe = NA, gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia appendiculata var. appendiculata", fix = "Pinnularia appenticulata-perirrorata-silvatica", spe = "Pinnularia appenticulata-perirrorata-silvatica", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia brebissonii var. brebissonii", fix = "Pinnularia microstauron Complex", spe = "Pinnularia microstauron Complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia decrescens var. decrescens", fix = "Pinnularia divergens complex", spe = "Pinnularia divergens complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia decrescens var. ignorata", fix = "Pinnularia divergens complex", spe = "Pinnularia divergens complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia divergens var. media", fix = "Pinnularia divergens complex", spe = "Pinnularia divergens complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia divergens var. sublineariformis", fix = "Pinnularia divergens complex", spe = "Pinnularia divergens complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia mayeri var. mayeri", fix = "Pinnularia brauniana", spe = "Pinnularia brauniana", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia mesolepta var. mesolepta", fix = "Pinnularia mesolepta Complex", spe = "Pinnularia mesolepta Complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia neomajor var. neomajor", fix = "Pinnularia maior Complex", spe = "Pinnularia maior Complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia nobilis var. nobilis", fix = "Pinnularia maior Complex", spe = "Pinnularia maior Complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia nobilis var. regularis", fix = "Pinnularia maior Complex", spe = "Pinnularia maior Complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia nodosa var. robusta", fix = "Pinnularia nodosa Complex", spe = "Pinnularia nodosa Complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia persudetica var. silvatica", fix = "Pinnularia persudetica", spe = "Pinnularia persudetica", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia pseudogibba var. pseudogibba", fix = "Pinnularia gibba complex", spe = "Pinnularia gibba complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia pseudogibba var. rostrata", fix = "Pinnularia gibba complex", spe = "Pinnularia gibba complex", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia rhombarea var. rhombarea", fix = "Pinnularia", spe = NA, gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia rupestris var. rupestris", fix = "Pinnularia rupestris-sudetica", spe = "Pinnularia rupestris-sudetica", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia schroeterae var. elliptica", fix = "Pinnularia", spe = NA, gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia schroeterae var. schroeterae", fix = "Pinnularia", spe = NA, gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia stomatophora var. irregularis", fix = "Pinnularia stomatophora-brandelii", spe = "Pinnularia stomatophora-brandelii", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia stomatophora var. stomatophora",fix = "Pinnularia stomatophora-brandelii", spe = "Pinnularia stomatophora-brandelii", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia subcommutata var. nonfasciata", fix = "Pinnularia subcommutata", spe = "Pinnularia subcommutata", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia subcommutata var. subcommutata", fix = "Pinnularia subcommutata", spe = "Pinnularia subcommutata", gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia subrupestris var. subrupestris", fix = "Pinnularia", spe = NA, gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia undula var. cuneata", fix = "Pinnularia", spe = NA, gen = "Pinnularia")
taxontable <- new_entry(ori ="Pinnularia viridiformis var. viridiformis", fix = "Pinnularia viridiformis", spe = "Pinnularia viridiformis", gen = "Pinnularia")
taxontable <- new_genus(ori ="Pinnunavis", fix = "Pinnunavis", spe = NA, gen = "Pinnunavis", fam = "Naviculaceae", ord = "Naviculales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista")
taxontable <- new_entry(ori ="Placoneis opportuna", fix = "Placoneis opportuna", spe = "Placoneis opportuna", gen = "Placoneis")
taxontable <- new_genus(ori ="Raphoneis amphiceros", fix = "Raphoneis amphiceros", spe = "Raphoneis amphiceros", gen = "Raphoneis", fam = "Rhaphoneidaceae", ord = "Rhaponeidales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista")
taxontable <- new_entry(ori ="Rhopalodia gibba var. gibba", fix = "Rhopalodia gibba Complex", spe = "Rhopalodia gibba Complex", gen = "Rhopalodia")
taxontable <- new_entry(ori ="Stauroneis phoenicenteron sensu stricto", fix = "Stauroneis phoenicenteron Complex", spe = "Stauroneis phoenicenteron Complex", gen = "Stauroneis")
taxontable <- new_entry(ori ="Stauroneis smithii var. sagitta", fix = "Stauroneis smithii Complex", spe = "Stauroneis smithii Complex", gen = "Stauroneis")
taxontable <- new_entry(ori ="Surirella brightwellii var. brightwellii", fix = "Surirella", spe = NA, gen = "Surirella")
taxontable <- new_genus(ori ="Tabellaria binalis var. binalis", fix = "Oxyneis", spe = NA, gen = "Oxyneis", fam = "Tabellariaceae", ord = "Rhabdonematales", cla = "Bacillariophyceae", phy = "Bacillariophyta", kin = "Chromista")



taxontable[str_detect(species, "Nitzschia compressa"), 
           c("species", "genus", "family", "order", "class", "phylum", "kingdom") :=
                   .("Tryblionella punctata", "Tryblionella","Bacillariaceae", "Bacillariales", "Bacillariophyceae", "Bacillariophyta", "Chromista")]


## check that TU is empty now
TU <- setdiff(unique(data$taxon), taxontable$original_name)

check_taxon_table(taxontable)

taxontable <- taxontable[!duplicated(taxontable$original_name), ]
taxontable[family == "Rhaphoneidaceae", order := "Rhaphoneidales"]
taxontable[family == "Tabellariaceae", order := "Rhabdonematales"]
taxontable[family == "Plagiogrammaceae", order := "Plagiogrammales"]
taxontable[order == "Naviculales", class := "Bacillariophyceae"]
taxontable[order == "Cymatosirales", class := "Mediophyceae"]

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))