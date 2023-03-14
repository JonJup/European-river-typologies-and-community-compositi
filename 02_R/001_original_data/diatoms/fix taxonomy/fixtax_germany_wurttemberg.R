## what is the most similar in in taxontable 
strdist_id  <- amatch(TU, taxontable$original_name, maxDist = 1000)
strdist_tbl <- data.table(taxontable_name = taxontable$original_name[strdist_id], TU) |> 
        filter(!is.na(taxontable_name)) 

## manual additions to taxon table 
taxontable <- append_to_tt("Achnanthes minutissima","Achnanthidium minutissimum var. minutissimum")
taxontable <- append_to_tt("Caloneis schumanniana","Caloneis schumanniana var. schumanniana")
taxontable <- append_to_tt("Cymbopleura subaequalis","Cymbopleura subaequalis var. subaequalis")
taxontable <- append_to_tt("Diatoma moniliformis var. ovalis","Diatoma moniliformis ssp. ovalis")
taxontable <- append_to_tt("Mayamaea atomus var.alcimonica","Mayamaea atomus var. alcimonica")
taxontable <- append_to_tt("Mayamaea fossalis var.obsidialis","Mayamaea fossalis var. obsidialis")
taxontable <- append_to_tt("Nitzschia palea var. tenuirostris sensu","Nitzschia palea var. tenuirostris")
taxontable <- append_to_tt("Surirella brebissonii var.kuetzingii","Surirella brebissonii var. kuetzingii")

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

taxontable <- new_genus(ori = "Cymbella excisa var. excisa",
                        fix = "Cymbella excisa",
                        spe = "Cymbella excisa",
                        gen = "Cymbella",
                        fam = "Cymbellaceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista"
)
taxontable <- new_genus(ori = "Cymbella hustedtii var. hustedtii",
                        fix = "Cymbella",
                        spe = NA,
                        gen = "Cymbella",
                        fam = "Cymbellaceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Cymbella neoleptoceros var. neoleptoceros",
                        fix = "Cymbella neoleptoceros",
                        spe = "Cymbella neoleptoceros",
                        gen = "Cymbella",
                        fam = "Cymbellaceae",
                        ord = "Cymbellales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable <- new_genus(ori = "Eunotia bilunaris var. bilunaris",
                        fix = "Eunotia arcus/mucophila/bilunaris Complex",
                        spe = "Eunotia arcus/mucophila/bilunaris Complex",
                        gen = "Eunotia",
                        fam = "Eunotiaceae",
                        ord = "Eunotiales",
                        cla = "Bacillariophyceae",
                        phy = "Bacillariophyta",
                        kin = "Chromista")
taxontable[genus == "Staurosira", family := "Staurosiraceae"]
taxontable <- new_entry(ori = "Fragilaria pinnata var. pinnata",
                        fix = "Staurosira mutabilis",
                        spe = "Staurosira mutabilis",
                        gen = "Staurosira")
taxontable <- new_entry(ori = "Fragilaria virescens var. virescens" ,
                        fix = "Fragilaria virescens complex",
                        spe = "Fragilaria virescens complex",
                        gen = "Fragilaria")
taxontable <- new_entry(ori = "Gyrosigma acuminatum var. acuminatum"  ,
                        fix = "Gyrosigma",
                        spe = NA,
                        gen = "Gyrosigma")
taxontable <- new_entry(ori = "Hantzschia amphioxys sensu stricto",
                        fix = "Hantzschia amphioxys",
                        spe = "Hantzschia amphioxys",
                        gen = "Hantzschia")
taxontable <- new_entry(ori = "Luticola mutica var. mutica" ,
                        fix = "Luticola",
                        spe = NA,
                        gen = "Luticola")
taxontable <- new_entry(ori = "Navicula cryptocephala var. cryptocephala",
                        fix = "Navicula cryptocephala",
                        spe = "Navicula cryptocephala",
                        gen = "Navicula")

taxontable <- new_entry(ori = "Navicula kotschyi var. kotschyi", 
                        fix = "Navicula kotschyi",
                        spe = "Navicula kotschyi",
                        gen = "Navicula"
)
taxontable <- new_entry(ori = "Navicula menisculus var. menisculus", 
                        fix = "Navicula menisculus/antonii",
                        spe = "Navicula menisculus/antonii",
                        gen = "Navicula"
)
taxontable <- new_entry(ori = "Navicula radiosa var. radiosa", 
                        fix = "Navicula radiosa",
                        spe = "Navicula radiosa",
                        gen = "Navicula"
)
taxontable <- new_entry(ori = "Navicula reichardtiana var. reichardtiana", 
                        fix = "Navicula reichardtiana-caterva",
                        spe = "Navicula reichardtiana-caterva",
                        gen = "Navicula"
)
taxontable <- new_entry(ori = "Navicula viridula - Sippen", 
                        fix = "Navicula viridula complex",
                        spe = "Navicula viridula complex",
                        gen = "Navicula"
)
taxontable <- new_entry(ori = "Nitzschia calida var. calida", 
                        fix = "Tryblionella calida",
                        spe = "Tryblionella calida",
                        gen = "Tryblionella"
)
taxontable <- new_entry(ori = "Nitzschia capitellata var. capitellata", 
                        fix = "Nitzschia palea complex",
                        spe = "Nitzschia palea complex",
                        gen = "Nitzschia"
)
taxontable <- new_entry(ori = "Nitzschia filiformis var. filiformis", 
                        fix = "Nitzschia filiformis",
                        spe = "Nitzschia filiformis",
                        gen = "Nitzschia"
)
taxontable <- new_entry(ori = "Nitzschia levidensis var. levidensis", 
                        fix = "Tryblionella",
                        spe = NA,
                        gen = "Tryblionella"
)
taxontable <- new_entry(ori = "Nitzschia recta var. recta", 
                        fix = "Nitzschia dissipata-recta Complex",
                        spe = "Nitzschia dissipata-recta Complex",
                        gen = "Nitzschia"
)
taxontable <- new_entry(ori = "Pinnularia gibba var. gibba", 
                        fix = "Pinnularia gibba complex",
                        spe = "Pinnularia gibba complex",
                        gen = "Pinnularia"
)
taxontable <- new_entry(ori = "Reimeria sinuata var. sinuata", 
                        fix = "Reimeria sinuata",
                        spe = "Reimeria sinuata",
                        gen = "Reimeria"
)
taxontable <- new_entry(ori = "Tabellaria flocculosa var. flocculosa", 
                        fix = "Tabellaria flocculosa Complex",
                        spe = "Tabellaria flocculosa Complex",
                        gen =  "Tabellaria"
)
taxontable <- new_entry(ori = "Fragilaria brevistriata var. brevistriata", 
                        fix = "Staurosira brevistriata",
                        spe = "Staurosira brevistriata",
                        gen =  "Staurosira"
)
taxontable <- new_entry(ori = "Navicula pierre-comperei", 
                        fix = "Navicula pierre-comperei",
                        spe = "Navicula pierre-comperei",
                        gen =  "Navicula"
)
taxontable <- new_entry(ori = "Nitzschia filiformis var. filiformis", 
                        fix = "Nitzschia filiformis",
                        spe = "Nitzschia filiformis",
                        gen =  "Nitzschia"
)



## check that TU is empty now
TU <- setdiff(TU, taxontable$original_name)

check_taxon_table(taxontable)

## remove duplicates 
taxontable <- taxontable[!which(duplicated(taxontable$original_name)), ]

## genera 
taxontable[genus == "Diatoma", family := "Tabellariaceae"]
taxontable[genus == "Pseudostaurosira", family := "Staurosiraceae"]
taxontable[genus == "Gomphonema", family := "Gomphonemataceae"]
taxontable[genus == "Gomphonella", family := "Cymbellales incertae sedis"]
taxontable[genus == "Mayamaea", family := "Naviculales incertae sedis"]
taxontable[genus == "Placoneis", family := "Gomphonemataceae"]
## families 
taxontable[family == "Achnanthidiaceae", order := "Achnanthales"]
taxontable[family == ""]
taxontable[original_name == "Brevilinea", c("family", "order", "class") := .("Bacillariophyta familia incertae sedis", "Bacillariophyta ordo incertae sedis", "Bacillariophyta classis incertae sedis")]
taxontable[original_name == "Cymbellales", c("genus", "family") := NA]
taxontable[original_name == "Fontigonium rectangulare", 
           c("family", "order", "class") := .("Hemiaulaceae", "Hemiaulales", "Mediophyceae")]
taxontable[original_name == "Khakista", c("family", "order") := NA]
taxontable[original_name == "Playaensis furtiva", c("family", "order", "class") := 
                   .("Bacillariophyta familia incertae sedis", 
                     "Bacillariophyta ordo incertae sedis", 
                     "Bacillariophyta classis incertae sedis")]
taxontable[family == "Cocconeidaceae", order := "Achnanthales"]
taxontable[family == "Gomphonemataceae", order := "Cymbellales"]
taxontable[family == "Stephanodiscaceae", order := "Thalassiosirales"]
taxontable[family == "Tabellariaceae", order := "Tabellariales"]
taxontable[family == "Cymbellales incertae sedis", order := "Cymbellales"]
taxontable[order == "Chaetocerotales", class := "Mediophyceae"]                
taxontable[order == "Thalassiosirales", class := "Mediophyceae"]                
taxontable[order == "Hemiaulales", class := "Mediophyceae"]                



saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))