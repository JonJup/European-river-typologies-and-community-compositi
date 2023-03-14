### --- harmonize taxa Joan Gomà

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


taxontable <- new_entry("Achnanthes flexella (Kützing)Brun var. flexella", fix = "Eucocconeis flexella", spe = "Eucocconeis flexella", gen = "Eucocconeis")
taxontable <- new_entry("Achnanthidium minutissimum (Kützing) Czarnecki" , fix = "Achnanthidium minutissimum", spe = "Achnanthidium minutissimum", gen = "Achnanthidium")
taxontable <- new_entry("Adlafia bryophila (Petersen) Moser Lange-Bertalot & Metzeltin", fix = "Kobayasiella", spe = NA, gen = "Kobayasiella")
taxontable <- new_entry("Amphora coffeaeformis (Agardh) Kützing var. coffeaeformis", fix = "Halamphora coffeaeformis", spe = "Halamphora coffeaeformis", gen = "Halamphora")
taxontable <- new_entry("Amphora pediculus (Kützing) Grunow", fix = "Amphora pediculus/inariensis Complex", spe = "Amphora pediculus/inariensis Complex", gen = "Amphora")
taxontable <- new_entry("Caloneis bacillum (Grunow) Cleve"  , fix = "Caloneis bacillum Complex", spe = "Caloneis bacillum Complex", gen = "Caloneis")
taxontable <- new_entry("Caloneis silicula (Ehr.)Cleve"     , fix = "Caloneis silicula Complex", spe = "Caloneis silicula Complex", gen = "Caloneis")
taxontable <- new_entry("Cocconeis placentula Ehrenberg var.euglypta (Ehr.) Grunow"  , fix = "Cocconeis placentula", spe = "Cocconeis placentula", gen = "Cocconeis")
taxontable <- new_entry("Cocconeis placentula Ehrenberg var.lineata (Ehr.)Van Heurck", fix = "Cocconeis placentula", spe = "Cocconeis placentula", gen = "Cocconeis")
taxontable <- new_entry("Craticula halophila (Grunow ex Van Heurck) Mann" , fix = "Craticula molestiformis-halophiloides++", spe = "Craticula molestiformis-halophiloides++", gen = "Craticula")
taxontable <- new_entry("Craticula molestiformis (Hustedt) Lange-Bertalot", fix = "Craticula molestiformis-halophiloides++", spe = "Craticula molestiformis-halophiloides++", gen = "Craticula")
taxontable <- new_entry("Cymbella cistula (Ehrenberg) Kirchner"           , fix = "Cymbella cistula group", spe = "Cymbella cistula group", gen = "Cymbella")
taxontable <- new_entry("Cymbella lancettula (Krammer) Krammer"           , fix = "Cymbella lancettula", spe = "Cymbella lancettula", gen = "Cymbella")
taxontable <- new_entry("Diadesmis contenta (Grunow ex V. Heurck) Mann"   , fix = "Diadesmis contenta", spe = "Diadesmis contenta", gen = "Diadesmis")
taxontable <- new_entry("Diadesmis contenta Grun.var. biceps (Grun. in V.H.) Hamilton", fix = "Diadesmis contenta", spe = "Diadesmis contenta", gen = "Diadesmis")
taxontable <- new_entry("Diadesmis perpusilla (Grunow) D.G. Mann in Round & al.", fix = "Diadesmis perpusilla", spe = "Diadesmis perpusilla", gen = "Diadesmis")
taxontable <- new_entry("Diatoma moniliformis (moniliforme) Kützing"            , fix = "Diatoma moniliformis/tenuis", spe = "Diatoma moniliformis/tenuis", gen = "Diatoma")
taxontable <- new_entry("Diploneis elliptica (Kützing) Cleve"                   , fix = "Diploneis elliptica/oblongella/ovalis/marginestrata/minuta/modica", spe = "Diploneis elliptica/oblongella/ovalis/marginestrata/minuta/modica", gen = "Diploneis")
taxontable <- new_entry("Diploneis oblongella (Naegeli) Cleve-Euler"            , fix = "Diploneis elliptica/oblongella/ovalis/marginestrata/minuta/modica", spe = "Diploneis elliptica/oblongella/ovalis/marginestrata/minuta/modica", gen = "Diploneis")
taxontable <- new_entry("Diploneis ovalis (Hilse) Cleve"                        , fix = "Diploneis elliptica/oblongella/ovalis/marginestrata/minuta/modica", spe = "Diploneis elliptica/oblongella/ovalis/marginestrata/minuta/modica", gen = "Diploneis")
taxontable <- new_entry("Encyonema lacustre (Agardh) F.W.Mills", fix = "Encyonema", spe = NA, gen = "Encyonema")
taxontable <- new_entry("Encyonema silesiacum (Bleisch in Rabh.) D.G. Mann", fix = "Encyonema silesicacum/minutum/lange-bertalotii", spe = "Encyonema silesicacum/minutum/lange-bertalotii", gen = "Encyonema")
taxontable <- new_entry("Encyonopsis cesatii (Rabenhorst) Krammer" , fix = "Encyonopsis cesatii", spe = "Encyonopsis cesatii", gen = "Encyonopsis")
taxontable <- new_entry("Encyonopsis microcephala (Grunow) Krammer", fix = "Encyonopsis descripta/falaisensis/microcephala", spe = "Encyonopsis descripta/falaisensis/microcephala", gen = "Encyonopsis")
taxontable <- new_entry("Eolimna minima(Grunow) Lange-Bertalot"    , fix = "Eolimna minima-seminulum-atomoides", spe = "Eolimna minima-seminulum-atomoides", gen = "Eolimna")
taxontable <- new_entry("Eolimna subminuscula (Manguin) Moser Lange-Bertalot & Metzeltin", fix = "Sellaphora submuralis Complex", spe = "Sellaphora submuralis Complex", gen = "Sellaphora")
taxontable <- new_entry("Epithemia adnata (Kützing) Brebisson", fix = "Epithemia adnata", spe = "Epithemia adnata", gen = "Epithemia")
taxontable <- new_entry("Eucocconeis flexella (Kützing) Brun", fix = "Eucocconeis flexella", spe = "Eucocconeis flexella", gen = "Eucocconeis")
taxontable <- new_entry("Eucocconeis laevis (Oestrup) Lange-Bertalot", fix = "Eucocconeis laevis/alpestris", spe = "Eucocconeis laevis/alpestris", gen = "Eucocconeis")
taxontable <- new_entry("FALLACIA  A.J. Stickle & D.G. Mann", fix = "Fallacia", spe = NA, gen = "Fallacia")
taxontable <- new_entry("Fallacia pygmaea (Kützing) Stickle & Mann  ssp.pygmaea Lange-Bertalot", fix = "Fallacia pygmaea-forcipata", spe = "Fallacia pygmaea-forcipata", gen = "Fallacia")
taxontable <- new_entry("Fallacia subhamulata (Grunow in V. Heurck) D.G. Mann", fix = "Fallacia subhamulata/helensis", spe = "Fallacia subhamulata/helensis", gen = "Fallacia")
taxontable <- new_entry("Fragilaria capucina Desmazieres var. austriaca (Grunow) Lange-Bertalot", fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry("Fragilaria capucina Desmazieres var. rumpens (Kützing) Lange-Bertalot" , fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry("Fragilaria capucina Desmazieres var.vaucheriae(Kützing)Lange-Bertalot" , fix = "Fragilaria capucina complex", spe = "Fragilaria capucina complex", gen = "Fragilaria")
taxontable <- new_entry("Fragilaria delicatissima (W.Smith) Lange-Bertalot", fix = "Fragilaria tenera complex", spe = "Fragilaria tenera complex", gen = "Fragilaria")
taxontable <- new_entry("Fragilaria elliptica Schumann (Staurosira)", fix = "Staurosira construens complex", spe = "Staurosira construens complex", gen = "Staurosira")
taxontable <- new_entry("Fragilaria parasitica (W.Sm.) Grun. var. parasitica", fix = "Pseudostaurosira parasitica complex", spe = "Pseudostaurosira parasitica complex", gen = "Pseudostaurosira")
taxontable <- new_entry("Fragilaria ulna (Nitzsch.) Lange-Bertalot var. ulna", fix = "Ulnaria ulna complex", spe = "Ulnaria ulna complex", gen = "Ulnaria")
taxontable <- new_entry("Fragilaria ulna (Nitzsch.)Lange-Bertalot var.acus (Kütz.) Lange-Bertalot", fix = "Ulnaria ulna var.acus", spe = "Ulnaria ulna var.acus", gen = "Ulnaria")
taxontable <- new_entry("Frustulia vulgaris (Thwaites) De Toni", fix = "Frustulia vulgaris", spe = "Frustulia vulgaris", gen = "Frustulia")
taxontable <- new_entry("GEISSLERIA Lange-Bertalot & Metzeltin", fix = "Geissleria", spe = NA, gen = "Geissleria")
taxontable <- new_entry("GOMPHONEMA  C.G. Ehrenberg", fix = "Gomphonema", spe = NA, gen = "Gomphonema")
taxontable <- new_entry("Gomphonema angustatum (Kützing) Rabenhorst", fix = "Gomphonema angustatum Complex", spe = "Gomphonema angustatum Complex", gen = "Gomphonema")
taxontable <- new_entry("Gomphonema minutum(Ag.)Agardh f. minutum", fix = "Gomphonema", spe = NA, gen = "Gomphonema")
taxontable <- new_entry("Gomphonema parvulum (Kützing) Kützing var. parvulum f. parvulum", fix = "Gomphonema parvulum Complex", spe = "Gomphonema parvulum Complex", gen = "Gomphonema")
taxontable <- new_entry("Hantzschia amphioxys (Ehr.) Grunow in Cleve et Grunow 1880", fix = "Hantzschia amphioxys", spe = "Hantzschia amphioxys", gen = "Hantzschia")
taxontable <- new_entry("Luticola goeppertiana (Bleisch in Rabenhorst) D.G. Mann", fix = "Luticola", spe = NA, gen = "Luticola")
taxontable <- new_entry("Luticola mutica (Kützing) D.G. Mann", fix = "Luticola", spe = NA, gen = "Luticola")
taxontable <- new_entry("Luticola ventricosa (Kützing) D.G. Mann", fix = "Luticola ventricosa", spe = "Luticola ventricosa", gen = "Luticola")
taxontable <- new_entry("Mastogloia lacustris (Grunow) van Heurck", fix = "Mastogloia lacustris", spe = "Mastogloia lacustris", gen = "Mastogloia")
taxontable <- new_entry("Mayamaea atomus (Kützing) Lange-Bertalot", fix = "Mayamaea", spe = NA, gen = "Mayamaea")
taxontable <- new_entry("Mayamaea atomus var. permitis (Hustedt) Lange-Bertalot", fix = "Mayamaea", spe = NA, gen = "Mayamaea")
taxontable <- new_entry("Meridion circulare (Greville) C.A.Agardh var. circulare", fix = "Meridion circulare Complex", spe = "Meridion circulare Complex", gen = "Meridion")
taxontable <- new_entry("NAVICULA  J.B.M. Bory de St. Vincent", fix = "Navicula", spe = NA, gen = "Navicula")
taxontable <- new_entry("Navicula cincta (Ehr.) Ralfs in Pritchard", fix = "Navicula cincta-heufleri", spe = "Navicula cincta-heufleri", gen = "Navicula")
taxontable <- new_entry("Navicula tripunctata (O.F.Müller) Bory", fix = "Navicula margalithii/tripunctata", spe = "Navicula margalithii/tripunctata", gen = "Navicula")
taxontable <- new_entry("NEIDIUM  E. Pfitzer", fix = "Neidium", spe = NA, gen = "Neidium")
taxontable <- new_entry("NITZSCHIA  A.H. Hassall", fix = "Nitzschia", spe = NA, gen = "Nitzschia")
taxontable <- new_entry("Nitzschia acicularis(Kützing) W.M.Smith", fix = "Nitzschia acicularis Complex", spe = "Nitzschia acicularis Complex", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia constricta (Kützing) Ralfs in Pritchard", fix = "Psammodictyon constrictum", spe = "Psammodictyon constrictum", gen = "Psammodictyon")
taxontable <- new_entry("Nitzschia dissipata(Kützing)Grunow var.dissipata", fix = "Nitzschia dissipata-recta Complex", spe = "Nitzschia dissipata-recta Complex", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia linearis(Agardh) W.M.Smith var.linearis", fix = "Nitzschia pura-linearis Complex", spe = "Nitzschia pura-linearis Complex", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia linearis(Agardh) W.M.Smith var.subtilis(Grunow) Hustedt", fix = "Nitzschia pura-linearis Complex", spe = "Nitzschia pura-linearis Complex", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia palea (Kützing) W.Smith", fix = "Nitzschia palea-paleacea", spe = "Nitzschia palea-paleacea", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia sinuata (Thwaites) Grunow var.delognei (Grunow)Lange-Bertalot", fix = "Nitzschia denticula var.delognei", spe = "Nitzschia denticula var.delognei", gen = "Nitzschia")
taxontable <- new_entry("Nitzschia sinuata (Thwaites) Grunow var.tabellaria Grunow", fix = "Nitzschia sinuata Complex", spe = "Nitzschia sinuata Complex", gen = "Nitzschia")
taxontable <- new_entry("PINNULARIA  C.G. Ehrenberg", fix = "Pinnularia", spe = NA, gen = "Pinnularia")
taxontable <- new_entry("Planothidium frequentissimum(Lange-Bertalot)Lange-Bertalot", fix = "Planothidium lanceolatum", spe = "Planothidium lanceolatum", gen = "Planothidium")
taxontable <- new_entry("Planothidium lanceolatum(Brebisson ex Kützing) Lange-Bertalot", fix = "Planothidium lanceolatum", spe = "Planothidium lanceolatum", gen = "Planothidium")
taxontable <- new_entry("Puncticulata radiosa (Lemmermann) Håkansson", fix = "Puncticulata radiosa", spe = "Puncticulata radiosa", gen = "Puncticulata")
taxontable <- new_entry("Rhoicosphenia abbreviata (C.Agardh) Lange-Bertalot", fix = "Rhoicosphenia", spe = NA, gen = "Rhoicosphenia")
taxontable <- new_entry("Sellaphora seminulum (Grunow) D.G. Mann", fix = "Eolimna minima-seminulum-atomoides", spe = "Eolimna minima-seminulum-atomoides", gen = "Eolimna")
taxontable <- new_entry("Sellaphora stroemii (Hustedt) Mann", fix = "Sellaphora submuralis Complex", spe = "Sellaphora submuralis Complex", gen = "Sellaphora")
taxontable <- new_entry("Tryblionella calida (grunow in Cl. & Grun.) D.G. Mann", fix = "Tryblionella calida", spe = "Tryblionella calida", gen = "Tryblionella")
taxontable <- new_entry("Ulnaria capitata (Ehrenberg) Compère", fix = "Ulnaria ulna complex", spe = "Ulnaria ulna complex", gen = "Ulnaria")


taxontable[original_name == "Cocconeis placentula var.intermedia", c("fixed_name", "species") := .("Cocconeis placentula", "Cocconeis placentula")]	
taxontable[original_name %in% c("Cymbella silesiaca", "Cymbella silesiaca Bleisch"), c("fixed_name", "species") := .("Encyonema silesicacum/minutum/lange-bertalotii", "Encyonema silesicacum/minutum/lange-bertalotii")]	
taxontable[original_name == "Navicula subminuscula", c("fixed_name", "species", "genus", "family") := .("Sellaphora submuralis Complex", "Sellaphora submuralis Complex", "Sellaphora", "Sellaphora")]

taxontable[original_name %in% c("Fragilaria ulna", "Fragilaria ulna Gruppe", "Fragilaria ulna - Sippen"), 
           c("fixed_name", "species", "genus", "family", "order") := .(
                   "Ulnaria ulna complex", "Ulnaria ulna complex", "Ulnaria", "Ulnariaceae", "Licmophorales"
           )]
taxontable[species == "Luticola ", species := NA]
taxontable[species == "Mayamaea ", species := NA]
taxontable[fixed_name %in% c("Planothidium frequentissimum", "Planothidium frequentissimum var.magnum"),
           c("fixed_name", "species"):= .("Planothidium lanceolatum")]        
taxontable[fixed_name == "Sellaphora stroemii", c("fixed_name", "species") := .(
        "Sellaphora submuralis Complex", "Sellaphora submuralis Complex"
)]


TU <- unique(data$taxon) |> sort()
TU <- setdiff(TU, taxontable$original_name)

check_taxon_table(taxontable)
taxontable <- taxontable[!duplicated(taxontable$original_name)]

saveRDS(taxontable, paste0("data/diatoms/",Sys.Date(),"_taxontable_diatoms.rds"))