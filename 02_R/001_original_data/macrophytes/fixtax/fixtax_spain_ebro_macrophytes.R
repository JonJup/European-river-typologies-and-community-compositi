## -- fix tax spain ebro macrophytes 

taxontable <- update_taxonomy_macrophytes(TU, taxontable)

taxontable[clean == FALSE, original_name]
taxontable[clean == FALSE, ]
taxontable <- taxontable[original_name != "--"]

taxontable[genus == "Paraleptophlebia"]

complete_entry <- function(x, s, g, f, o,  c, p, k){
        
        taxontable[original_name == x, c("species", "genus", "family", "order", "class", "phylum", "kingdom") := .(s, g, f, o, c, p, k) ]
        
}
complete_entry("Achnanthidium sp.",      NA, "Achnanthidium", "Achnanthidiaceae", "Achnanthales", c = "Bacillariophyceae", p = "Ochrophyta", k = "Chromista")
complete_entry("Alisma sp.",             NA, "Alisma"       , "Alismataceae", "Alismatales", c = "Liliopsida", p = "Tracheophyta", k = "Plantae")
complete_entry("Amphora sp."           , NA, "Amphora"      , "Catenulaceae", "Thalassiophysales", "Bacillariophyceae", "Ochrophyta", "Chromista")
complete_entry("Anabaena sp."          , NA, "Anabaena"     , "Nostocaceae", "Cyanobacteriales", "Cyanobacteriia", "Cyanobacteria", "Bacteria")
complete_entry("Aphanizomenon sp."     , NA, "Aphanizomenon", "Nostocaceae", "Cyanobacteriales", "Cyanobacteriia", "Cyanobacteria", "Bacteria")
complete_entry("Aphanocapsa sp."       , NA, "Aphanocapsa"  , "Merismopediaceae" , "Synechococcales", "Cyanobacteriia", "Cyanobacteria", "Bacteria")
complete_entry("Aphanochaete sp"       , NA, "Aphanochaete" , "Aphanochaetaceae" , "Chaetophorales", "Chlorophyceae", "Chlorophyta", "Plantae")
complete_entry("Aphanothece sp."       , NA, "Aphanothece"  ,  "Aphanothecaceae" , "Cyanobacteriales", "Cyanobacteriia", "Cyanobacteria", "Bacteria")
complete_entry("Apium graveolens"      , "Apium graveolens" , "Apium"            , "Apiaceae"        , "Apiales"       , "Magnoliopsida", "Tracheophyta", "Plantae")
complete_entry("Apium sp."             , NA, "Apium"            , "Apiaceae"        , "Apiales"       , "Magnoliopsida", "Tracheophyta", "Plantae")
complete_entry("Arthronema sp."        , NA, "Arthronema"   , "Pseudanabaenaceae", "Pseudanabaenales", "Cyanobacteriia", "Cyanobacteria", "Bacteria")
complete_entry("Aulacoseira sp."       , NA, "Aulacoseira", "Aulacoseiraceae", "Aulacoseirales", "Bacillariophyceae", "Ochrophyta", "Chromista")
complete_entry("Azolla sp."            , NA, "Azolla"     , "Salviniaceae", "Salviniales", "Polypodiopsida", "Tracheophyta", "Plantae" )
complete_entry("Bacillaria paxillifera", "Bacillaria paxillifera", "Bacillaria", "Bacillariaceae", "Bacillariales", "Bacillariophyceae", "Ochrophyta", "Chromista")
complete_entry("Bangia sp."            , NA, "Bangia", "Bangiaceae", "Bangiales", "Bangiophyceae", "Rhodophyta", "Plantae")
complete_entry("Barbula sp."           , NA, "Barbula", "Pottiaceae", "Pottiales", "Bryopsida", "Bryophyta", "Plantae")
complete_entry("Binuclearia sp."       , NA, "Binuclearia", "Ulotrichaceae", "Ulotrichales", "Ulvophyceae", "Chlorophyta", "Plantae")
complete_entry("Botryochloris sp."     , NA, "Botryochloris", "Botryochloridaceae", "Mischococcales", "Xanthophyceae", "Ochrophyta", "Chromista")
complete_entry("Botryococcus sp."      , NA, "Botryococcus", "Botryococcaceae", "Trebouxiales", "Trebouxiophyceae", "Chlorophyta", "Plantae")
complete_entry("Fam. Brachytheciaceae" , NA, NA, "Brachytheciaceae", "Hypnales", "Bryopsida", "Bryophyta", "Plantae")
complete_entry("Fam. Grimmiaceae"      , NA, NA, "Grimmiaceae",      "Grimmiales", "Bryopsida", "Bryophyta", "Plantae")
complete_entry("Fam. Jungermanniaceae" , NA, NA, "Jungermanniaceae", "Jungermanniales", "Jungermanniopsida", "Marchantiophyta", "Plantae")
complete_entry("Fam. Pleosporaceae"    , NA, NA, "Pleosporaceae"   , "Pleosporales"   , "Dothideomycetes"  , "Ascomycota"     , "Fungi")
complete_entry("Fam. Pottiaceae"       , NA, NA, "Pottiaceae"      , "Pottiales"      , "Bryopsida", "Bryophyta", "Plantae")
complete_entry("Fam. Radiococcaceae"   , NA, NA, "Radiococcaceae"  , "Sphaeropleales" , "Chlorophyceae", "Chlorophyta", "Plantae")
complete_entry("Liquen"                , NA, NA, NA, NA, NA, "Ascomycota"     , "Fungi")
complete_entry("Orden Chlorococcales"  , NA, NA, NA, "Chlamydomonadales", "Chlorophyceae", "Chlorophyta", "Plantae")
complete_entry("Orden Chroococcales"   , NA, NA, NA, "Cyanobacteriales", "Cyanobacteriia", "Cyanobacteria", "Bacteria")
complete_entry("Orden Mischococcales"  , NA, NA, NA, "Mischococcales", "Xanthophyceae", "Ochrophyta", "Chromista")
complete_entry("Orden Naviculales"     , NA, NA, NA, "Naviculales", "Bacillariophyceae", "Ochrophyta", "Chromista")


complete_entry("Ceratophyllum sp."     , NA, "Ceratophyllum", "Ceratophyllaceae", "Ceratophyllales"  , "Magnoliopsida"   , "Tracheophyta" , "Plantae")
complete_entry("Coccomyxa sp."         , NA, "Coccomyxa"    , "Coccomyxaceae"   , "Chlamydomonadales", "Chlorophyceae"   , "Chlorophyta"  , "Plantae")
complete_entry("Geminella sp."         , NA, "Geminella"    , "Chlorellaceae"   , "Chlorellales"     , "Trebouxiophyceae", "Chlorophyta"  , "Plantae")
complete_entry("Nodularia sp."         , NA, "Nodularia"    , "Nostocaceae"     , "Cyanobacteriales" , "Cyanobacteriia"  , "Cyanobacteria", "Bacteria")
complete_entry("Schizothrix sp."       , NA, "Schizothrix"  , "Schizotrichaceae", "Cyanobacteriales" , "Cyanobacteriia"  , "Cyanobacteria", "Bacteria")
complete_entry("Filo Bryophyta"        , NA, NA, NA, NA, NA, "Bryophyta", "Plantae")
complete_entry("Filo Hepatophyta"      , NA, NA, NA, NA, NA, "Marchantiophyta", "Plantae")




taxontable[, clean := TRUE]
# taxontable[taxon_state == "", sort(unique(kingdom))]
# taxontable[taxon_state == "", sort(unique(phylum))]
# taxontable[taxon_state == "", sort(unique(class))]
taxontable[taxon_state == "", sort(unique(original_name))]

taxontable[kingdom == "Chromista" & taxon_state == ""]

taxontable[genus == "Zannichellia"]
taxontable[original_name == "Ceratophyllum submersum"                 , taxon_state == "hydrophytes"]
taxontable[original_name == "Alisma sp."                              , taxon_state := "helophytes"]
taxontable[original_name == "Apium graveolens"                        , taxon_state := "drop"]
taxontable[original_name == "Apium sp."                               , taxon_state := "drop"]
taxontable[original_name == "Ceratophyllum sp."                       , taxon_state := "hydrophytes"]
taxontable[original_name == "Eupatorium sp."                          , taxon_state := "drop"]
taxontable[original_name == "Nuphar sp."                              , taxon_state := "hydrophytes"]
taxontable[original_name == "Oenanthe sp."                            , taxon_state := "drop"]
taxontable[original_name == "Ranunculus peltatus ssp. saniculifolius" , taxon_state := "hydrophytes"]
taxontable[original_name == "Salicornia sp."                          , taxon_state := "drop"]
taxontable[original_name == "Schoenus nigricans"                      , taxon_state := "drop"]
taxontable[original_name == "Zannichellia sp."                        , taxon_state := "hydrophytes"]

saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))

## -- old script 
# taxontable <- update_taxonomy2(TU)
# 
# taxontable[clean == FALSE]
# taxontable <- fill_taxon_table(o = "Fam. Brachytheciaceae",
#                                f = "Brachytheciaceae",
#                                or = "Hypnales",
#                                c = "Bryopsida", 
#                                p = "Bryophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Fam. Grimmiaceae",
#                                f = "Grimmiaceae",
#                                or = "Grimmiales",
#                                c = "Bryopsida", 
#                                p = "Bryophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Fam. Jungermanniaceae",
#                                f = "Jungermanniaceae",
#                                or = "Jungermanniales",
#                                c = "Jungermanniopsida", 
#                                p = "Marchantiophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Fam. Pleosporaceae",
#                                f = "Pleosporaceae",
#                                or = "Pleosporales",
#                                c = "Dothideomycetes", 
#                                p = "Ascomycota", 
#                                k = "Fungi")
# taxontable <- fill_taxon_table(o = "Fam. Pottiaceae",
#                                f = "Pottiaceae",
#                                or = "Pottiales",
#                                c = "Bryopsida", 
#                                p = "Bryophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Fam. Radiococcaceae",
#                                f = "Radiococcaceae",
#                                or = "Sphaeropleales",
#                                c = "Chlorophyceae", 
#                                p = "Chlorophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Liquen",
#                                k = "Remove me")
# taxontable <- fill_taxon_table(o = "--",
#                                k = "Remove me")
# taxontable <- fill_taxon_table(o = "Orden Chlorococcales",
#                                or = "Chlamydomonadales",
#                                c = "Chlorophyceae", 
#                                p = "Chlorophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Orden Chroococcales",
#                                or = "Cyanobacteriales",
#                                c = "Cyanobacteriia", 
#                                p = "Cyanobacteria", 
#                                k = "Bacteria")
# taxontable <- fill_taxon_table(o = "Orden Mischococcales",
#                                or = "Mischococcales",
#                                c = "Xanthophyceae", 
#                                p = "Ochrophyta", 
#                                k = "Chromista")
# taxontable <- fill_taxon_table(o = "Orden Naviculales",
#                                or = "Naviculales",
#                                c = "Bacillariophyceae", 
#                                p = "Ochrophyta", 
#                                k = "Chromista")
# taxontable <- fill_taxon_table(o = "Myriophyllum sp.",
#                                g = "Myriophyllum", 
#                                f = "Haloragaceae",
#                                or = "Saxifragales",
#                                c = "Magnoliopsida", 
#                                p = "Tracheophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Ceratophyllum sp.",
#                                g = "Ceratophyllum", 
#                                f = "Ceratophyllaceae",
#                                or = "Ceratophyllales",
#                                c = "Magnoliopsida", 
#                                p = "Tracheophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Coccomyxa sp.",
#                                g = "Coccomyxa", 
#                                f = "Coccomyxaceae",
#                                or = "Chlamydomonadales",
#                                c = "Chlorophyceae", 
#                                p = "Chlorophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Schizothrix sp.",
#                                g = "Schizothrix", 
#                                f = "Schizotrichaceae",
#                                or = "Cyanobacteriales",
#                                c = "Cyanobacteriia", 
#                                p = "Cyanobacteria", 
#                                k = "Bacteria")
# taxontable <- fill_taxon_table(o = "Geminella sp.",
#                                g = "Geminella", 
#                                f = "Chlorellaceae",
#                                or = "Chlorellales",
#                                c = "Trebouxiophyceae", 
#                                p = "Chlorophyta", 
#                                k = "Plantae")
# taxontable <- fill_taxon_table(o = "Nodularia sp.",
#                                g = "Nodularia", 
#                                f = "Nostocaceae",
#                                or = "Cyanobacteriales",
#                                c = "Cyanobacteriia", 
#                                p = "Cyanobacteria", 
#                                k = "Bacteria")
# taxontable <- fill_taxon_table(o = "Porella sp.",
#                                g = "Porella", 
#                                f = "Bryocryptellidae",
#                                or = "Cheilostomatida",
#                                c = "Gymnolaemata", 
#                                p = "Bryozoa", 
#                                k = "Animalia")
# taxontable <- fill_taxon_table(o = "Paraleptophlebia sp.",
#                                g = "Porella", 
#                                f = "Bryocryptellidae",
#                                or = "Cheilostomatida",
#                                c = "Gymnolaemata", 
#                                p = "Bryozoa", 
#                                k = "Animalia")
# 
# taxontable[, clean := TRUE]