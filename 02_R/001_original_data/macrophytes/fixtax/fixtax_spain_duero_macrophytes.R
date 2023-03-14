## -- fix tax spain duero macrophytes

complete_entry <- function(x, s, g, f, o,  c, p, k){
        
        taxontable[original_name == x, c("species", "genus", "family", "order", "class", "phylum", "kingdom") := .(s, g, f, o, c, p, k) ]
        
}

taxontable <- update_taxonomy_macrophytes(TU, taxontable)
taxontable[clean == FALSE]
taxontable[taxon_state == "", original_name]

taxontable[genus == "Ulotrichales"]
taxontable[family == "Ulotrichales"]
taxontable[order == "Ulotrichales"]

taxontable[original_name == "Ankistrodesmus"       , taxon_state := "green_algae"]
taxontable[original_name == "Aphanochaete"         , taxon_state := "green_algae"]
taxontable[original_name == "Asteraceae"           , taxon_state := "drop"]
taxontable[original_name == "Baldellia"            , taxon_state := "hydrophytes"]
taxontable[original_name == "Boraginaceae"         , taxon_state := "drop"]
taxontable[original_name == "Brassicales"          , taxon_state := "drop"]
taxontable[original_name == "Caryophyllaceae"      , taxon_state := "drop"]
taxontable[original_name == "Fabales"              , taxon_state := "drop"]
taxontable[original_name == "Gramineae"            , taxon_state := "drop"]
taxontable[original_name == "Grimmiaceae"          , taxon_state := "drop"]
taxontable[original_name == "Groenlandia"          , taxon_state := "hydrophytes"]
taxontable[original_name == "Hedwigiaceae"         , taxon_state := "drop"]
taxontable[original_name == "Hippuris"             , taxon_state := "helophytes"]
taxontable[original_name == "Labiatae"             , taxon_state := "drop"]
taxontable[original_name == "Lamiales"             , taxon_state := "drop"]
taxontable[original_name == "Malvaceae"            , taxon_state := "drop"]
taxontable[original_name == "Phragmites"           , taxon_state := "helophytes"]
taxontable[original_name == "Trollius"             , taxon_state := "drop"]
taxontable[original_name == "Ulotrichales"         , taxon_state := "green_algae"]

saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))

