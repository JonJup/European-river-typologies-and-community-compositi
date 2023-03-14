## -- fix tax uk 

complete_entry <- function(x, s, g, f, o,  c, p, k){
        
        taxontable[original_name == x, c("species", "genus", "family", "order", "class", "phylum", "kingdom") := .(s, g, f, o, c, p, k) ]
        
}

taxontable <- update_taxonomy_macrophytes(TU, taxontable)

taxontable[clean == FALSE]
taxontable[taxon_state == "", original_name]


complete_entry("Urtica",      NA, "Urtica", "Urticaceae", "Rosales", "Magnoliopsida", "Tracheophyta", "Plantae")

taxontable[genus == "Urtica"]
taxontable[taxon_state == ""]
taxontable[original_name == "Symphoricarpus"]

taxontable[original_name == "Acorus"    , taxon_state := "helophytes"]
taxontable[original_name == "Atriplex"  , taxon_state := "drop"]
taxontable[original_name == "Bulboschoenus"     , taxon_state := "drop"]
taxontable[original_name == "Caltha"    , taxon_state := "helophytes"]
taxontable[original_name == "Catabrosa" , taxon_state := "helophytes"]
taxontable[original_name == "Geum"      , taxon_state := "drop"]
taxontable[original_name == "Gunnera"   , taxon_state := "drop"]
taxontable[original_name == "Lampetra"  , taxon_state := "drop"]
taxontable[original_name == "Lunaria"   , taxon_state := "drop"]
taxontable[original_name == "Lycopus"   , taxon_state := "drop"]
taxontable[original_name == "Mimulus"   , taxon_state := "drop"]
taxontable[original_name == "Montbretia" , taxon_state := "drop"]
taxontable[original_name == "Myosoton"  , taxon_state := "drop"]
taxontable[original_name == "Plantago"  , taxon_state := "drop"]
taxontable[original_name == "Pulicaria" , taxon_state := "drop"]
taxontable[original_name == "Ranunculus (Batrachian) spp.", taxon_state := "drop"]
taxontable[original_name == "Rheum"     , taxon_state := "drop"]
taxontable[original_name == "Symphoricarpus"    , taxon_state := "drop"]
taxontable[original_name == "Symphytum" , taxon_state := "drop"]
taxontable[original_name == "Tussilago" , taxon_state := "drop"]
taxontable[original_name == "Urtica"    , taxon_state := "drop"]

saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))
