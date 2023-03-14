
taxontable <- update_taxonomy_macrophytes(TU, taxontable_arg = taxontable)

complete_entry <- function(x, s, g, f, o,  c, p, k){
        
        taxontable[original_name == x, c("species", "genus", "family", "order", "class", "phylum", "kingdom") := .(s, g, f, o, c, p, k) ]
        
}

complete_entry("Liverwort", NA, NA, NA, NA, NA, "Marchantiophyta", " Plantae")
complete_entry("Mosses"   , NA, NA, NA, NA, NA, "Bryophyta",        "Plantae")

taxontable[taxon_state == "", original_name]
taxontable[genus == "Tabellaria"]
taxontable[original_name == "Rycciocarpus natans"]


taxontable[original_name == "Alnus sp."              , taxon_state := "drop"]
taxontable[original_name == "Callitriche Cophocarpa" , taxon_state := "hydrophytes"]
taxontable[original_name == "Nuphar luthea"          , taxon_state := "hydrophytes"]
taxontable[original_name == "Nymphaea alba ssp. alba", taxon_state := "hydrophytes"]
taxontable[original_name == "Rumex hydrolaphatum"    , taxon_state := "drop"]

saveRDS(taxontable, "data/macrophytes/2022-06-14_taxontable_macrophytes.rds")
