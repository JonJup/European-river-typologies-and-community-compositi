# - Fix Taxa Hungary Ecosurv


# - old 

# taxontable <- update_taxonomy2(TU)
# taxontable[original_name == "Inula hirta", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := .(
#         "Pentanema hirtum", "Pentanema", "Asteraceae", "Asterales", NA, "Magnoliopsida", "Tracheophyta", "Plantae")
#         ]
# taxontable[, clean := TRUE]
# taxontable[original_name == "Salix sp.", c("family", "order", "class", "phylum", "kingdom") := .("Salicaceae", "Malpighiales", "Magnoliopsida", "Tracheophyta", "Plantae")]
# taxontable[original_name == "Galeopsis sp.", c("genus", "family", "order", "class", "phylum", "kingdom") := .("Stachys", "Lamiaceae", "Lamiales", "Magnoliopsida", "Tracheophyta", "Plantae")]
# taxontable[original_name == "Viola sp.", c("family", "order", "class", "phylum", "kingdom") := .("Violaceae", "Malpighiales", "Magnoliopsida", "Tracheophyta", "Plantae")]
# taxontable[, kindgom := NULL]
# saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))


taxontable <- update_taxonomy_macrophytes(TU, taxontable)

taxontable[clean == FALSE]
taxontable[taxon_state == "", original_name]
taxontable[genus == "Xanthium"]
taxontable[original_name == "Callitiriche sp."]
taxontable[original_name == "Acer sp."          , taxon_state := "drop"]
taxontable[original_name == "Achillea sp."      , taxon_state := "drop"]
taxontable[original_name == "Amaranthus sp."    , taxon_state := "drop"]
taxontable[original_name == "Atriplex sp."      , taxon_state := "drop"]
taxontable[original_name == "Bryonia sp."       , taxon_state := "drop"]
taxontable[original_name == "Callitiriche sp."  , taxon_state := "hydrophytes"]
taxontable[original_name == "Dipsacus sp."      , taxon_state := "drop"]
taxontable[original_name == "Fraxinus sp."      , taxon_state := "drop"]
taxontable[original_name == "Gypsophila sp."    , taxon_state := "drop"]
taxontable[original_name == "Koeleria sp."      , taxon_state := "drop"]
taxontable[original_name == "Molinia sp."       , taxon_state := "drop"]
taxontable[original_name == "Oxalis sp."        , taxon_state := "drop"]
taxontable[original_name == "Phleum sp."        , taxon_state := "drop"]
taxontable[original_name == "Populus spp."      , taxon_state := "drop"]
taxontable[original_name == "Prunus sp."        , taxon_state := "drop"]
taxontable[original_name == "Puccinellia sp."   , taxon_state := "drop"]
taxontable[original_name == "Rosa spp."         , taxon_state := "drop"]
taxontable[original_name == "Schoenoplectus sp.", taxon_state := "helophytes"]
taxontable[original_name == "Sonchus paluster"  , taxon_state := "drop"]
taxontable[original_name == "Sonchus sp."       , taxon_state := "drop"]
taxontable[original_name == "Thalictrum sp."    , taxon_state := "drop"]
taxontable[original_name == "Viola sp."         , taxon_state := "drop"]
taxontable[original_name == "Vitis sp."         , taxon_state := "drop"]
taxontable[original_name == "Xanthium sp."      , taxon_state := "drop"]

saveRDS(taxontable, "data/macrophytes/2022-06-14_taxontable_macrophytes.rds")
