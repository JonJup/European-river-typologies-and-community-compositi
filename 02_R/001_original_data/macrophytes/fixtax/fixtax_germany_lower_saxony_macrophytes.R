# fixtax Germany Lower Saxony 

taxontable <- update_taxonomy_macrophytes(TU = TU, taxontable)
taxontable[clean == FALSE]

taxontable[taxon_state == "", sort(unique(original_name))]
taxontable[taxon_state == "", sort(unique(phylum))]

taxontable[phylum == "Cnidaria"]

taxontable[original_name == "Ludwigia", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := 
                   .(NA, "Ludwigia", "Onagraceae", "Myrtales", NA, "Magnoliopsida", "Tracheophyta", "Plantae" )]

taxontable[original_name == "Microcystis", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := 
                   .(NA, "Microcystis", "Microcystaceae", "Cyanobacteriales", NA, "Cyanobacteriia", "Cyanobacteria", "Bacteria" )]

taxontable[original_name == "Ceratophyllum", c("species", "genus", "family", "order", "subclass", "class", "phylum", "kingdom") := 
                   .(NA, "Ceratophyllum", "Ceratophyllaceae", "Ceratophyllales", NA, "Magnoliopsida", "Tracheophyta", "Plantae" )]

taxontable[genus == "Sedum"]
taxontable[original_name == "Alnus", taxon_state := "drop"]
taxontable[original_name == "Ceratophyllum", taxon_state := "hydrophyte"]
taxontable[original_name == "Chaerophyllum", taxon_state := "drop"]
taxontable[original_name == "Schoenoplectus", taxon_state := "helophytes"]
taxontable[original_name == "Sedum", taxon_state := "drop"]
taxontable[genus == "Schoenoplectus", taxon_state := "helophytes"]


saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))


select(taxontable, original_name, genus, taxon_state) |> View()
