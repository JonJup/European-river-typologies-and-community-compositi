# fixtax Germany Bavaria

taxontable <- update_taxonomy_macrophytes(TU = TU, taxontable)
taxontable[clean == FALSE]
taxontable[taxon_state == "", sort(unique(phylum))]
taxontable[taxon_state == "", sort(unique(original_name))]

taxontable[phylum == "Mollusca"]



taxontable[original_name == "Acer", taxon_state := "drop"]
taxontable[original_name == "Ranunculus, aquatisch", taxon_state := "hydrophyte"]
taxontable[original_name == "Ribes", taxon_state := "drop"]
taxontable[original_name == "Rubus  sp.", taxon_state := "drop"]

# taxontable <- update_taxonomy2(TU)
# 
# taxontable[clean == FALSE]
# 
# ## errors discovered below 
# taxontable <- fill_taxon_table(o = "Schizothrix", g = "Schizothrix", f = "Synechococcales familia incertae sedis", or = "Synechococcales", sc = "Synechococcophycidae", c = "Cyanophyceae", p = "Cyanobacteria", k = "Bacteria")
# taxontable <- fill_taxon_table(o = "Iris", g = "Iris", f = "Iridaceae", or = "Asparagales",  c = "Magnoliopsida", p = "Tracheophyta", k = "Plantae")
# taxontable <- fill_taxon_table(o = "Ludwigia", g = "Ludwigia", f = "Onagraceae", or = "Myrtales",  c = "Magnoliopsida", p = "Tracheophyta", k = "Plantae")
# taxontable <- fill_taxon_table(o = "Myriophyllum", g = "Myriophyllum", f = "Haloragaceae", or = "Saxifragales",  c = "Magnoliopsida", p = "Tracheophyta", k = "Plantae")
# taxontable <- fill_taxon_table(o = "Porella", g = "Porella", f = "Porellaceae", or = "Porellales",  c = "Jungermanniopsida", p = "Marchantiophyta", k = "Plantae")
# taxontable <- fill_taxon_table(o = "Viola", g = "Viola", f = "Violacae", or = "Malpighiales",  c = "Magnoliopsida", p = "Tracheophyta", k = "Plantae")
# taxontable <- fill_taxon_table(o = "Ceratophyllum", g = "Ceratophyllum", f = "Ceratophyllaceae", or = "Ceratophyllales",  c = "Magnoliopsida", p = "Tracheophyta", k = "Plantae")
# taxontable <- fill_taxon_table(o = "Microcystis", g = "Microcystis", f = "Microcystaceae", or = "Cyanobacteriales",  c = "Cyanobacteriia", p = "Cyanobacteria", k = "Bacteria")
# taxontable <- fill_taxon_table(o = "Salix", g = "Salix", f = "Salicaceae", or = "Malpighiales",  c = "Magnoliopsida", p = "Tracheophyta", k = "Plantae")
# taxontable <- fill_taxon_table(o = "Salix alba", s = "Salix alba", g = "Salix", f = "Salicaceae", or = "Malpighiales",  c = "Magnoliopsida", p = "Tracheophyta", k = "Plantae")
# 
# 
# taxontable = taxontable[!original_name %in% drop.taxa]
# 
# taxontable[, clean := TRUE]
# saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))


saveRDS(taxontable, paste0("data/macrophytes/",Sys.Date(),"_taxontable_macrophytes.rds"))


select(taxontable, original_name, genus, taxon_state) |> View()
