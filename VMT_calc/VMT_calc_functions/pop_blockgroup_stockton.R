### Finding the census population for each block group goint to amenities in Stockton.

stockton_boundary_influence <- st_read("C:/Users/Derek/Desktop/Stockton_R_code/VMT_Calculation_new/SpheresOfInfluence/SpheresOfInfluence.shp") %>% filter(SPHERE == "STOCKTON") %>% st_transform(st_crs(4326))
sjc_bgs <- block_groups("California", "San Joaquin County", cb = TRUE) %>% st_transform(st_crs(4326))
stockton_bgs <- sjc_bgs[which(sjc_bgs$GEOID %in% st_centroid(sjc_bgs)[stockton_boundary_influence,]$GEOID),]
stockton_bgs <- stockton_bgs %>% dplyr::select(GEOID)
colnames(stockton_bgs) <- c("origin", "geometry")

pop_blockgroup_stockton <- getCensus(name = "acs/acs5", vintage = 2017, key = "c8aa67e4086b4b5ce3a8717f59faa9a28f611dab",
                             vars = c("B00001_001E"), region = "block group:*", regionin = "state:06+county:077")
pop_blockgroup_stockton <- data.frame(cbind(paste(pop_blockgroup_stockton[, 1], pop_blockgroup_stockton[, 2], pop_blockgroup_stockton[, 3], pop_blockgroup_stockton[, 4], sep = ""), pop_blockgroup_stockton[, 5]), stringsAsFactors = FALSE)
colnames(pop_blockgroup_stockton) <- c("origin", "origin_population")

pop_blockgroup_stockton <- left_join(stockton_bgs, pop_blockgroup_stockton, by = "origin")

return(pop_blockgroup_stockton)