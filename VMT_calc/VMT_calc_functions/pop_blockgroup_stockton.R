### Finding the census population for each block group goint to amenities in Stockton.

pop_blockgroup_stockton <- function(month_hps){

  stockton_boundary_influence <- st_read("C:/Users/Derek/Desktop/VMT_calc_important_files/SpheresOfInfluence/SpheresOfInfluence.shp") %>% filter(SPHERE == "STOCKTON") %>% st_transform(st_crs(4326))
  sjc_bgs <- block_groups("California", "San Joaquin County", cb = TRUE) %>% st_transform(st_crs(4326))
  stockton_bgs <- sjc_bgs[which(sjc_bgs$GEOID %in% st_centroid(sjc_bgs)[stockton_boundary_influence,]$GEOID),]
  stockton_bgs <- stockton_bgs %>% dplyr::select(GEOID)
  colnames(stockton_bgs) <- c("origin", "geometry")
  
  ### pop_blockgroup_stockton <- getCensus(name = "acs/acs5", vintage = 2017, key = "c8aa67e4086b4b5ce3a8717f59faa9a28f611dab",
  ###                                      vars = c("B00001_001E"), region = "block group:*", regionin = "state:06+county:077")
  
  ### pop_blockgroup_stockton_B23025 <-
  ###   getCensus(
  ###     key = "c8aa67e4086b4b5ce3a8717f59faa9a28f611dab",
  ###     name = "acs/acs5",
  ###     vintage = 2018,
  ###     vars = c("B23025_001E", "B23025_002E", "B23025_003E", "B23025_004E", "B23025_005E", "B23025_006E", "B23025_007E"),
  ###     region = "block group:*",
  ###     regionin = "state:06+county:077"
  ###   ) %>%
  ###   mutate(
  ###     total = B23025_001E,
  ###     total_laborforce = B23025_002E,
  ###     total_laborforce_civilian = B23025_003E,
  ###     total_laborforce_civilian_empl = B23025_004E,
  ###     total_laborforce_civilian_not_empl = B23025_005E,
  ###     total_laborforce_civilian_armed_forces = B23025_006E,
  ###     total_laborforce_civilian_not_in_labor = B23025_007E
  ###   ) %>%
  ###   dplyr::select(
  ###     total_laborforce_civilian_empl,
  ###     total_laborforce_civilian_not_empl,
  ###     total_laborforce_civilian_armed_forces,
  ###     total_laborforce_civilian_not_in_labor
  ###   )
  
  pop_blockgroup_stockton <- # pop_blockgroup_stockton_B23027
    getCensus(
      key = "c8aa67e4086b4b5ce3a8717f59faa9a28f611dab",
      name = "acs/acs5",
      vintage = 2018,
      vars = c("B23027_001E", "B23027_003E", "B23027_006E", "B23027_008E", "B23027_011E", "B23027_013E",
               "B23027_016E", "B23027_018E", "B23027_021E", "B23027_023E", "B23027_026E", "B23027_028E",
               "B23027_031E", "B23027_033E", "B23027_036E"),
      region = "block group:*",
      regionin = "state:06+county:077"
    ) %>%
    mutate(
      worked_16_to_19 = B23027_003E,
      no_work_16_to_19 = B23027_006E,
      worked_20_to_24 = B23027_008E,
      no_work_20_to_24 = B23027_011E,
      worked_25_to_44 = B23027_013E,
      no_work_25_to_44 = B23027_016E,
      worked_45_to_54 = B23027_018E,
      no_work_45_to_54 = B23027_021E,
      worked_55_to_64 = B23027_023E,
      no_work_55_to_64 = B23027_026E,
      worked_64_to_69 = B23027_028E,
      no_work_64_to_69 = B23027_031E,
      worked_70_plus = B23027_033E,
      no_work_70_plus = B23027_036E
    ) %>%
    dplyr::select(
      state,
      county,
      tract,
      block_group,
      worked_16_to_19,
      worked_20_to_24,
      worked_25_to_44,
      worked_45_to_54,
      worked_55_to_64,
      worked_64_to_69,
      worked_70_plus
    )

  pop_blockgroup_stockton <- data.frame(cbind(paste(pop_blockgroup_stockton[, 1], pop_blockgroup_stockton[, 2], pop_blockgroup_stockton[, 3], pop_blockgroup_stockton[, 4], sep = ""), rowSums(pop_blockgroup_stockton[, 5:11])), stringsAsFactors = FALSE)
  # pop_blockgroup_stockton <- data.frame(cbind(paste(pop_blockgroup_stockton[, 1], pop_blockgroup_stockton[, 2], pop_blockgroup_stockton[, 3], pop_blockgroup_stockton[, 4], sep = ""), pop_blockgroup_stockton[, 5]), stringsAsFactors = FALSE)
  colnames(pop_blockgroup_stockton) <- c("origin", "origin_population")

  pop_blockgroup_stockton <- left_join(stockton_bgs, pop_blockgroup_stockton, by = "origin")

  pop_blockgroup_stockton$origin_numeric <- as.numeric(pop_blockgroup_stockton$origin)
  
  colnames(month_hps)[1] <- "origin_numeric"
  pop_blockgroup_stockton_hps <- left_join(pop_blockgroup_stockton, month_hps, by = "origin_numeric")

  return(pop_blockgroup_stockton_hps)
}