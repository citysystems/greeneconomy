### Finding the census population for each block group goint to amenities in Stockton.

pop_blockgroup_stockton <- function(month_hps){

  # stockton_boundary_influence <- st_read("C:/Users/Derek/Desktop/VMT_calc_important_files/SpheresOfInfluence/SpheresOfInfluence.shp") %>% filter(SPHERE == "STOCKTON") %>% st_transform(st_crs(4326))
  # sjc_bgs <- block_groups("California", "San Joaquin County", cb = TRUE) %>% st_transform(st_crs(4326))
  # stockton_bgs <- sjc_bgs[which(sjc_bgs$GEOID %in% st_centroid(sjc_bgs)[stockton_boundary_influence,]$GEOID),]
  # stockton_bgs <- stockton_bgs %>% dplyr::select(GEOID)
  # colnames(stockton_bgs) <- c("origin", "geometry")
  
  stockton_boundary <- 
    places("CA", cb = TRUE) %>% 
    filter(NAME == "Stockton")
  
  stockton_boundary_buffer <-
    stockton_boundary %>% 
    st_transform(26910) %>% 
    st_buffer(1600) %>% 
    st_transform(st_crs(stockton_boundary))
  
  # includes unincorporated areas on periphery that have addresses in Stockton, but removing Lodi and Manteca areas
  stockton_bgs_full <- 
    block_groups("CA", cb = TRUE)[stockton_boundary_buffer,c("GEOID")] %>% 
    filter(!(GEOID %in% c("060770051351","060770040011","060770041061","060770041022")))
  colnames(stockton_bgs_full)[1] <- "origin"
  
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
  acs5emp4 <-
    getCensus(
      name = "acs/acs5",
      vintage = 2018,
      vars = "group(B23025)",
      region = "block group:*",
      regionin = "state:06+county:077",
      key = "c8aa67e4086b4b5ce3a8717f59faa9a28f611dab"
    ) %>% 
    mutate(
      bg = paste0(state,county,tract,block_group)
    ) %>% 
    select_if(!names(.) %in% c("GEO_ID","state","county","tract","block_group","NAME")) %>%
    dplyr::select(-c(contains("EA"),contains("MA"),contains("M"))) %>%
    gather(
      key = "variable",
      value = "estimate",
      -bg
    ) %>%
    mutate(
      label = acs_vars$label[match(variable,acs_vars$name)]
    ) %>% 
    select(-variable) %>% 
    separate(
      label,
      into = c(NA,NA,"participate","type","work"),
      sep = "!!"
    )
  ### tot_labor <-
  ###   acs5emp4 %>% 
  ###   filter(participate == "In labor force") %>% 
  ###   filter(is.na(type)) %>% 
  ###   select(estimate) %>% 
  ###   sum()
  ### tot_employed <-
  ###   acs5emp4 %>% 
  ###   filter(work == "Employed") %>% 
  ###   select(estimate) %>% 
  ###   sum()
  pop_blockgroup_stockton <-
    acs5emp4 %>% 
    filter(work == "Employed")
  ### sjc_bgs <- block_groups("CA") %>% 
  ###   filter(COUNTYFP == "077")
  ### sjc_bgs %>% 
  ###   left_join(
  ###     employed_bgs,
  ###     by = c("GEOID"="bg")
  ###   ) %>% 
  ###   mapview(zcol="estimate")

  pop_blockgroup_stockton <- data.frame(cbind(pop_blockgroup_stockton[,"bg"], pop_blockgroup_stockton[,"estimate"]), stringsAsFactors = FALSE)
  ### pop_blockgroup_stockton <- data.frame(cbind(paste(pop_blockgroup_stockton[, 1], pop_blockgroup_stockton[, 2], pop_blockgroup_stockton[, 3], pop_blockgroup_stockton[, 4], sep = ""), pop_blockgroup_stockton[, 5]), stringsAsFactors = FALSE)
  colnames(pop_blockgroup_stockton) <- c("origin", "origin_population")

  pop_blockgroup_stockton <- left_join(stockton_bgs_full, pop_blockgroup_stockton, by = "origin")

  pop_blockgroup_stockton$origin_numeric <- as.numeric(pop_blockgroup_stockton$origin)
  
  colnames(month_hps)[1] <- "origin_numeric"
  pop_blockgroup_stockton_hps <- left_join(pop_blockgroup_stockton, month_hps, by = "origin_numeric")

  return(pop_blockgroup_stockton_hps)
  
}