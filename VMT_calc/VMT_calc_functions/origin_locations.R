# Origin Points for Locations

month_origin_matrix <- function(month_patterns_new, month_hps, population_bg_stockton){
  
  m_origin_matrix <- dplyr::select(month_patterns_new, c("safegraph_place_id", "location_name", "name_address", "block_id", "visit_count", "unique_visitor_count", "distance_from_home"))
  m_origin_matrix <- left_join(m_origin_matrix, month_hps, by = "block_id")

  zeroPaste <- paste(matrix(data = '0', nrow = length(month_patterns_new$block_id), ncol = 1))
  m_origin_matrix$GEOID <- paste(zeroPaste, paste(m_origin_matrix$block_id), sep = "")
  m_origin_matrix$full_address <- paste(m_origin_matrix$location_name, m_origin_matrix$name_address, sep = ", ")
  m_origin_centroids <- st_centroid(ca_bgs[which(ca_bgs$GEOID %in% m_origin_matrix$GEOID),])
  m_origin_centroids <- dplyr::select(m_origin_centroids, c("GEOID", "geometry"))

  m_origin_matrix <- left_join(m_origin_matrix, m_origin_centroids, by = "GEOID")
  m_origin_matrix <- dplyr::select(m_origin_matrix, c("GEOID", "visit_count", "unique_visitor_count", "number_devices_residing", "distance_from_home", "safegraph_place_id", "location_name", "name_address", "full_address", "geometry"))
  colnames(m_origin_matrix)[1] <- "origin"; colnames(m_origin_matrix)[6] <- "destination"; colnames(m_origin_matrix)[7] <- "location_name"; colnames(m_origin_matrix)[8] <- "destination_address"; colnames(m_origin_matrix)[9] <- "full_address"; colnames(m_origin_matrix)[10] <- "geometry_origin";
  # m_origin_matrix_sf <- st_as_sf(m_origin_matrix)
  # m_origin_matrix_sf <- st_set_crs(m_origin_matrix_sf, "+proj=longlat +datum=WGS84 +nodefs")
  
  # population_bg_stockton <- data.frame(cbind(population_bg_stockton$origin, population_bg_stockton$origin_population))
  # colnames(population_bg_stockton) <- c("origin", "origin_population")

  m_origin_matrix <- left_join(m_origin_matrix, dplyr::select(population_bg_stockton, c("origin", "origin_population", "origin_numeric")), by = "origin")
  m_origin_matrix <- dplyr::select(m_origin_matrix, c("origin", "visit_count", "unique_visitor_count", "number_devices_residing","origin_population", "distance_from_home", 
                                                      "destination", "location_name", "destination_address", "full_address", "geometry_origin", "geometry"))
  colnames(m_origin_matrix)[11] <- "geometry_shape"
  
  # m_origin_matrix_sf <- left_join(m_origin_matrix_sf, population_bg_stockton, by = "origin")
  # m_origin_matrix_sf <- dplyr::select(m_origin_matrix_sf, c("origin", "visit_count", "number_devices_residing","origin_population", "distance_from_home", 
  #                                                           "destination", "location_name", "destination_address", "geometry_origin"))
  
  return(m_origin_matrix)
  
}