# Origin Points for Locations

month_origin_matrix <- function(month_patterns_new, month_hps, population_bg_stockton){

  m_origin_matrix <- select(month_patterns_new, c("safegraph_place_id", "name_address", "block_id", "visit_count", "unique_visitor_count"))
  m_origin_matrix <- left_join(m_origin_matrix, month_hps, by = "block_id")

  zeroPaste <- paste(matrix(data = '0', nrow = length(month_patterns_new$block_id), ncol = 1))
  m_origin_matrix$GEOID <- paste(zeroPaste, paste(m_origin_matrix$block_id), sep = "")
  m_origin_centroids <- st_centroid(ca_bgs[which(ca_bgs$GEOID %in% m_origin_matrix$GEOID),])
  m_origin_centroids <- select(m_origin_centroids, c("GEOID", "geometry"))

  m_origin_matrix <- left_join(m_origin_matrix, m_origin_centroids, by = "GEOID")
  m_origin_matrix <- select(m_origin_matrix, c("GEOID", "visit_count", "unique_visitor_count", "number_devices_residing", "safegraph_place_id", "name_address", "geometry"))
  colnames(m_origin_matrix)[1] <- "origin"; colnames(m_origin_matrix)[5] <- "destination"; colnames(m_origin_matrix)[6] <- "destination_address"; colnames(m_origin_matrix)[7] <- "geometry_origin";
  m_origin_matrix_sf <- st_as_sf(m_origin_matrix)
  m_origin_matrix_sf <- st_set_crs(m_origin_matrix_sf, "+proj=longlat +datum=WGS84 +nodefs")
  
  m_origin_matrix_sf <- left_join(m_origin_matrix_sf, population_bg_stockton, by = "origin")
  
  return(m_origin_matrix_sf)
  
}