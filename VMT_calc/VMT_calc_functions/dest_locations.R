# Destination Points for Location

month_dest_matrix <- function(month_patterns_join){

  m_dest_matrix <- select(month_patterns_join, c("safegraph_place_id", "name_address", "longitude", "latitude", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs"))
  colnames(m_1_dest_matrix) <- c("destination", "name_address", "longitude", "latitude", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "origin_visit_cbgs", "origin_visitor_cbgs")
  m_dest_matrix_sf <- st_as_sf(m_1_dest_matrix, coords = c("longitude", "latitude"), crs = 4326)
  m_dest_matrix_sf <- st_set_crs(m_1_dest_matrix_sf, "+proj=longlat +datum=WGS84 +nodefs")

  return(m_dest_matrix_sf)
  
}