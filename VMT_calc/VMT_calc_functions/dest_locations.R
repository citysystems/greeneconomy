# Destination Points for Location

month_dest_matrix <- function(month_patterns_join){

  m_dest_matrix <- dplyr::select(month_patterns_join, c("safegraph_place_id", "location_name", "name_address", "longitude", "latitude", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs"))
  m_dest_matrix$full_address <- paste(m_dest_matrix$location_name, m_dest_matrix$name_address, sep = ", ")
  colnames(m_dest_matrix) <- c("destination", "location_name", "name_address", "longitude", "latitude", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "origin_visit_cbgs", "origin_visitor_cbgs", "full_address")
  # m_dest_matrix_sf <- st_as_sf(m_dest_matrix, coords = c("longitude", "latitude"), crs = 4326)
  # m_dest_matrix_sf <- st_set_crs(m_dest_matrix_sf, "+proj=longlat +datum=WGS84 +nodefs")
  # colnames(m_dest_matrix_sf)[8] <- "geometry_destination"
  
  return(m_dest_matrix)
  
}