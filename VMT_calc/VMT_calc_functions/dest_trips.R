# Destination Points for Trips (for osrmRoute)

month_dest <- function(month_patterns_new, month_origin_sf){

  m_dest <- dplyr::select(month_patterns_new, c("safegraph_place_id", "longitude", "latitude"))
  colnames(m_dest) <- c("safegraph_place_id", "longitude", "latitude")

  m_dest <- cbind(m_dest, month_origin_sf$origin)
  colnames(m_dest) <- c("destination", "longitude_dest", "latitude_dest", "origin")

  m_dest <- dplyr::select(m_dest, c("origin", "destination", "longitude_dest", "latitude_dest"))
  m_dest_sf <- st_as_sf(m_dest, coords = c("longitude_dest", "latitude_dest"), crs = 4326)
  m_dest_sf <- st_set_crs(m_dest_sf, "+proj=longlat +datum=WGS84 +nodefs")
  colnames(m_dest_sf) <- c("origin", "destination", "geometry_destination")
  
  return(m_dest_sf)
  
}