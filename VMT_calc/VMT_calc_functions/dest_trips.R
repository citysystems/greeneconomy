# Destination Points for Trips (for osrmRoute)

month_dest <- function(month_patterns_new){

  m_dest <- select(month_patterns_new, c("safegraph_place_id", "longitude", "latitude"))
  colnames(m_dest) <- c("safegraph_place_id", "longitude", "latitude")

  m_dest <- cbind(m_dest, select(m_1_origin, c("origin")))
  colnames(m_dest) <- c("destination", "longitude_dest", "latitude_dest", "origin")

  m_dest <- select(m_dest, c("origin", "destination", "longitude_dest", "latitude_dest"))
  m_dest_sf <- st_as_sf(m_dest, coords = c("longitude_dest", "latitude_dest"), crs = 4326)
  m_dest_sf <- st_set_crs(m_dest_sf, "+proj=longlat +datum=WGS84 +nodefs")
  
}