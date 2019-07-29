# Origin Points for Trips (for osrmRoute)

month_origin <- function(month_patterns_new){

  m_origin <- dplyr::select(month_patterns_new, c("safegraph_place_id", "block_id"))

  zeroPaste <- paste(matrix(data = '0', nrow = length(m_patterns_new$block_id), ncol = 1))
  m_origin$GEOID <- paste(zeroPaste, paste(m_origin$block_id), sep = "")
  m_origin_centroids <- st_centroid(ca_bgs[which(ca_bgs$GEOID %in% m_origin$GEOID),])
  m_origin_centroids <- dplyr::select(m_origin_centroids, c("GEOID", "geometry"))

  m_origin <- left_join(m_origin, m_origin_centroids, by = "GEOID")
  m_origin <- dplyr::select(m_origin, c("GEOID", "safegraph_place_id", "geometry"))
  colnames(m_origin) <- c("origin", "destination", "geometry_origin")
  m_origin_sf <- st_as_sf(m_origin)
  m_origin_sf <- st_set_crs(m_origin_sf, "+proj=longlat +datum=WGS84 +nodefs")

  return(m_origin_sf)
  
}