# Destination Points for Location

month_dest_matrix <- function(month_patterns_join, dest_unique, safegraphplaces){

  month_patterns_join <- unique(dplyr::select(month_patterns_join, c("location_name", "name_address", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs")))
  month_patterns_join$safegraph_place_id <- NA; month_patterns_join$longitude <- NA; month_patterns_join$latitude <- NA
  month_patterns_join <- dplyr::select(month_patterns_join, c("safegraph_place_id", "location_name", "name_address", "longitude", "latitude",
                                                              "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs"))
  
  m_dest_matrix <- dplyr::select(month_patterns_join, c("safegraph_place_id", "location_name", "name_address", "longitude", "latitude", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs"))
  m_dest_matrix$full_address <- paste(m_dest_matrix$location_name, m_dest_matrix$name_address, sep = ", ")
  colnames(m_dest_matrix) <- c("destination", "location_name", "name_address", "longitude", "latitude", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "origin_visit_cbgs", "origin_visitor_cbgs", "full_address")
  # m_dest_matrix_sf <- st_as_sf(m_dest_matrix, coords = c("longitude", "latitude"), crs = 4326)
  # m_dest_matrix_sf <- st_set_crs(m_dest_matrix_sf, "+proj=longlat +datum=WGS84 +nodefs")
  # colnames(m_dest_matrix_sf)[8] <- "geometry_destination"
  
  destinations <- dest_unique[[1]]
  
  for(counter in destinations){
    
    if(nrow(m_dest_matrix[m_dest_matrix$full_address == counter, ]) > 1){
      
      row_temp <- m_dest_matrix[m_dest_matrix$full_address == counter, ]
      
      row_new <- row_temp[1,]
      row_new$distance_from_home <- as.character( round( sum( as.numeric(row_temp$distance_from_home) * as.numeric(row_temp$raw_visit_counts) ) / sum( as.numeric(row_temp$raw_visit_counts) ) ) )
      row_new$raw_visit_counts <- as.character(sum(as.numeric(row_temp$raw_visit_counts)))
      row_new$raw_visitor_counts <- as.character(sum(as.numeric(row_temp$raw_visitor_counts)))
      
      m_dest_matrix <- m_dest_matrix[!(m_dest_matrix$full_address == counter), ]
      
      m_dest_matrix <- rbind(m_dest_matrix, row_new)
      
    }
    
  }
  
  m_dest_matrix$longitude <- NULL
  m_dest_matrix$latitude <- NULL

  safegraphplaces_filtered <- subset(safegraphplaces, !duplicated(safegraphplaces[,"name_address"]))
  
  m_dest_matrix_longlat <- left_join(m_dest_matrix, select(safegraphplaces_filtered, c("name_address", "longitude", "latitude")), by = "name_address")
  m_dest_matrix_longlat <- unique(m_dest_matrix_longlat)
  
  return(m_dest_matrix_longlat)

}