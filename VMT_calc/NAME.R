###

append_matrix <- NULL

for(counterMonth in 1:12){
    
  patterns_text <- patterns_choice(counterMonth)
  filename <- paste("C:/Users/Derek/Desktop/m_patterns_new", substr(patterns_text, 38, 51), "_new.RData", sep = "")
  load(filename)
  
  unique_dest <- unique(m_patterns_new[, c("location_name", "name_address")])
  
  append_matrix <- rbind(append_matrix, unique_dest)
  
  rm(m_origin_matrix_sf, m_dest_matrix_sf, m_patterns_new)

}

append_matrix_unique <- unique(append_matrix)

append_matrix_unique$full_address <- paste(append_matrix_unique$location_name, append_matrix_unique$name_address, sep = ", ")
# append_matrix_unique$location_name <- NULL
# append_matrix_unique$name_address <- NULL

all_dest <- left_join(append_matrix_unique, safegraphplaces, by = "full_address")
missing_dest <- all_dest[is.na(all_dest[,c("latitude"),]),]
missing_dest$location_name.y <- NULL
colnames(missing_dest)[1] <- "location_name"

num_missing_dest <- nrow(missing_dest)

for (index_missing_dest in 1:num_missing_dest){
  
  location_name_iter <- missing_dest[index_missing_dest, c("name_address")]
  geo_coordinates <- geocodeSL(location_name_iter)
  
  all_dest[all_dest[,c("full_address")] == missing_dest[index_missing_dest, c("full_address")], c("latitude")] = geo_coordinates[2]
  all_dest[all_dest[,c("full_address")] == missing_dest[index_missing_dest, c("full_address")], c("longitude")] = geo_coordinates[1]
  
}

all_dest$location_name.y <- NULL
colnames(all_dest)[1] <- "location_name"

osrm_drive_stockton_vmt <- do.call(rbind, lapply( 1:nrow(all_dest), function(counterTable){

  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("safegraph_place_id", "longitude", "latitude")]) )

  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")

  return(safegraph_osrm)

}))
