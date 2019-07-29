safegraphplaces_cleanse <- function(safegraphplaces){

  safegraphplaces <- dplyr::select(safegraphplaces, c("safegraph_place_id", "location_name", "latitude", "longitude", "street_address", "city", "state", "zip_code"))

  name_address <- paste(safegraphplaces$street_address, safegraphplaces$city, safegraphplaces$state, safegraphplaces$zip_code, sep = ", ")
  safegraphplaces <- cbind(safegraphplaces$safegraph_place_id, safegraphplaces$longitude, safegraphplaces$latitude, name_address)
  colnames(safegraphplaces) <- c("safegraph_place_id", "longitude", "latitude", "name_address")

  safegraphplaces_cleanse <- data.frame(safegraphplaces, stringsAsFactors = FALSE)
  
  return(safegraphplaces_cleanse)
  
}