m_patterns_join_cleanse <- function(m_patterns, safegraphplaces){

  # Joining the patterns data with the safegraph data.
  
  m_patterns_join <- left_join(m_patterns, safegraphplaces, by = "name_address", copy = FALSE, suffix = c(".x", ".y"))
  # m_patterns_join <- m_patterns_join[!is.na(m_patterns_join$longitude), ]
  m_patterns_join <- select(m_patterns_join, c("safegraph_place_id", "location_name", "name_address", "longitude", "latitude", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs"))

  geocode_Stanford_API_key <- "knSPxKIEjqjOvvrkzYGwu9h7DHH3Ne2Yn2huwDwjPLoHGa_PrKda1QBnCij3LTjW"
  
  return(m_patterns_join)
  
}