m_patterns_cleanse <- function(p_text){
  
  m_patterns <- read.csv(p_text, header=TRUE, stringsAsFactors = FALSE)
  m_patterns <- dplyr::select(m_patterns, c("location_name", "street_address", "city", "state", "zip_code", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visitor_home_cbgs", "visit_home_cbgs"))
  # m_patterns <- filter(m_patterns, city == 'stockton')
  
  name_address <- paste(m_patterns$street_address, m_patterns$city, m_patterns$state, m_patterns$zip_code, sep = ", ")
  m_patterns <- cbind(m_patterns$location_name, m_patterns$street_address, m_patterns$city, m_patterns$state, m_patterns$zip_code, m_patterns$distance_from_home,
                      name_address, m_patterns$raw_visit_counts, m_patterns$raw_visitor_counts, m_patterns$visit_home_cbgs, m_patterns$visitor_home_cbgs)
  colnames(m_patterns) <- c("location_name", "street_address", "city", "state", "zip_code", "distance_from_home", "name_address", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs")
  
  m_patterns <- data.frame(m_patterns, stringsAsFactors = FALSE)
  
  return(m_patterns)
  
}