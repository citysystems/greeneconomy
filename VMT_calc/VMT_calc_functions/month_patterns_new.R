### Looping through the data to disaggregate the census blocks from the monthly patterns .csv files.

month_patterns_new <- function(month_patterns_join, pop_blockgroup_stockton){
  
  m_patterns_new <- data.frame(stringsAsFactors = FALSE)
  
  month_patterns_join <- unique(dplyr::select(month_patterns_join, c("location_name", "name_address", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs")))
  month_patterns_join$safegraph_place_id <- NA; month_patterns_join$longitude <- NA; month_patterns_join$latitude <- NA
  month_patterns_join <- dplyr::select(month_patterns_join, c("safegraph_place_id", "location_name", "name_address", "longitude", "latitude",
                                                 "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs"))
  
  ### This first for loop breaks up all of the origin block groups from one string to individual strings.
  
  for(counter1 in 1:nrow(month_patterns_join)){
    
    string_visit <- month_patterns_join$visit_home_cbgs[counter1]
    string_visit <- substring(string_visit, 3)
    string_visit <- substr(string_visit,1,nchar(string_visit)-1)
    string_visit <- strsplit(string_visit, split = ",\"")[[1]]
  
    string_visitor <- month_patterns_join$visitor_home_cbgs[counter1]
    string_visitor <- substring(string_visitor, 3)
    string_visitor <- substr(string_visitor,1,nchar(string_visitor)-1)
    string_visitor <- strsplit(string_visitor, split = ",\"")[[1]]
  
    matrix_visit <- data.frame(matrix(data = NA, nrow = length(string_visit), ncol = 2))
    colnames(matrix_visit) <- c("block_id", "visit_count")
  
    matrix_visitor <- data.frame(matrix(data = NA, nrow = length(string_visitor), ncol = 2))
    colnames(matrix_visitor) <- c("block_id", "unique_visitor_count")
  
    m_patterns_holder <- data.frame(stringsAsFactors = FALSE)
  
    if(nrow(matrix_visit) > 0){

      ### This second for loop breaks up each individual block group string
      ### into the block group and either the visit or visitor count.
          
      for(counter2 in 1:nrow(matrix_visit)){
      
        matrix_visit[counter2, 1] <- as.numeric(substr(string_visit[counter2], 1, 12))
        matrix_visit[counter2, 2] <- as.numeric(substr(string_visit[counter2], 15, 30))
        
        matrix_visitor[counter2, 1] <- as.numeric(substr(string_visitor[counter2], 1, 12))
        matrix_visitor[counter2, 2] <- as.numeric(substr(string_visitor[counter2], 15, 30))
        
        ### Some of the Safegraph lines of code have destinations without geocodes.
        ### These lines of code check if the destination being considered has a latlon geocode.
        ### If these lines of code do not have a latlon code, then the following geocodes them in.
        
        # if(is.na(month_patterns_join[counter1, "longitude"])){
        #   
        #   resdf <- geocodeSL(month_patterns_join[counter1, "name_address"])
        #   month_patterns_join[counter1, "longitude"] <- resdf["lon"]
        #   month_patterns_join[counter1, "latitude"] <- resdf["lat"]
        #   
        # }

        amenity <- month_patterns_join[counter1, 1:6]
        
        m_patterns_holder <- rbind(m_patterns_holder, amenity)
        
      }
    
    }
  
    ### This if-statement checks to see if any of the block groups being analyzed are within 
    ### the Stockton boundary. If so, these block groups into the m_patterns_new matrix.
    ### Otherwise, don't take these block groups into consideration.
    
    bgs_origin <- paste("0", as.character(matrix_visit$block_id), sep = "")
  
    if( any(bgs_origin %in% pop_blockgroup_stockton$origin) ){
    
      matrix_m <- left_join(matrix_visit, matrix_visitor, by = "block_id")
      matrix_m_tot <- cbind(m_patterns_holder, matrix_m)
      m_patterns_new <- rbind(m_patterns_new, matrix_m_tot)
    
    }
  
  }
  
  return(m_patterns_new)
  
}