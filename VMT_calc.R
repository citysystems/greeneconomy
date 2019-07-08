  ### Uploading libraries.

library(tidycensus)
library(censusapi)
library(tigris)
library(dplyr)
library(ggplot2)
library(sf)
library(osrm)
library(mapview)
library(readr)
library(ggmap)
library(censusr)
library(maptools)
library(rgeos)

  ### Import files.

patterns_choice <- source("C:/Users/Derek/Desktop/Stockton_R_code/VMT_Calculation_new/patterns_choice.R")

  ### Using the "tigris" package for use of shape files.

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

  ### Using the census API to create block groups and use census information in Stockton.

census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab", overwrite = TRUE)
acs5_17 <- load_variables(2017, "acs5")
ca_bgs <- block_groups("CA", cb = TRUE)

  ### Uploading safegraph Places .csv file.

safegraphplaces <- read.csv("C:/Users/Derek/Desktop/safegraph/poi/safegraphplaces.csv", header=TRUE, stringsAsFactors = FALSE)

  ### Data cleansing.

##########

safegraphplaces <- select(safegraphplaces, c("safegraph_place_id", "location_name", "latitude", "longitude", "street_address", "city", "state", "zip_code"))
safegraphplaces <- filter(safegraphplaces, city == 'stockton')

name_address <- paste(safegraphplaces$location_name, safegraphplaces$street_address, safegraphplaces$city, safegraphplaces$state, safegraphplaces$zip_code, sep = ", ")
safegraphplaces <- cbind(safegraphplaces$safegraph_place_id, safegraphplaces$longitude, safegraphplaces$latitude, name_address)
colnames(safegraphplaces) <- c("safegraph_place_id", "longitude", "latitude", "name_address")

safegraphplaces <- data.frame(safegraphplaces, stringsAsFactors = FALSE)

##########

  ### Downloading Safegraph patterns .csv files.

##########

  ### m_1_patterns

patterns_text <- patterns_choice(num)

m_1_patterns <- read.csv(patterns_text, header=TRUE, stringsAsFactors = FALSE)

##########

  ### Data cleansing.

##########

m_1_patterns <- select(m_1_patterns, c("location_name", "street_address", "city", "state", "zip_code", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visitor_home_cbgs", "visit_home_cbgs"))
m_1_patterns <- filter(m_1_patterns, city == 'stockton')

name_address <- paste(m_1_patterns$location_name, m_1_patterns$street_address, m_1_patterns$city, m_1_patterns$state, m_1_patterns$zip_code, sep = ", ")
m_1_patterns <- cbind(m_1_patterns$location_name, m_1_patterns$street_address, m_1_patterns$city, m_1_patterns$state, m_1_patterns$zip_code, m_1_patterns$distance_from_home,
                      name_address, m_1_patterns$raw_visit_counts, m_1_patterns$raw_visitor_counts, m_1_patterns$visit_home_cbgs, m_1_patterns$visitor_home_cbgs)
colnames(m_1_patterns) <- c("location_name", "street_address", "city", "state", "zip_code", "distance_from_home", "name_address", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs")

m_1_patterns <- data.frame(m_1_patterns, stringsAsFactors = FALSE)

# Joining the patterns data with the safegraph data.

m_1_patterns_join <- left_join(m_1_patterns, safegraphplaces, by = "name_address", copy = FALSE, suffix = c(".x", ".y"))
m_1_patterns_join <- m_1_patterns_join[!is.na(m_1_patterns_join$longitude), ] # Need to do something with this data, rather than just neglect it.
m_1_patterns_join <- select(m_1_patterns_join, c("safegraph_place_id", "name_address", "longitude", "latitude", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs"))

##########

  ### Filtering of data to disaggregate the "origin" of the safegraph trips.

##########

  ### Looping through the data to disaggregate the census blocks from the monthly patterns .csv files.

m_1_patterns_new <- data.frame(stringsAsFactors = FALSE)

for(counter1 in 1:nrow(m_1_patterns_join)){

  string_visit <- m_1_patterns_join$visit_home_cbgs[counter1]
  string_visit <- substring(string_visit, 3)
  string_visit <- substr(string_visit,1,nchar(string_visit)-1)
  string_visit <- strsplit(string_visit, split = ",\"")[[1]]
  
  string_visitor <- m_1_patterns_join$visitor_home_cbgs[counter1]
  string_visitor <- substring(string_visitor, 3)
  string_visitor <- substr(string_visitor,1,nchar(string_visitor)-1)
  string_visitor <- strsplit(string_visitor, split = ",\"")[[1]]
  
  matrix_visit <- data.frame(matrix(data = NA, nrow = length(string_visit), ncol = 2))
  colnames(matrix_visit) <- c("block_id", "visit_count")
  
  matrix_visitor <- data.frame(matrix(data = NA, nrow = length(string_visitor), ncol = 2))
  colnames(matrix_visitor) <- c("block_id", "unique_visitor_count")
  
  m_1_patterns_holder <- data.frame(stringsAsFactors = FALSE)
  
  if(nrow(matrix_visit) > 0){
  
    for(counter2 in 1:nrow(matrix_visit)){
    
      matrix_visit[counter2, 1] <- as.numeric(substr(string_visit[counter2], 1, 12))
      matrix_visit[counter2, 2] <- as.numeric(substr(string_visit[counter2], 15, 30))

      matrix_visitor[counter2, 1] <- as.numeric(substr(string_visitor[counter2], 1, 12))
      matrix_visitor[counter2, 2] <- as.numeric(substr(string_visitor[counter2], 15, 30))

      amenity <- m_1_patterns_join[counter1, 1:6]
      m_1_patterns_holder <- rbind(m_1_patterns_holder, amenity)
    
    }
  
  }
  
  matrix_m_1 <- left_join(matrix_visit, matrix_visitor, by = "block_id")
  matrix_m_1_tot <- cbind(m_1_patterns_holder, matrix_m_1)
  m_1_patterns_new <- rbind(m_1_patterns_new, matrix_m_1_tot)
  
}

  ### m_1 from "home_panel_summary"

m_1 <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_1.csv", header=TRUE, stringsAsFactors = FALSE)
m_1 <- filter(m_1, month == 1, state == "ca")
m_1 <- select(m_1, c("census_block_group", "number_devices_residing"))
colnames(m_1) <- c("block_id", "number_devices_residing")

  ### Creation of the "m_1_origins" and "m_1_dest" variables. 

# Origin Points for Trips (for osrmRoute)
m_1_origin <- select(m_1_patterns_new, c("safegraph_place_id", "block_id"))

zeroPaste <- paste(matrix(data = '0', nrow = length(m_1_patterns_new$block_id), ncol = 1))
m_1_origin$GEOID <- paste(zeroPaste, paste(m_1_origin$block_id), sep = "")
m_1_origin_centroids <- st_centroid(ca_bgs[which(ca_bgs$GEOID %in% m_1_origin$GEOID),])
m_1_origin_centroids <- select(m_1_origin_centroids, c("GEOID", "geometry"))

m_1_origin <- left_join(m_1_origin, m_1_origin_centroids, by = "GEOID")
m_1_origin <- select(m_1_origin, c("GEOID", "safegraph_place_id", "geometry"))
colnames(m_1_origin) <- c("origin", "destination", "geometry_origin")
m_1_origin_sf <- st_as_sf(m_1_origin)
m_1_origin_sf <- st_set_crs(m_1_origin_sf, "+proj=longlat +datum=WGS84 +nodefs")

# Destination Points for Trips (for osrmRoute)
m_1_dest <- select(m_1_patterns_new, c("safegraph_place_id", "longitude", "latitude"))
colnames(m_1_dest) <- c("safegraph_place_id", "longitude", "latitude")
m_1_dest <- cbind(m_1_dest, select(m_1_origin, c("GEOID")))
colnames(m_1_dest) <- c("destination", "longitude_dest", "latitude_dest", "origin")
m_1_dest <- select(m_1_dest, c("origin", "destination", "longitude_dest", "latitude_dest"))
m_1_dest_sf <- st_as_sf(m_1_dest, coords = c("longitude_dest", "latitude_dest"), crs = 4326)
# colnames(m_1_dest_sf) <- c("origin", "destination", "geometry_destination")
m_1_dest_sf <- st_set_crs(m_1_dest_sf, "+proj=longlat +datum=WGS84 +nodefs")

# Origin Points for Locations
m_1_origin_matrix <- select(m_1_patterns_new, c("safegraph_place_id", "name_address", "block_id", "visit_count", "unique_visitor_count"))
m_1_origin_matrix <- left_join(m_1_origin_matrix, m_1, by = "block_id")

zeroPaste <- paste(matrix(data = '0', nrow = length(m_1_patterns_new$block_id), ncol = 1))
m_1_origin_matrix$GEOID <- paste(zeroPaste, paste(m_1_origin_matrix$block_id), sep = "")
m_1_origin_centroids <- st_centroid(ca_bgs[which(ca_bgs$GEOID %in% m_1_origin_matrix$GEOID),])
m_1_origin_centroids <- select(m_1_origin_centroids, c("GEOID", "geometry"))

m_1_origin_matrix <- left_join(m_1_origin_matrix, m_1_origin_centroids, by = "GEOID")
m_1_origin_matrix <- select(m_1_origin_matrix, c("GEOID", "visit_count", "unique_visitor_count", "number_devices_residing", "safegraph_place_id", "name_address", "geometry"))
colnames(m_1_origin_matrix)[1] <- "origin"; colnames(m_1_origin_matrix)[5] <- "destination"; colnames(m_1_origin_matrix)[6] <- "destination_address"; colnames(m_1_origin_matrix)[7] <- "geometry_origin";
m_1_origin_matrix_sf <- st_as_sf(m_1_origin_matrix)
m_1_origin_matrix_sf <- st_set_crs(m_1_origin_matrix_sf, "+proj=longlat +datum=WGS84 +nodefs")

# Destination Points for Location
m_1_dest_matrix <- select(m_1_patterns_join, c("safegraph_place_id", "name_address", "longitude", "latitude", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "visit_home_cbgs", "visitor_home_cbgs"))
colnames(m_1_dest_matrix) <- c("destination", "name_address", "longitude", "latitude", "distance_from_home", "raw_visit_counts", "raw_visitor_counts", "origin_visit_cbgs", "origin_visitor_cbgs")
m_1_dest_matrix_sf <- st_as_sf(m_1_dest_matrix, coords = c("longitude", "latitude"), crs = 4326)
m_1_dest_matrix_sf <- st_set_crs(m_1_dest_matrix_sf, "+proj=longlat +datum=WGS84 +nodefs")

##########

  ### Reperforming Derek's time & distance prep.

##########

OD_time_dist <- do.call(rbind,lapply(1:nrow(m_1_patterns_new),function(row){
  osrmRoute(src = m_1_origin_sf[row, ],
            dst = m_1_dest_sf[row, ], overview = FALSE)
}))

##########

  ### Finding the census population for each block group goint to amenities in Stockton.

##########

  # Side Note: Is there an easier way of doing this? Can I apply the getCensus() funciton to all elements in a row at one time?

pop_bg_stockton <- getCensus(name = "acs/acs5", vintage = 2017, key = "c8aa67e4086b4b5ce3a8717f59faa9a28f611dab",
                   vars = c("B00001_001E"), region = "block group:*", regionin = "state:06+county:077")

### Delete below.

pop_Stockton_bg <- data.frame(stringsAsFactors = FALSE)

for(counterBG in 1:nrow(m_1_origin_matrix_sf)){
  
  string_bg <- m_1_origin_matrix_sf$origin[counterVMT]
  
  state <- substr(string_bg, 1, 2)
  county <- substr(string_bg, 3, 5)
  tract <- substr(string_bg, 6, 11)
  bg <-  substr(string_bg, 12, 12)
  
  reg <- paste("block group:", bg, sep = "")
  regIn <- paste("state:", state, "+county:", county, "+tract:", tract, sep = "")
  
  pop_bg <- getCensus(name = "acs/acs5", vintage = 2017, key = "c8aa67e4086b4b5ce3a8717f59faa9a28f611dab",
            vars = c("B00001_001E"), region = reg, regionin = regIn)
  
  pop_Stockton_bg <- rbind(pop_Stockton_bg, pop_bg)
  
}

m_1_origin_matrix_calc <- cbind(m_1_origin_matrix_sf[, 1:4], pop_Stockton_bg$bg_population, m_1_origin_matrix_sf[, 5:6])
m_1_origin_matrix_calc$geometry_origin.1 <- NULL
colnames(m_1_origin_matrix_calc)[5] <-  "pop_Stockton_bg"

##########

  ### Calculation of NHTS nudge values for the VMT calculations.

##########

NHTS_df_final <- read.csv(file = "C:/Users/Derek/Desktop/Stockton_R_code/VMT_Calculation/df_final.csv", header = TRUE)
# NHTS_df_final <- NHTS_df_final[NHTS_df_final$trptransfilt == 4, ]
# NHTS_df_final <- NHTS_df_final[NHTS_df_final$tdaydate == 201701, ]

NHTS_wghtAvg <- sum(NHTS_df_final$trpmiles * NHTS_df_final$wttrdfin) / sum(NHTS_df_final$wttrdfin)
NHTS_medVal <- as.numeric(median(rep(NHTS_df_final$trpmiles, round(NHTS_df_final$wttrdfin))))
  
NHTS_MeanMedConv <- NHTS_wghtAvg / NHTS_medVal # Need to filter by "trptransmode" and use of variables for the network distance (e.g., TRPMILES or VMT_MILES).
  
NHTS_LinkedTripsConv <- 1

##########

  ### Calculation of the VMTs per origin, per destination, and in total.
  ### "as-crow-flies" (ACF) calculation for origin-destination pairs.

##########

dest_amenities_matrix <- data.frame(unique(m_1_origin_matrix_calc$destination_address))
colnames(dest_amenities_matrix) <- "name_address"
dest_num <- nrow(dest_amenities_matrix)

count_dist_start <- 1
count_dist_end <- 0

distance <- OD_time_dist[, "distance"]

VMT_perOrigin <- data.frame(stringsAsFactors = FALSE)
VMT_perDest <- data.frame(stringsAsFactors = FALSE)

m_1_dest_matrix_sf$VMT <- NA

for(counterVMT in 1:dest_num){
  
  origin_matrix <- m_1_origin_matrix_calc[m_1_origin_matrix_calc$destination_address == dest_amenities_matrix[counterVMT, ], ]
  dest_vector <- m_1_dest_matrix_sf[m_1_dest_matrix_sf$name_address == dest_matrix[counterVMT, ], ]
  # dest_matrix_data <- m_1_dest_matrix_sf[m_1_dest_matrix_sf$name_address == dest_amenities_matrix[counterVMT, ], ]
  
  ACF_safegraph <- as.numeric(dest_vector$distance_from_home)
  ACF_OD <- as.numeric(median(st_distance(origin_matrix, dest_vector)))
  
  count_dist_end <- count_dist_end + nrow(origin_matrix)
  
  distance_new <- distance[count_dist_start:count_dist_end] * ACF_safegraph / ACF_OD * NHTS_MeanMedConv * NHTS_LinkedTripsConv
  
  VMT_census <- origin_matrix$visit_count * distance_new * origin_matrix$pop_Stockton_bg / origin_matrix$number_devices_residing
  VMT_perOrigin <- rbind(VMT_perOrigin, data.frame(VMT_census))
  
  VMT_census_sum <- sum(VMT_census)
  VMT_scaleFullPop <- as.numeric(dest_vector$raw_visit_counts) / sum(origin_matrix$visit_count)
  VMT_dest <- VMT_scaleFullPop * VMT_census_sum

  count_dist_start <- count_dist_start + nrow(origin_matrix)
  
  m_1_dest_matrix_sf[m_1_dest_matrix_sf$name_address == dest_matrix[counterVMT, ], "VMT"] <- VMT_dest

  VMT_perDest <- rbind(VMT_perDest,VMT_dest)
    
}

colnames(VMT_perOrigin) <- "VMT_perOrigin"
colnames(VMT_perDest) <- "VMT_perDest"

VMT_Avg <- mean(VMT_perDest$VMT_perDest)
m_1_dest_matrix_sf[is.na(m_1_dest_matrix_sf$VMT), "VMT"] <- VMT_Avg
VMT_tot <- sum(m_1_dest_matrix_sf$VMT)

##########


##################################################

# Other inputs into R.

##########

  ### More Safegraph data being input.

### m_2_patterns

m_2_patterns <- read.csv("C:/Users/Derek/Desktop/safegraph/m_2_patterns.csv", header=TRUE, stringsAsFactors = FALSE)
m_2_patterns <- m_2_patterns[m_2_patterns$city == "stockton", ]

m_3_patterns <- read.csv("C:/Users/Derek/Desktop/safegraph/m_3_patterns.csv", header=TRUE, stringsAsFactors = FALSE)
m_3_patterns <- m_3_patterns[m_3_patterns$city == "stockton", ]

m_4_patterns <- read.csv("C:/Users/Derek/Desktop/safegraph/m_4_patterns.csv", header=TRUE, stringsAsFactors = FALSE)
m_4_patterns <- m_4_patterns[m_4_patterns$city == "stockton", ]

m_5_patterns <- read.csv("C:/Users/Derek/Desktop/safegraph/m_5_patterns.csv", header=TRUE, stringsAsFactors = FALSE)
m_5_patterns <- m_5_patterns[m_5_patterns$city == "stockton", ]

m_6_patterns <- read.csv("C:/Users/Derek/Desktop/safegraph/m_6_patterns.csv", header=TRUE, stringsAsFactors = FALSE)
m_6_patterns <- m_6_patterns[m_6_patterns$city == "stockton", ]

m_7_patterns <- read.csv("C:/Users/Derek/Desktop/safegraph/m_7_patterns.csv", header=TRUE, stringsAsFactors = FALSE)
m_7_patterns <- m_7_patterns[m_7_patterns$city == "stockton", ]

m_8_patterns <- read.csv("C:/Users/Derek/Desktop/safegraph/m_8_patterns.csv", header=TRUE, stringsAsFactors = FALSE)
m_8_patterns <- m_8_patterns[m_8_patterns$city == "stockton", ]

m_9_patterns <- read.csv("C:/Users/Derek/Desktop/safegraph/m_9_patterns.csv", header=TRUE, stringsAsFactors = FALSE)
m_9_patterns <- m_9_patterns[m_9_patterns$city == "stockton", ]

m_10_patterns <- read.csv("C:/Users/Derek/Desktop/safegraph/m_10_patterns.csv", header=TRUE, stringsAsFactors = FALSE)
m_10_patterns <- m_10_patterns[m_10_patterns$city == "stockton", ]

m_11_patterns <- read.csv("C:/Users/Derek/Desktop/safegraph/m_11_patterns.csv", header=TRUE, stringsAsFactors = FALSE)
m_11_patterns <- m_11_patterns[m_11_patterns$city == "stockton", ]

m_12_patterns <- read.csv("C:/Users/Derek/Desktop/safegraph/m_12_patterns.csv", header=TRUE, stringsAsFactors = FALSE)
m_12_patterns <- m_12_patterns[m_12_patterns$city == "stockton", ]

##########

# Forget about everything under here.

##########

stockton_lodes_h <- cbind(stockton_lodes_h, prep)

stockton_lodes_h <- stockton_lodes_h %>% mutate(COUNTY = substr(w_bg,3,5))

stockton_lodes_w_counties <- stockton_lodes_h %>% mutate(COUNTY = substr(w_bg,3,5), person_miles = S000*as.numeric(distance)/1.60934, person_hours = S000*as.numeric(duration)/60) %>% group_by(COUNTY) %>% summarise_at(c("S000","SA01","SA02","SA03","SE01","SE02","SE03","SI01","SI02","SI03","person_miles", "person_hours"), sum) %>% mutate(avg_distance = person_miles/S000, avg_duration = person_hours/S000, person_miles_rm_excessive = ifelse(avg_duration < 3, person_miles, 0), `Percent High Wage Jobs in County` = SE03/S000, `Percent High Wage Jobs Overall` = SE03/sum(SE03,na.rm = TRUE), `Percent Total Jobs` = S000/sum(S000,na.rm = TRUE), `Percent VMT` = person_miles_rm_excessive/sum(person_miles_rm_excessive,na.rm = TRUE),`GHG Annual` = (person_miles_rm_excessive*0.82*2+person_miles_rm_excessive*.116/2*2)*369.39*0.00035812, `Percent GHG` = `GHG Annual`/sum(`GHG Annual`), `Average GHG` = `GHG Annual`/S000) %>% rename(Jobs = S000, `Average Distance` = avg_distance)

stockton_lodes_w_counties <- ca_counties %>% select(COUNTYFP, NAME) %>% left_join(stockton_lodes_w_counties, by = c("COUNTYFP" = "COUNTY"))

stockton_lodes_w_top_counties <- stockton_lodes_w_counties %>% filter(NAME %in% c("San Joaquin", "Alameda", "Sacramento", "Santa Clara","Stanislaus","Contra Costa","San Francisco","San Mateo","Solano","Fresno","Placer","Yolo","Monterey","Sonoma","Merced")) %>% arrange(desc(Jobs))

m1 <- mapview(stockton_lodes_w_top_counties, zcol=c("Jobs","Percent High Wage Jobs in County","Average Distance","Average GHG","Percent GHG"), map.types = c("OpenStreetMap"), legend = TRUE, hide = TRUE)
m1
mapshot(m1, url = "stockton_lodes_w_top_counties.html")
l1 <- addStaticLabels(m1, label = stockton_lodes_w_top_counties$NAME)

sum(stockton_lodes_w_counties$S000, na.rm = TRUE)
sum(stockton_lodes_w_counties$person_miles, na.rm = TRUE)
mapview(stockton_lodes_w_counties, zcol='avg_distance')

stockton_lodes_h_summary <- stockton_lodes_h %>% mutate(person_miles = S000*as.numeric(distance)/1.60934, person_hours = S000*as.numeric(duration)/60) %>% group_by(h_bg) %>% summarise_at(c("S000","SA01","SA02","SA03","SE01","SE02","SE03","SI01","SI02","SI03","person_miles", "person_hours"), sum)

write_csv(stockton_lodes_w_counties, "C:\\Users\\Derek Ouyang\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes_w_counties.csv")

# save(stockton_lodes_w, stockton_lodes_h, stockton_rac, stockton_wac, stockton_lodes_summary, prep, file = "C:\\Users\\Derek Ouyang\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes.R")
load("C:\\Users\\Derek Ouyang\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes.R")

stockton_bgs <- stockton_bgs %>% geo_join(stockton_lodes_summary, "GEOID", "h_bg") 

stockton_bgs$rank <- NULL

stockton_bgs$avg_distance <- stockton_bgs$person_miles/stockton_bgs$S000
stockton_bgs$avg_duration <- stockton_bgs$person_hours/stockton_bgs$S000

mapview(stockton_boundary, alpha.regions = 0, lwd = 2) + mapview(stockton_bgs, zcol='avg_distance') 

mapview(stockton_rac, zcol='C000')

mapview(stockton_wac, zcol='C000')

mapshot(mapview(epa_blocks, alpha.regions = 0, lwd = 2) + mapview(epa_parcels, zcol='HAS_AAL'), url = "epamap.html")

plot(stockton_bgs["S000"])

stockton_rac$perc_low_wage <- stockton_rac$CE01/stockton_rac$C000
stockton_wac$perc_low_wage <- stockton_wac$CE01/stockton_wac$C000

mapview(stockton_rac, zcol='perc_low_wage')
mapview(stockton_wac, zcol='perc_low_wage')
plot(stockton_wac["perc_low_wage"])
plot(stockton_rac["perc_low_wage"])

stockton_lodes_dest_convert <- stockton_lodes_dest_bg %>% geo_join(stockton_lodes_h, "GEOID", "w_bg") %>% st_set_geometry(NULL) %>% group_by(COUNTYFP) %>% summarize(jobs = sum(S000))
stockton_lodes_dest_county <- 
  
  mapview(stockton_boundary, alpha.regions = 0, lwd = 2) +
  mapview(stockton_lodes_dest_bg, zcol='S000')
