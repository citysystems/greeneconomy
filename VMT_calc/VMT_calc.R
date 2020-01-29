# Uploading approprte libraries.

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
library(microbenchmark)

# Using the "tigris" package for use of shape files.

# Local server to run the st_intersects package at a faster rate.
options(osrm.server = "http://127.0.0.1:5000/")

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# Import files.

NHTS_df_final <- read.csv(file = "C:/Users/Derek/Desktop/VMT_calc_important_files/CSVdata_files/NHTS_df_final.csv", header = TRUE)

source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/patterns_choice.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/safegraphplaces_cleanse.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/m_patterns_cleanse.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/m_patterns_join_cleanse.R")
# source("C:/Users/Derek/Desktop/VMT_calc_functions/home_panel_summary.R")
# source("C:/Users/Derek/Desktop/VMT_calc_functions/pop_blockgroup_stockton.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/month_patterns_new.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/origin_trips.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/dest_trips.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/origin_locations.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/dest_locations.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/geocodeSL.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/nudge_MeanMedConv.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/nudge_LinkedTrips.R")
source("C:/Users/Derek/Desktop/VMT_calc_important_files/VMT_calc_functions/nudge_CarpoolVehicleFilter.R")

# Using the census API to create block groups and use census information in Stockton.
census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab", overwrite = TRUE)
ca_bgs <- block_groups("CA", cb = TRUE)

# Uploading safegraph places .csv file & data cleansing.
safegraphplaces_SJ <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/safegraphplaces.csv", header=TRUE, stringsAsFactors = FALSE)
safegraphplaces <- safegraphplaces_cleanse(safegraphplaces_SJ)

safegraphplaces_CA <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/safegraphplaces_ca_edit.csv", header=TRUE, stringsAsFactors = FALSE)
safegraphplaces_CA_edit <- safegraphplaces_cleanse(safegraphplaces_CA)

# safegraphplaces_tot <- rbind(safegraphplaces_SJ, safegraphplaces_CA)
# safegraphplaces <- safegraphplaces_cleanse(safegraphplaces_tot)

##########

# Computation of the "drive" O-D matrix for all of Stockton's 205 block groups
# (within the boundary of indluence) to all possible safegraph destinations.  

# The following code uploads the data from the CCF package and uses this data to answer the questions below.
### load("G:/My Drive/Stanford City Systems - Greg's Summer Work/00 Complete Communities Framework (CCF)/03 CCF Survey/month_patterns_join_all.RData")
### load("G:/My Drive/Stanford City Systems - Greg's Summer Work/00 Complete Communities Framework (CCF)/03 CCF Survey/pre_survey.RData")
load("C:/Users/Derek/Desktop/CCF_survey_folder/amenities_persubcat_perblockgroup.RData")
load("C:/Users/Derek/Desktop/CCF_survey_folder/ca_blockgroups_pop.RData")

# The following code uploads scripts from the Stockton Green Economt Project - VMT Analysis.
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/home_panel_summary.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/pop_blockgroup_stockton.R")

# Testing of the two possible OSRM approaches: (1) OSRM Isochrones and (2) OSRM Routing

# Uploading the South Stockton Promise Zone (SSPZ) and the Stockton Spheres of Influence. 
sspzboundary <- st_read("C:/Users/Derek/Desktop/VMT_calc_important_files/sspzboundary/sspzboundary.shp") %>% st_transform(st_crs(4326))
stockton_boundary_influence <- (st_read("C:/Users/Derek/Desktop/VMT_calc_important_files/SpheresOfInfluence/SpheresOfInfluence.shp") %>% filter(SPHERE == "STOCKTON") %>% st_transform(st_crs(4326)))[1,]

# Taking the block groups within Stockton and filtering them by those within the following:
# (1) Stockton Boundary of Influence (SBOI)
# (2) Stockton South Promise Zone (SSPZ)

sjc_bgs <- block_groups("California", "San Joaquin County", cb = TRUE) %>% st_transform(st_crs(4326))

sboi_bgs <- sjc_bgs[which(sjc_bgs$GEOID %in% st_centroid(sjc_bgs)[stockton_boundary_influence,]$GEOID),]
sboi_centroid <- st_centroid(sboi_bgs)

# Use only for when analyzing the South Stockton Promise Zone.

# sspz_bgs <- sjc_bgs[which(sjc_bgs$GEOID %in% st_centroid(sjc_bgs)[sspzboundary,]$GEOID),]
# sspz_centroid <- st_centroid(sspz_bgs)

# Uploading the San Joaquin safegraphplaces dataset.
# safegraphplaces <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/safegraphplaces.csv", header=TRUE, stringsAsFactors = FALSE)
# safegraphplaces <- safegraphplaces[!is.na(safegraphplaces$sub_category), ]
# safegraphplaces$full_address <- paste(safegraphplaces$location_name, safegraphplaces$street_address,
#                                       safegraphplaces$city, safegraphplaces$state, safegraphplaces$zip_code, sep = ", ")

##########
### osrmTable_Stockton_VMT <- do.call(rbind, lapply( 1:nrow(safegraphplaces), function(counterTable){

###  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = safegraphplaces[counterTable, c("safegraph_place_id", "longitude", "latitude")]) )

###  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, safegraphplaces[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
###  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")

###  return(safegraph_osrm)

### }))

### save(osrmTable_Stockton_VMT, file = "S:/CCF/OD_VMT_df/osrm_Stockton_VMT.RData")

load(file = "C:/Users/Derek/Desktop/osrmTable_Stockton_VMT_v2.RData")

##########

##########

### Calculation of NHTS nudge values for the VMT calculations.

### Nudge: Performing the linked trips conversion factor.
NHTS_LinkedTripsConv <- nudge_LinkedTrips(NHTS_df_final)

### Nudge: Peforming the median to mean converison.
NHTS_MeanMedConv <- nudge_MeanMedConv(NHTS_df_final)

### Nudge: Peforming the carpooling and vehicle vs. other transit mode nudge.
NHTS_carpoolVehicleFilter <- nudge_carpoolVehicleFilter(NHTS_df_final)

# Downloading Safegraph patterns .csv files.

for(num in 1:12){
  
  print(num)
  
  patterns_text <- patterns_choice(num)
  m_patterns <- m_patterns_cleanse(patterns_text)
  m_patterns_join <- m_patterns_join_cleanse(m_patterns, safegraphplaces)
  
  # m_hps from "home_panel_summary"
  m_hps <- home_panel_summary(num)
  
  # Finding the census population for each block group goint to amenities in Stockton.
  pop_bg_stockton <- pop_blockgroup_stockton(m_hps)
  
  # Looping through the data to disaggregate the census blocks from the monthly patterns .csv files.
  m_patterns_new <- month_patterns_new(m_patterns_join, pop_bg_stockton)
  
  # Origin Points for Locations
  # This includes finding the census population for each block group going to amenities in Stockton.
  m_origin_matrix_sf <- month_origin_matrix(m_patterns_new, m_hps, pop_bg_stockton)
  
  # Destination Points for Location
  m_dest_matrix_sf <- month_dest_matrix(m_patterns_join, safegraphplaces)
  
  ##########
  
  # You can include a potential function for geocoding, here.
  
  # The following includes some sample code, which should be put
  # in a for-loop to see whether there is a geo-code or not.
  
  # resdf <- geocodeSL(m_dest_matrix_sf[1, "name_address"])
  # m_dest_matrix_sf[, "longitude"] <- resdf["lon"]
  # m_dest_matrix_sf[, "latitude"] <- resdf["lat"]
  
  ##########
  
  filename <- paste("C:/Users/Derek/Desktop/m_patterns_new/", substr(patterns_text, 39, 51), "_new.RData", sep = "")
  
  save(m_origin_matrix_sf, m_dest_matrix_sf, m_patterns_new, file = filename)
  
  rm(m_origin_matrix_sf, m_dest_matrix_sf, m_patterns_new, filename)
  
}

##########

### Creation of the "m_origins" and "m_dest" variables.

# Origin Points for Trips (for osrmRoute)
#m_origin_sf <- month_origin(m_patterns_new)

# Destination Points for Trips (for osrmRoute)
#m_dest_sf <- month_dest(m_patterns_new, m_origin_sf)

##########

### osrmRoute time & distance prep.

##########

# OD_time_dist <- do.call(rbind,lapply(1:nrow(m_1_patterns_new),function(row){
#   osrmRoute(src = m_origin_sf[row, ],
#             dst = m_dest_sf[row, ], overview = FALSE)
# }))

##########

### Calculation of the VMTs per origin, per destination, and in total.
### "as-crow-flies" (ACF) calculation for origin-destination pairs.

##########

# Conversion factors

conv_MeterToMile <- 0.000621371
factor_twoWayTrip <- 2

# Constants to evaluate number of categories necessary for the VMT calculations.

months_in_year <- 12
VMT_considerations <- 3
months_considerations <- months_in_year * VMT_considerations

# Creation of the origin matrices for all of Stockton's 205 origins.
# Home panel summary of '1' was chosen. For this part of the analysis, it doesn't matter which number is chosen.
# All that is necessary is that Stockton's total population is chosen.

m_hps <- home_panel_summary(1)
pop_bg_stockton <- pop_blockgroup_stockton(m_hps)

VMT_Origin_recorded <- data.frame(matrix(0, nrow = nrow(pop_bg_stockton), ncol = 13))
VMT_Origin_nonrecorded <- data.frame(matrix(0, nrow = nrow(pop_bg_stockton), ncol = 13))
VMT_Origin_otherdest_nonrecorded <- data.frame(matrix(0, nrow = nrow(pop_bg_stockton), ncol = 13))

VMT_Origin_recorded[, 1] <- pop_bg_stockton$origin
VMT_Origin_nonrecorded[, 1] <- pop_bg_stockton$origin
VMT_Origin_otherdest_nonrecorded[, 1] <- pop_bg_stockton$origin

colnames_VMT <- c("origin",
                  "VMTs_m_1",
                  "VMTs_m_2",
                  "VMTs_m_3",
                  "VMTs_m_4",
                  "VMTs_m_5",
                  "VMTs_m_6",
                  "VMTs_m_7",
                  "VMTs_m_8",
                  "VMTs_m_9",
                  "VMTs_m_10",
                  "VMTs_m_11",
                  "VMTs_m_12"
)

colnames(VMT_Origin_recorded) <- colnames_VMT
colnames(VMT_Origin_nonrecorded) <- colnames_VMT
colnames(VMT_Origin_otherdest_nonrecorded) <- colnames_VMT

rownames(VMT_Origin_recorded) <- pop_bg_stockton$origin
rownames(VMT_Origin_nonrecorded) <- pop_bg_stockton$origin
rownames(VMT_Origin_otherdest_nonrecorded) <- pop_bg_stockton$origin

# Creating shapefiles that include (1) the Stockton boundary and (2) the Stockton boundary with a 1-mile buffer.

meters_to_miles <- 1609.34
stockton_boundary_influence <- (st_read("C:/Users/Derek/Desktop/VMT_calc_important_files/SpheresOfInfluence/SpheresOfInfluence.shp") %>% filter(SPHERE == "STOCKTON") %>% st_transform(st_crs(4326)))[1,]
stockton_boundary_influence <- st_transform(stockton_boundary_influence, crs = "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")
stockton_boundary_influence_milebuffer <- st_buffer(stockton_boundary_influence, meters_to_miles)

# Converting the coordinates reference system ("crs") back into its original form (the 4326 default) for the sake of the future analysis.

stockton_boundary_influence <- st_transform(stockton_boundary_influence, crs = 4326)
stockton_boundary_influence_milebuffer <- st_transform(stockton_boundary_influence_milebuffer, crs = 4326)

# Creating a blank shapefile data frame for the destinations of consideration.

locations_of_consideration <- st_sf(data.frame(matrix(data = NA, nrow = 0, ncol = 47), geom = st_sfc()), crs = 4326) # "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs"

# This is a matrix of the column names for the VMT types being calculated.

dest_col_names <- c("VMTs_recorded_m_1",
                    "VMTs_recorded_m_2",
                    "VMTs_recorded_m_3",
                    "VMTs_recorded_m_4",
                    "VMTs_recorded_m_5",
                    "VMTs_recorded_m_6",
                    "VMTs_recorded_m_7",
                    "VMTs_recorded_m_8",
                    "VMTs_recorded_m_9",
                    "VMTs_recorded_m_10",
                    "VMTs_recorded_m_11",
                    "VMTs_recorded_m_12",
                    "VMTs_nonrecorded_m_1",
                    "VMTs_nonrecorded_m_2",
                    "VMTs_nonrecorded_m_3",
                    "VMTs_nonrecorded_m_4",
                    "VMTs_nonrecorded_m_5",
                    "VMTs_nonrecorded_m_6",
                    "VMTs_nonrecorded_m_7",
                    "VMTs_nonrecorded_m_8",
                    "VMTs_nonrecorded_m_9",
                    "VMTs_nonrecorded_m_10",
                    "VMTs_nonrecorded_m_11",
                    "VMTs_nonrecorded_m_12",
                    "VMTs_nonrecorded_otherdest_m_1",
                    "VMTs_nonrecorded_otherdest_m_2",
                    "VMTs_nonrecorded_otherdest_m_3",
                    "VMTs_nonrecorded_otherdest_m_4",
                    "VMTs_nonrecorded_otherdest_m_5",
                    "VMTs_nonrecorded_otherdest_m_6",
                    "VMTs_nonrecorded_otherdest_m_7",
                    "VMTs_nonrecorded_otherdest_m_8",
                    "VMTs_nonrecorded_otherdest_m_9",
                    "VMTs_nonrecorded_otherdest_m_10",
                    "VMTs_nonrecorded_otherdest_m_11",
                    "VMTs_nonrecorded_otherdest_m_12",
                    "within_Stockton",
                    "within_StocktonBuffer"
)

# The following includes a vector of all of the cities within the San Joaquin county.
# I chose the city of Stockton and the bigger, more closer cities by Stockton.
# This was done manually by checking on Google Maps and can be more calculated in the future. However, Stockton is the significant city to include.

SanJoaquin_Cities_Vector <- unique(safegraphplaces_SJ$city)[c(3,5,6,12,18,23,31,37,47,52)]

##########

# save.image(file = "C:/Users/Derek/Desktop/VMT_analysis_start.RData")

# load(file = "C:/Users/Derek/Desktop/VMT_analysis_start.RData")

# This for loop analyses the VMT by origin and destination, per month.

for(counterMonth in 1:12){
  
  # Loading the m_patterns_new data. This includes "m_origin_matrix_sf", "m_dest_matrix_sf" and "m_patterns_new".
  # This is done for each of the 12 months of the year.
  
  print(counterMonth)
  
  patterns_text <- patterns_choice(counterMonth)
  m_patterns <- m_patterns_cleanse(patterns_text)
  filename <- paste("C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new", substr(patterns_text, 63, 76), "_new.RData", sep = "")
  load(filename)
  
  # Editing the "m_dest_matrix_sf" data frame. This involves adding adding additional columns,
  # changing the lat/long to be numeric and using new column names.
  
  m_dest_matrix_sf <- cbind( m_dest_matrix_sf, data.frame( matrix( NA, ncol = (months_considerations + 2) ) ) )
  m_dest_matrix_sf$longitude <- as.numeric( m_dest_matrix_sf$longitude )
  m_dest_matrix_sf$latitude <- as.numeric( m_dest_matrix_sf$latitude )
  colnames(m_dest_matrix_sf)[12:49] <- dest_col_names
  
  # Creating a vector of the unique destination names and a scalar value of the amount of destinations.
  
  dest_amenities_matrix <- data.frame( unique( m_origin_matrix_sf$full_address ) )
  colnames(dest_amenities_matrix) <- "name_address"
  dest_num <- nrow(dest_amenities_matrix)
  
  # Adding these locations of interest to a holding variable. Moreover, to make sure the analysis
  # is done on a monthly basis, I calculate which rows are those are the start and end of each month
  # as done with the "start_counter" and "end_counter" variables.
  
  start_counter <- nrow(locations_of_consideration)
  
  m_dest_matrix_sf_temp <- left_join(m_dest_matrix_sf, m_patterns[,c("city", "state", "full_address")], by = "full_address")
  m_dest_matrix_sf_temp_latlongNA <- m_dest_matrix_sf_temp[m_dest_matrix_sf_temp$city %in% SanJoaquin_Cities_Vector & m_dest_matrix_sf_temp$state == "ca" & is.na(m_dest_matrix_sf_temp$longitude), "full_address"]
  
  for (missing_latlong_index in m_dest_matrix_sf_temp_latlongNA){
    
    resdf <- geocodeSL(m_dest_matrix_sf[m_dest_matrix_sf$full_address == missing_latlong_index, "name_address"])
    
    m_dest_matrix_sf[m_dest_matrix_sf$full_address == missing_latlong_index, "longitude"] <- as.numeric(as.character(resdf["lon"]))
    m_dest_matrix_sf[m_dest_matrix_sf$full_address == missing_latlong_index, "latitude"] <- as.numeric(as.character(resdf["lat"]))
    
  }
  
  print("missing_latlong_index")
  
  locations_of_consideration <- rbind(locations_of_consideration, st_sf(st_as_sf(m_dest_matrix_sf[!is.na(m_dest_matrix_sf$longitude), ],
                                                                                 coords = c("longitude", "latitude"), crs = 4326) ) )
  
  pre_end_counter <- nrow( locations_of_consideration )
  
  non_sf_SJ_dest <- m_dest_matrix_sf[m_dest_matrix_sf$full_address %in% as.character(dest_amenities_matrix[!(dest_amenities_matrix$name_address %in% locations_of_consideration[(start_counter+1):pre_end_counter,]$full_address),]),]
  non_sf_SJ_dest$longitude <- NULL
  non_sf_SJ_dest$latitude <- NULL
  non_sf_SJ_dest_latlong <- left_join(non_sf_SJ_dest[,1:9], safegraphplaces_CA_edit[, c("longitude", "latitude", "full_address")], by = "full_address")
  non_sf_SJ_dest_latlong <- cbind(non_sf_SJ_dest_latlong, non_sf_SJ_dest[,10:47])
  
  for (latlong_index in 1:nrow(non_sf_SJ_dest_latlong)){
    
    if(is.na(non_sf_SJ_dest_latlong[latlong_index,"longitude"])){
      
      resdf <- geocodeSL(non_sf_SJ_dest_latlong[latlong_index, "name_address"])
      
      non_sf_SJ_dest_latlong[latlong_index, "longitude"] <- as.numeric(as.character(resdf["lon"]))
      non_sf_SJ_dest_latlong[latlong_index, "latitude"] <- as.numeric(as.character(resdf["lat"]))
      
      # non_sf_SJ_dest_latlong[latlong_index, "longitude"] <- as.numeric(non_sf_SJ_dest_latlong[latlong_index, "longitude"])
      # non_sf_SJ_dest_latlong[latlong_index, "latitude"] <- as.numeric(non_sf_SJ_dest_latlong[latlong_index, "latitude"])
      
    }
    
  }
  
  non_sf_SJ_dest_latlong$longitude <- as.numeric(non_sf_SJ_dest_latlong$longitude)
  non_sf_SJ_dest_latlong$latitude <- as.numeric(non_sf_SJ_dest_latlong$latitude)
  
  non_sf_SJ_dest_latlong_sf <- st_sf(st_as_sf(non_sf_SJ_dest_latlong, coords = c("longitude", "latitude"), crs = 4326) )
  
  locations_of_consideration <- rbind(locations_of_consideration, non_sf_SJ_dest_latlong_sf)
  
  end_counter <- nrow( locations_of_consideration )
  
  # Creation of the column values that disclose whether a destination is within Stockton and/or its 1-mile buffer.
  
  withinStockton_rownum <- st_intersects(stockton_boundary_influence, locations_of_consideration[(start_counter + 1):end_counter, "geometry"], sparce = TRUE)[[1]]
  withinStocktonBuffer_rownum <- st_intersects(stockton_boundary_influence_milebuffer, locations_of_consideration[(start_counter + 1):end_counter, "geometry"], sparce = TRUE)[[1]]
  
  locations_of_consideration[(start_counter + 1):end_counter, ][withinStockton_rownum, "within_Stockton"] <- TRUE
  locations_of_consideration[(start_counter + 1):end_counter, ][is.na(locations_of_consideration[(start_counter + 1):end_counter, ]$within_Stockton), "within_Stockton"] <- FALSE
  
  locations_of_consideration[(start_counter + 1):end_counter, ][withinStocktonBuffer_rownum, "within_StocktonBuffer"] <- TRUE
  locations_of_consideration[(start_counter + 1):end_counter, ][is.na(locations_of_consideration[(start_counter + 1):end_counter, ]$within_StocktonBuffer), "within_StocktonBuffer"] <- FALSE
  
  # m_hps from "home_panel_summary" and finding the census population for each block group going to amenities in Stockton.
  m_hps <- home_panel_summary(counterMonth)
  pop_bg_stockton <- pop_blockgroup_stockton(m_hps)
  
  # Creating two vectors that allow for inputs of the proportion of origins to a destination being from Stockton.
  # The first vector is used for destinations within Stockton.
  # The second vector is used for destinations outside of Stockton.
  # A counterDest scalar was created to count the number of previous destinations that have been used from the previous month's analysis.
  
  StocktonDest_proportion_recorded <- data.frame( matrix( ncol = 2, NA ) )
  nonStocktonDest_proportion_recorded <- data.frame( matrix( ncol = 2, NA ) )
  
  colnames(StocktonDest_proportion_recorded) <- c("locations", "StocktonDest_proportion_recorded")
  colnames(nonStocktonDest_proportion_recorded) <- c("locations", "nonStocktonDest_proportion_recorded")
  
  # This for loop performs the VMT analysis for each of Stockton's block groups.
  
  print("counterVMT")
  
  for(counterVMT in 1:dest_num){
    
    # For each destination, I'm creating a list of the origins and a vector with the destination. These data structures have visit and visitor count information.
    
    origin_matrix <- m_origin_matrix_sf[m_origin_matrix_sf$full_address == dest_amenities_matrix[counterVMT, "name_address"], ]
    dest_vector <- m_dest_matrix_sf[m_dest_matrix_sf$full_address == dest_amenities_matrix[counterVMT, "name_address"], ]
    
    # These vectors take in the origin and destination distance information for the origin(s) and the destination.
    # In the case in which we use the median distance, these values will be the same.
    # In the case in which we use OSRM routing, these values will be different.
    
    distance_origin <- as.numeric(origin_matrix$distance_from_home) * factor_twoWayTrip * conv_MeterToMile * NHTS_MeanMedConv * NHTS_LinkedTripsConv * NHTS_carpoolVehicleFilter  
    distance_dest <- as.numeric(dest_vector$distance_from_home) * factor_twoWayTrip * conv_MeterToMile * NHTS_MeanMedConv * NHTS_LinkedTripsConv * NHTS_carpoolVehicleFilter
    
    # This vector computes the proportion of the origins coming from Stockton that go to the safegraph place destination being analyzed.
    
    proportion_Stockton <- sum(origin_matrix[!is.na(origin_matrix$origin_population), "visit_count"]) / sum(origin_matrix[, "visit_count"])
    
    # The following code computes the distances being driven by the origins recorded ("recorded") and those that have not been recorded "non-recorded".
    
    distance_bg_recorded <- data.frame( as.numeric(dest_vector$raw_visit_counts) / as.numeric(dest_vector$raw_visitor_counts) * origin_matrix$unique_visitor_count * distance_origin )
    distance_bg_recorded_sum <- sum( distance_bg_recorded )
    distance_bg_nonrecorded <- ( distance_dest * as.numeric(dest_vector$raw_visit_counts) - sum(distance_bg_recorded) ) * proportion_Stockton
    
    # The following code computes the "recorded" VMTs from the "distance_bg_recorded" feature previously calculated. 
    
    VMT_Origin_recorded_unique_dest <- distance_bg_recorded * as.numeric(origin_matrix$origin_population) / as.numeric(origin_matrix$number_devices_residing)
    VMT_Origin_recorded[(c(origin_matrix$origin)), (1 + counterMonth)] <- VMT_Origin_recorded[c(origin_matrix$origin), (1 + counterMonth)] + VMT_Origin_recorded_unique_dest
    VMT_Origin_recorded <- na.omit(VMT_Origin_recorded)
    
    ####################
    
    # The following code computes the "non-recorded" VMTs from the "distance_bg_nonrecorded" feature previously calculated.
    
    subset_non_recorded <- subset( VMT_Origin_nonrecorded[, c(1, (1 + counterMonth))], !(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin) )
    
    osrmTable_Stockton_VMT_unique_dest <-
      osrmTable_Stockton_VMT_v2[osrmTable_Stockton_VMT_v2$full_address == dest_vector$full_address, ][
        !(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), c("source_GEOID", "full_address", "safegraph_place_id", "time_minutes")]
    osrmTable_Stockton_VMT_unique_dest$VMT_proportion <- osrmTable_Stockton_VMT_unique_dest$time_minutes / sum(osrmTable_Stockton_VMT_unique_dest$time_minutes)
    
    VMT_Origin_nonrecorded_unique_dest <- ( matrix(nrow = nrow(subset_non_recorded), ncol = 1, distance_bg_nonrecorded * osrmTable_Stockton_VMT_unique_dest$VMT_proportion)
                                            * as.numeric( pop_bg_stockton[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), "origin_population"][[1]] )
                                            / as.numeric( pop_bg_stockton[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), "number_devices_residing"][[1]] ) )
    
    # These "non-recorded" VMTs are then allocated to the block group origins that did not have have any "recorded" VMTs associated to them.
    
    VMT_Origin_nonrecorded[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), (1 + counterMonth)] <-
      (VMT_Origin_nonrecorded[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), (1 + counterMonth)] + as.numeric(VMT_Origin_nonrecorded_unique_dest) )
    
    ####################
    
    # This code takes the proportion of those coming from Stockton and inputs that value into one of two vectors.
    # The proportion of those coming from Stockton will be inserted into the first vector if the destination is within Stockton.
    # The proportion of those coming from Stockton will be inserted into the second vector if the destination is outside of Stockton.
    
    withinStockton_conditional <- locations_of_consideration[(start_counter + 1):end_counter, ][locations_of_consideration[(start_counter + 1):end_counter, ]$full_address == dest_vector$full_address, ]$within_StocktonBuffer
    if(length(withinStockton_conditional) == 0){withinStockton_conditional = FALSE}
    
    if(withinStockton_conditional == TRUE){
      StocktonDest_proportion_recorded <- rbind(StocktonDest_proportion_recorded, c(dest_vector$full_address, proportion_Stockton))
      nonStocktonDest_proportion_recorded <- rbind(nonStocktonDest_proportion_recorded, c(NA, NA))
    } else {
      nonStocktonDest_proportion_recorded <- rbind(nonStocktonDest_proportion_recorded, c(dest_vector$full_address, proportion_Stockton) )
      StocktonDest_proportion_recorded <- rbind(StocktonDest_proportion_recorded, c(NA, NA) )
    }
    
    # The following code passes the sum of "VMT_Origin_recorded_unique_dest" and "VMT_Origin_nonrecorded_unique_dest" to the "locations_of_consideration" feature.
    # The "locations_of_consideration" feature is used to store the VMT data associated with the "recorded", "non-recorded" and "unique-dest non-recorded" data.
    
    locations_of_consideration[(start_counter + 1):end_counter, ][locations_of_consideration[(start_counter + 1):end_counter, ]$full_address == dest_vector$full_address, (9 + counterMonth)] <- sum(VMT_Origin_recorded_unique_dest, na.rm = TRUE)
    locations_of_consideration[(start_counter + 1):end_counter, ][locations_of_consideration[(start_counter + 1):end_counter, ]$full_address == dest_vector$full_address, (21 + counterMonth)] <- sum(VMT_Origin_nonrecorded_unique_dest)
    
  }
  
  # Calculating the VMTs associated with destinations not included within "dest_amenities_matrix".
  
  # Creation of the VMT proportion values for within the Stockton 1-mile buffer and outside of the Stockton 1-mile buffer.
  
  # Proportion value for within the Stockton 1-mile buffer
  
  StocktonDest_proportion_recorded_narm <- StocktonDest_proportion_recorded[!is.na(StocktonDest_proportion_recorded$locations), ]
  loc_of_cons_proportion_recorded <- locations_of_consideration[(start_counter + 1):end_counter, ][(locations_of_consideration[(start_counter + 1):end_counter,]$full_address %in% StocktonDest_proportion_recorded_narm$locations),]
  
  k_withinStocktonBuffer <- sum( as.numeric(StocktonDest_proportion_recorded_narm[,"StocktonDest_proportion_recorded"] ) * (loc_of_cons_proportion_recorded[, (9 + counterMonth)] + loc_of_cons_proportion_recorded[, (21 + counterMonth)])[, 1] / sum((loc_of_cons_proportion_recorded[, (9 + counterMonth)] + loc_of_cons_proportion_recorded[, (21 + counterMonth)])[,1]) )
  
  # Proportion value for outside of the Stockton 1-mile buffer
  
  nonStocktonDest_proportion_recorded_narm <- nonStocktonDest_proportion_recorded[!is.na(nonStocktonDest_proportion_recorded$locations), ]
  loc_of_cons_proportion_nonrecorded <- locations_of_consideration[(start_counter + 1):end_counter, ][(locations_of_consideration[(start_counter + 1):end_counter,]$full_address %in% nonStocktonDest_proportion_recorded_narm$locations),]
  
  k_outsideStocktonBuffer <- sum( as.numeric(nonStocktonDest_proportion_recorded_narm[,"nonStocktonDest_proportion_recorded"]) * (loc_of_cons_proportion_nonrecorded[, (9 + counterMonth)] + loc_of_cons_proportion_nonrecorded[, (21 + counterMonth)])[, 1] / sum((loc_of_cons_proportion_nonrecorded[, (9 + counterMonth)] + loc_of_cons_proportion_nonrecorded[, (21 + counterMonth)])[,1]) )
  
  # save.image(file = "C:/Users/Derek/Desktop/save_debug_checkpoint1.RData")
  # load("C:/Users/Derek/Desktop/save_debug_checkpoint3.RData")
  
  # Inserting average VMT values into NA slots for both the "within Stockton buffer" and "outside Stockton buffer" "locations of consideration"
  
  loc_month <- locations_of_consideration[(start_counter + 1):end_counter, ][!(locations_of_consideration[(start_counter + 1):end_counter,]$full_address %in% dest_amenities_matrix$name_address),]
  loc_true_buffer <- loc_month[loc_month$within_StocktonBuffer == TRUE, ]
  loc_false_buffer <- loc_month[loc_month$within_StocktonBuffer == FALSE, ]
  
  mean_loc_true_buffer <- mean(as.numeric(loc_true_buffer$distance_from_home), na.rm = TRUE)
  mean_loc_false_buffer <- mean(as.numeric(loc_false_buffer$distance_from_home), na.rm = TRUE)
  
  # Creating the "condition_isna" conditional
  condition_isna <- is.na(locations_of_consideration[(start_counter + 1):end_counter, ][!(locations_of_consideration[(start_counter + 1):end_counter,]$full_address %in% dest_amenities_matrix$name_address),]$distance_from_home)
  
  # Creating the "condition_trueBuffer" conditional
  condition_trueBuffer <- locations_of_consideration[(start_counter + 1):end_counter, ][!(locations_of_consideration[(start_counter + 1):end_counter,]$full_address %in% dest_amenities_matrix$name_address),]$within_StocktonBuffer == TRUE
  
  # Creating the "condition_falseBuffer" conditional
  condition_falseBuffer <- locations_of_consideration[(start_counter + 1):end_counter, ][!(locations_of_consideration[(start_counter + 1):end_counter,]$full_address %in% dest_amenities_matrix$name_address),]$within_StocktonBuffer == FALSE
  
  # Inserting the "mean_loc_true_buffer" and "mean_loc_false_buffer" values into the proper "distance_from_home" cells.
  locations_of_consideration[(start_counter + 1):end_counter, ][!(locations_of_consideration[(start_counter + 1):end_counter,]$full_address %in% dest_amenities_matrix$name_address),][condition_isna & condition_trueBuffer, "distance_from_home"] <- mean_loc_true_buffer
  locations_of_consideration[(start_counter + 1):end_counter, ][!(locations_of_consideration[(start_counter + 1):end_counter,]$full_address %in% dest_amenities_matrix$name_address),][condition_isna & condition_falseBuffer, "distance_from_home"] <- mean_loc_false_buffer
  
  ##########
  
  # Creation of the "outsideStockton_VMT_correction_factor" to correct for the over-estimate/under-estimate of the non-recorded VMTs with no block groups accociated with them.
  
  ### loc_of_cons_nonrecordVMT_all <- locations_of_consideration[(start_counter + 1):end_counter, ][!(locations_of_consideration[(start_counter + 1):end_counter, ]$full_address %in% dest_amenities_matrix$name_address),]
  ### loc_of_cons_nonrecordVMT_all_within_StocktonBuffer <- loc_of_cons_nonrecordVMT_all[loc_of_cons_nonrecordVMT_all$within_StocktonBuffer == TRUE, ]
  ### loc_of_cons_nonrecordVMT_all_not_within_StocktonBuffer <- loc_of_cons_nonrecordVMT_all[loc_of_cons_nonrecordVMT_all$within_StocktonBuffer == FALSE, ]
  
  ### VMT_within_StocktonBuffer <- sum(k_withinStocktonBuffer * as.numeric(loc_of_cons_nonrecordVMT_all_within_StocktonBuffer$distance_from_home) * as.numeric(loc_of_cons_nonrecordVMT_all_within_StocktonBuffer$raw_visit_counts) * factor_twoWayTrip *
  ###   conv_MeterToMile * NHTS_MeanMedConv * NHTS_LinkedTripsConv * NHTS_carpoolVehicleFilter)
  
  ### VMT_not_within_StocktonBuffer <- sum(k_outsideStocktonBuffer * as.numeric(loc_of_cons_nonrecordVMT_all_not_within_StocktonBuffer$distance_from_home) * as.numeric(loc_of_cons_nonrecordVMT_all_not_within_StocktonBuffer$raw_visit_counts) * factor_twoWayTrip *
  ###   conv_MeterToMile * NHTS_MeanMedConv * NHTS_LinkedTripsConv * NHTS_carpoolVehicleFilter)
  
  ### outsideStockton_VMT_correction_factor <- nudge_VMT / (VMT_not_within_StocktonBuffer/VMT_within_StocktonBuffer)
  
  ##########
  
  # Calculating the VMTs associated with each "location of consideration" and allocating those VMTs to all block groups according to their distance from the "location of consideration".
  # This for-loop does take a considerable amount of time. However, it is necessary for the allocation of VMTs with respect to the "location of consideration".
  # This analysis can be made easier if the allocation method is changed from that of the "distance away" to that of a uniform distribution.
  
  loc_of_cons_nonrecordVMT <- locations_of_consideration[(start_counter + 1):end_counter, ][!(locations_of_consideration[(start_counter + 1):end_counter, ]$full_address %in% dest_amenities_matrix$name_address) & locations_of_consideration[(start_counter + 1):end_counter, ]$within_StocktonBuffer == TRUE,]$full_address
  
  print("location_nonrecordedVMT")
  
  for (location_nonrecordedVMT in loc_of_cons_nonrecordVMT){
    
    loc_of_cons_row <- locations_of_consideration[(start_counter + 1):end_counter, ][locations_of_consideration[(start_counter + 1):end_counter, ]$full_address == location_nonrecordedVMT, ]
    
    k_row <- k_withinStocktonBuffer
    
    osrmTable_Stockton_nonrecordedVMT <- osrmTable_Stockton_VMT_v2[osrmTable_Stockton_VMT_v2$full_address == location_nonrecordedVMT, ][ , c("source_GEOID", "full_address", "safegraph_place_id", "time_minutes")]
    
    osrmTable_Stockton_nonrecordedVMT$VMT_proportion <- osrmTable_Stockton_nonrecordedVMT$time_minutes / sum(osrmTable_Stockton_nonrecordedVMT$time_minutes)
    
    bgpop_device_scaleup_factor <- sum( osrmTable_Stockton_nonrecordedVMT$VMT_proportion * as.numeric( pop_bg_stockton[, "origin_population"][[1]] ) / as.numeric( pop_bg_stockton[, "number_devices_residing"][[1]] ) )
    
    VMTs_nonrecorded <- k_row * bgpop_device_scaleup_factor * as.numeric(loc_of_cons_row$distance_from_home) * as.numeric(loc_of_cons_row$raw_visit_counts) *
      factor_twoWayTrip * conv_MeterToMile * NHTS_MeanMedConv * NHTS_LinkedTripsConv * NHTS_carpoolVehicleFilter
    
    locations_of_consideration[(start_counter + 1):end_counter, ][locations_of_consideration[(start_counter + 1):end_counter, ]$full_address == location_nonrecordedVMT, (33 + counterMonth)] <- VMTs_nonrecorded
    
    VMT_Origin_otherdest_nonrecorded[, (1 + counterMonth)] <- VMT_Origin_otherdest_nonrecorded[, (1 + counterMonth)] + VMTs_nonrecorded * osrmTable_Stockton_nonrecordedVMT$VMT_proportion * as.numeric( pop_bg_stockton[, "origin_population"][[1]] ) / as.numeric( pop_bg_stockton[, "number_devices_residing"][[1]] )
    
    # } else if(loc_of_cons_row$within_StocktonBuffer == FALSE){
    
    # k_row <- 0 # outsideStockton_VMT_correction_factor * k_outsideStocktonBuffer
    
    # locations_of_consideration[(start_counter + 1):end_counter, ][locations_of_consideration[(start_counter + 1):end_counter, ]$full_address == location_nonrecordedVMT, (33 + counterMonth)] <- k_row
    
    # print(count)
    
    # }
    
  }
  
  locations_of_consideration[(start_counter + 1):end_counter, ][!(locations_of_consideration[(start_counter + 1):end_counter, ]$full_address %in% dest_amenities_matrix$name_address) & locations_of_consideration[(start_counter + 1):end_counter, ]$within_StocktonBuffer == FALSE, (33 + counterMonth)] <- 0
  
  # At the end of each loop, I delete the m_patterns_new data. This includes "m_origin_matrix_sf", "m_dest_matrix_sf" and "m_patterns_new".
  # This is done so that the next month's data can be loaded in a clean manner.
  
  rm(m_origin_matrix_sf, m_dest_matrix_sf, m_patterns_new)
  
  # save.image(file = "C:/Users/Derek/Desktop/save_debug_checkpoint2.RData")
  # load("C:/Users/Derek/Desktop/save_debug_checkpoint2.RData")

  if(counterMonth == 1){
    save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m1.RData")
  } else if(counterMonth == 2){
      save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m2.RData")
  } else if(counterMonth == 3){
      save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m3.RData")
  } else if(counterMonth == 4){
      save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m4.RData")
  } else if(counterMonth == 5){
      save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m5.RData")
  } else if(counterMonth == 6){
      save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m6.RData")
  } else if(counterMonth == 7){
      save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m7.RData")
  } else if(counterMonth == 8){
      save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m8.RData")
  } else if(counterMonth == 9){
      save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m9.RData")
  } else if(counterMonth == 10){
      save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m10.RData")
  } else if(counterMonth == 11){
      save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m11.RData")
  } else if(counterMonth == 12){
      save.image(file = "C:/Users/Derek/Desktop/save_checkpoint_m12.RData")
  }
  
}

# Operations for VMT mapping of Stockton Block Groups

pop_bg_stockton_geospatialVMT <- pop_bg_stockton
pop_bg_stockton_geospatialVMT$number_devices_residing <- NULL
VMT_colnames <- c("VMTs_m_1", "VMTs_m_2", "VMTs_m_3", "VMTs_m_4", "VMTs_m_5", "VMTs_m_6",
                  "VMTs_m_7", "VMTs_m_8", "VMTs_m_9", "VMTs_m_10", "VMTs_m_11", "VMTs_m_12")

VMT_sum_recorded <- rowSums(VMT_Origin_recorded[, VMT_colnames])
VMT_sum_nonrecorded <- rowSums(VMT_Origin_nonrecorded[, VMT_colnames])
VMT_sum_otherdest_nonrecorded <- rowSums(VMT_Origin_otherdest_nonrecorded[, VMT_colnames])
VMT_sum_all <- VMT_sum_recorded + VMT_sum_nonrecorded + VMT_sum_otherdest_nonrecorded

VMT_all <- cbind(pop_bg_stockton_geospatialVMT, VMT_sum_recorded, VMT_sum_nonrecorded, VMT_sum_otherdest_nonrecorded, VMT_sum_all)
VMT_all$VMT_norm <- ( VMT_all$VMT_sum_all / as.numeric( as.character( VMT_all$origin_population ) ) )
save(VMT_all, file = "C:/Users/Derek/Desktop/VMT_all.RData")

VMT_all_mapview <- mapview(VMT_all, zcol = c("VMT_sum_all", "VMT_norm"), legend = TRUE)

mapshot(VMT_all_mapview, url = "C:/Users/Derek/Desktop/Stockton_Safegraph_VMT.html")
# mapshot(VMT_all_mapview, url = "S:/CCF/dashboard/Stockton_Safegraph_VMT.html")

VMT_lessthan1.25M <- nrow(VMT_all[VMT_all$VMT_sum_all < 1250000, ])
VMT_1.25Mkto1.5M <- nrow(VMT_all[VMT_all$VMT_sum_all >= 1250000 & VMT_all$VMT_sum_all < 1500000, ])
VMT_1.5Mto1.75M <- nrow(VMT_all[VMT_all$VMT_sum_all >= 1500000 & VMT_all$VMT_sum_all < 1750000, ])
VMT_1.75Mto2.0M <- nrow(VMT_all[VMT_all$VMT_sum_all >= 1750000 & VMT_all$VMT_sum_all < 2000000, ])
VMT_2.0Mplus <- nrow(VMT_all[VMT_all$VMT_sum_all >= 2000000 , ])

VMT_per_person_lessthan10k <- nrow(VMT_all[VMT_all$VMT_norm < 10000, ])
VMT_per_person_10kto15k <- nrow(VMT_all[VMT_all$VMT_norm >= 10000 & VMT_all$VMT_norm < 15000, ])
VMT_per_person_15kto20k <- nrow(VMT_all[VMT_all$VMT_norm >= 15000 & VMT_all$VMT_norm < 20000, ])
VMT_per_person_20kto30k <- nrow(VMT_all[VMT_all$VMT_norm >= 20000 & VMT_all$VMT_norm < 30000, ])
VMT_per_person_30kplus <- nrow(VMT_all[VMT_all$VMT_norm >= 30000 , ])

# Operations for VMT mapping of Business Destinations

locations_of_consideration_fix <- locations_of_consideration

count <- 0

for(full_address_iter in unique(locations_of_consideration$full_address)){
  
  # full_address_iter <- unique(locations_of_consideration$full_address)[6969]
  
  count <- count + 1
  print(count)
  
  loc_of_cons_placeholder <- locations_of_consideration_fix[locations_of_consideration_fix$full_address == full_address_iter,]
  locations_of_consideration_fix[locations_of_consideration_fix$full_address == full_address_iter, "geometry"] <- loc_of_cons_placeholder[1, "geometry"]
  
}

location_VMT <- 
  locations_of_consideration_fix %>%
  group_by(full_address) %>%
  dplyr::summarize(
    location_name = first(location_name),
    VMT_recorded = sum(VMTs_recorded_m_1,
                       VMTs_recorded_m_2,
                       VMTs_recorded_m_3,
                       VMTs_recorded_m_4,
                       VMTs_recorded_m_5,
                       VMTs_recorded_m_6,
                       VMTs_recorded_m_7,
                       VMTs_recorded_m_8,
                       VMTs_recorded_m_9,
                       VMTs_recorded_m_10,
                       VMTs_recorded_m_11,
                       VMTs_recorded_m_12,
                       na.rm = TRUE),
    VMT_nonrecorded = sum(VMTs_nonrecorded_m_1,
                          VMTs_nonrecorded_m_2,
                          VMTs_nonrecorded_m_3,
                          VMTs_nonrecorded_m_4,
                          VMTs_nonrecorded_m_5,
                          VMTs_nonrecorded_m_6,
                          VMTs_nonrecorded_m_7,
                          VMTs_nonrecorded_m_8,
                          VMTs_nonrecorded_m_9,
                          VMTs_nonrecorded_m_10,
                          VMTs_nonrecorded_m_11,
                          VMTs_nonrecorded_m_12,
                          na.rm = TRUE),
    VMT_nonrecorded_otherdest = sum(VMTs_nonrecorded_otherdest_m_1,
                                    VMTs_nonrecorded_otherdest_m_2,
                                    VMTs_nonrecorded_otherdest_m_3,
                                    VMTs_nonrecorded_otherdest_m_4,
                                    VMTs_nonrecorded_otherdest_m_5,
                                    VMTs_nonrecorded_otherdest_m_6,
                                    VMTs_nonrecorded_otherdest_m_7,
                                    VMTs_nonrecorded_otherdest_m_8,
                                    VMTs_nonrecorded_otherdest_m_9,
                                    VMTs_nonrecorded_otherdest_m_10,
                                    VMTs_nonrecorded_otherdest_m_11,
                                    VMTs_nonrecorded_otherdest_m_12,
                                    na.rm = TRUE),
    VMT_total = VMT_recorded + VMT_nonrecorded + VMT_nonrecorded_otherdest
  )

colnames(location_VMT) <- c("full_address", "location_name", "VMT_recorded", "VMT_nonrecorded", "VMT_nonrecorded_otherdest", "VMT_total", "geometry")
location_VMT <- location_VMT[location_VMT$VMT_total != 0,]

# Mapping Attempts

loc_of_cons_mapview <- mapview(stockton_boundary_influence_milebuffer, legend = TRUE) + mapview(location_VMT, zcol = "VMT_total", legend = TRUE)
loc_of_cons_mapview
mapshot(loc_of_cons_mapview, url = "C:/Users/Derek/Desktop/locations_of_consideration_VMT.html")

location_VMT_sorted <- location_VMT[with(location_VMT, order(-VMT_total)),]
save(location_VMT_sorted, file = "C:/Users/Derek/Desktop/location_VMT_sorted.RData")

# mapview(stockton_boundary_influence_milebuffer) + mapview(location_VMT, zcol = "VMT_total", at = c(seq(0, 1000000, 10000), 5000000), legend = TRUE)
# mapview(location_VMT, zcol = "VMT_total", legend = TRUE)
# mapview(VMT_all, zcol = c("VMT_sum_recorded"), legend = TRUE) + mapview(location_VMT, cex = "VMT_total", legend = TRUE)

# save.image(file = "C:/Users/Derek/Desktop/non_work_VMT_complete.RData")
# load("C:/Users/Derek/Desktop/non_work_VMT_complete.RData")