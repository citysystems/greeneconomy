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

# Using the "tigris" package for use of shape files.

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# Import files.

NHTS_df_final <- read.csv(file = "S:/CCF/CSVdata_files/NHTS_df_final.csv", header = TRUE)

source("S:/CCF/VMT_calc_functions/patterns_choice.R")
source("S:/CCF/VMT_calc_functions/safegraphplaces_cleanse.R")
source("S:/CCF/VMT_calc_functions/m_patterns_cleanse.R")
source("S:/CCF/VMT_calc_functions/m_patterns_join_cleanse.R")
source("S:/CCF/VMT_calc_functions/pop_blockgroup_stockton.R")
source("S:/CCF/VMT_calc_functions/month_patterns_new.R")
source("S:/CCF/VMT_calc_functions/home_panel_summary.R")
source("S:/CCF/VMT_calc_functions/origin_trips.R")
source("S:/CCF/VMT_calc_functions/dest_trips.R")
source("S:/CCF/VMT_calc_functions/origin_locations.R")
source("S:/CCF/VMT_calc_functions/dest_locations.R")
source("S:/CCF/VMT_calc_functions/geocodeSL.R")
source("S:/CCF/VMT_calc_functions/nudge_MeanMedConv.R")
source("S:/CCF/VMT_calc_functions/nudge_LinkedTrips.R")
source("S:/CCF/VMT_calc_functions/nudge_CarpoolVehicleFilter.R")

# Using the census API to create block groups and use census information in Stockton.
census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab", overwrite = TRUE)
acs5_17 <- load_variables(2017, "acs5")
ca_bgs <- block_groups("CA", cb = TRUE)

# Uploading safegraph Places .csv file & data cleansing.
safegraphplaces <- read.csv("S:/Restricted Data Library/Safegraph/poi/safegraphplaces.csv", header=TRUE, stringsAsFactors = FALSE)
safegraphplaces <- safegraphplaces_cleanse(safegraphplaces)

### Calculation of NHTS nudge values for the VMT calculations.

##########

### Nudge: Performing the linked trips conversion factor.

NHTS_LinkedTripsConv <- nudge_LinkedTrips(NHTS_df_final)

### Nudge: Peforming the median to mean converison.

NHTS_MeanMedConv <- nudge_MeanMedConv(NHTS_df_final)

### Nudge: Peforming the carpooling and vehicle vs. other transit mode nudge.

NHTS_carpoolVehicleFilter <- NHTS_carpoolVehicleFilter(NHTS_df_final)

# Downloading Safegraph patterns .csv files.

for(num in 1:12){

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
  dest_unique <- data.frame(unique(m_origin_matrix_sf$full_address))
  m_dest_matrix_sf <- month_dest_matrix(m_patterns_join, dest_unique, safegraphplaces)
 
  filename <- paste("S:/CCF/m_patterns_new/", substr(patterns_text, 45, 57), "_new.RData", sep = "")
  
  save(m_origin_matrix_sf, m_dest_matrix_sf, m_patterns_new, file = filename)
   
  rm(m_origin_matrix_sf, m_dest_matrix_sf, m_patterns_new, file = filename)
  
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

# Calculation of decimal degree to mile conversion.

Stockton_longitude <- -121.2908
Stockton_latitude <- 37.9577

latitude_45 <- 45
latitude_23 <- 23

deci_km_45 <- 78.71
deci_km_23 <- 102.47

km_to_mile_conv <- 0.621371
deci_degree_mile_conversion <- ( (deci_km_45 - deci_km_23)/(latitude_45 - latitude_23) *
                               (Stockton_latitude - latitude_23) + deci_km_23 ) * km_to_mile_conv

### Wiki Link: https://en.wikipedia.org/wiki/Decimal_degrees

# Conversion factors

conv_MeterToMile <- 0.000621371
factor_twoWayTrip <- 2

# Creation of the origin matrices for all of Stockton's 205 origins.

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

# Constants to evaluate number of categories necessary for the VMT calculations.

months_in_year <- 12
VMT_considerations <- 3
months_considerations <- months_in_year * VMT_considerations

# Creating shapefiles that include (1) the Stockton boundary and (2) the Stockton boundary with a 1-mile buffer.

stockton_boundary_influence <- (st_read("S:/CCF/SpheresOfInfluence/SpheresOfInfluence.shp") %>% filter(SPHERE == "STOCKTON") %>% st_transform(st_crs(4326)))[1,]
stockton_boundary_influence_milebuffer <- st_buffer(stockton_boundary_influence, 1/deci_degree_mile_conversion)

# Creating a blank shapefile dataframe for thr destinations of consideration.

locations_of_consideration <- st_sf(data.frame(matrix(data = NA, nrow = 0, ncol = 47), geom = st_sfc()), crs = 4326)

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

# Creating two vectors that allow for inputs of the proportion of origins to a destination being from Stockton.
# The first vector is used for destinations within Stockton.
# The second vector is used for destinations outside of Stockton.
# A counterDest scalar was created to count the number of previous destinations that have been used from the previous month's analysis.

StocktonDest_proportion_recorded <- data.frame( matrix( ncol = 2, NA ) )
nonStocktonDest_proportion_recorded <- data.frame( matrix( ncol = 2, NA ) )

colnames(StocktonDest_proportion_recorded) <- c("locations", "StocktonDest_proportion_recorded")
colnames(nonStocktonDest_proportion_recorded) <- c("locations", "nonStocktonDest_proportion_recorded")

##########

# This for loop analyses the VMT by origin and destination, per month.

for(counterMonth in 1:12){

  # Loading the m_patterns_new data. This includes "m_origin_matrix_sf", "m_dest_matrix_sf" and "m_patterns_new".
  # This is done for each of the 12 months of the year.
  
  patterns_text <- patterns_choice(counterMonth)
  filename <- paste("S:/CCF/m_patterns_new/", substr(patterns_text, 45, 57), "_new.RData", sep = "")
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
  
  ##########
  # count_dist_start <- 1
  # count_dist_end <- 0
  # distance <- OD_time_dist[, "distance"]
  ##########
  
  # Adding these locations of interest to a holding variable. Moreover, to make sure the analysis
  # is done on a monthly basis, I calculate which rows are those are the start and end of each month
  # as done with the "start_counter" and "end_counter" variables.
  
  start_counter <- nrow(locations_of_consideration)
  locations_of_consideration <- rbind(locations_of_consideration, st_sf(st_as_sf(m_dest_matrix_sf[!is.na(m_dest_matrix_sf$longitude), ],
                                coords = c("longitude", "latitude"), crs = 4326) ) )
  end_counter <- nrow( locations_of_consideration )

  # Creation of the column values that disclose whether a destination is within Stockton and/or its 1-mile buffer.
    
  withinStockton_rownum <- st_intersects(stockton_boundary_influence, locations_of_consideration[(start_counter + 1):end_counter, "geometry"], sparce = TRUE)[[1]] 
  withinStocktonBuffer_rownum <- st_intersects(stockton_boundary_influence_milebuffer, locations_of_consideration[(start_counter + 1):end_counter, "geometry"], sparce = TRUE)[[1]]
  
  locations_of_consideration[(start_counter + 1):end_counter, ][withinStockton_rownum, "within_Stockton"] <- TRUE
  locations_of_consideration[(start_counter + 1):end_counter, ][is.na(locations_of_consideration[(start_counter + 1):end_counter, ]$within_Stockton), "within_Stockton"] <- FALSE
  
  locations_of_consideration[(start_counter + 1):end_counter, ][withinStocktonBuffer_rownum, "within_StocktonBuffer"] <- TRUE
  locations_of_consideration[(start_counter + 1):end_counter, ][is.na(locations_of_consideration[(start_counter + 1):end_counter, ]$within_StocktonBuffer), "within_StocktonBuffer"] <- FALSE
  
  # m_hps from "home_panel_summary" and finding the census population for each block group goint to amenities in Stockton.
  m_hps <- home_panel_summary(counterMonth)
  pop_bg_stockton <- pop_blockgroup_stockton(m_hps)
  
  # This for loop performs the VMT analysis for each of Stockton's block groups.

  for(counterVMT in 1:dest_num){
  
    # For each destination, I'm creating a list of the origins and a vector with the destination. These data structures have visit and vistor count information.
    
    origin_matrix <- m_origin_matrix_sf[m_origin_matrix_sf$full_address == dest_amenities_matrix[counterVMT, "name_address"], ]
    dest_vector <- m_dest_matrix_sf[m_dest_matrix_sf$full_address == dest_amenities_matrix[counterVMT, "name_address"], ]
  
    ##########
    # ACF_safegraph <- as.numeric(dest_vector$distance_from_home)
    # ACF_OD <- as.numeric(median(st_distance(origin_matrix, dest_vector)))
    # count_dist_end <- count_dist_end + nrow(origin_matrix)
    # distance_new <- distance[count_dist_start:count_dist_end] * ACF_safegraph / ACF_OD * NHTS_MeanMedConv * NHTS_LinkedTripsConv
    ##########
  
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
    
    # The following code computes the "non-recorded" VMTs from the "distance_bg_nonrecorded" feature previously calculated.
    
    subset_non_recorded <- subset( VMT_Origin_nonrecorded[, c(1, (1 + counterMonth))], !(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin) )
    VMT_Origin_nonrecorded_unique_dest <- ( matrix(nrow = nrow(subset_non_recorded), ncol = 1, distance_bg_nonrecorded/nrow(subset_non_recorded))
                                            * as.numeric( pop_bg_stockton[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), "origin_population"][[1]] )
                                            / as.numeric( pop_bg_stockton[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), "number_devices_residing"][[1]] ) )
    VMT_Origin_nonrecorded[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), (1 + counterMonth)] <-
    (VMT_Origin_nonrecorded[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), (1 + counterMonth)] + VMT_Origin_nonrecorded_unique_dest)

    # This code takes the proportion of those coming from Stockton and inputs that value into one of two vectors.
    # The proportion of those coming from Stockton will be inserted into the first vector if the destination is within Stockton.
    # The proportion of those coming from Stockton will be inserted into the second vector if the destination is outside of Stockton.
    
    withinStockton_conditional <- locations_of_consideration[(start_counter + 1):end_counter, ][locations_of_consideration[(start_counter + 1):end_counter, ]$full_address == dest_vector$full_address, ]$within_Stockton
    if(length(withinStockton_conditional) == 0){withinStockton_conditional = FALSE}
    
    if(withinStockton_conditional == TRUE){
      StocktonDest_proportion_recorded <- rbind(StocktonDest_proportion_recorded, c(dest_vector$full_address, proportion_Stockton))
      nonStocktonDest_proportion_recorded <- rbind(nonStocktonDest_proportion_recorded, c(NA, NA))
      }
    else{
      nonStocktonDest_proportion_recorded <- rbind(nonStocktonDest_proportion_recorded, c(dest_vector$full_address, proportion_Stockton) )
      StocktonDest_proportion_recorded <- rbind(StocktonDest_proportion_recorded, c(NA, NA))
      }

    ##########
    # count_dist_start <- count_dist_start + nrow(origin_matrix)
    ##########
    
    # The following code passes the sum of "VMT_Origin_recorded_unique_dest" and "VMT_Origin_nonrecorded_unique_dest" to the "locations_of_consideration" feature.
    # The "locations_of_consideration" feature is used to store the VMT data associated with the "recorded", "non-recorded" and "unique-dest non-recorded" data.
    
    locations_of_consideration[(start_counter + 1):end_counter, ][locations_of_consideration[(start_counter + 1):end_counter, ]$full_address == dest_vector$full_address, (9 + counterMonth)] <- sum(VMT_Origin_recorded_unique_dest)
    locations_of_consideration[(start_counter + 1):end_counter, ][locations_of_consideration[(start_counter + 1):end_counter, ]$full_address == dest_vector$full_address, (21 + counterMonth)] <- sum(VMT_Origin_nonrecorded_unique_dest)
    
    # I just printed out '---' so that I knew this code was working at every loop. This can be deleted in the future.
    print('---')
    
  }
  
  # Calculating the VMTs associated with destinations not included within the ("dest_amenities_matrix")
  
  ##########
  # distance_cutoff <- 1 # miles
  # locations_of_consideration[(start_counter + 1):end_counter, "distance_to_Stockton"] <- latlong_mile_conversion * sqrt( (as.numeric(locations_of_consideration[(start_counter + 1):end_counter, "longitude"]) - Stockton_longitude)^2
  #                                                                                                                      + (as.numeric(locations_of_consideration[(start_counter + 1):end_counter, "latitude"]) - Stockton_latitude)^2 )
  ##########

  ##########
  ### VMT_unique_dest_avg <- mean(VMT_Origin_nonrecorded[, (1 + counterMonth)]) # Comment #1: I need to change this soon.
  ##########
  
  # The following code first computes two conditionals to understand if there is a destination that has not been allocated VMTs to. This analysis is done on a monthly basis.
  # The first conditional checks to see whether any of the destinations have had VMTs allocated to them. If not, TRUE. Otherwise, FALSE. 
  # The second conditional checks to see whether the destinations are within 1 mile of the Stockton buffer zone. If so, TRUE. Otherwise, FALSE.
  
  VMT_unique_dest_conditional_1 <- is.na(locations_of_consideration[(start_counter + 1):end_counter, ][locations_of_consideration[(start_counter + 1):end_counter,(21 + counterMonth)], (21 + counterMonth)])[, 1]
  VMT_unique_dest_conditional_2 <- (locations_of_consideration[(start_counter + 1):end_counter, ]$within_StocktonBuffer == TRUE)

  # The following code computes the number of rows in which the following two conditionals hold true (TRUE).
    
  otherdest_rows <- nrow(locations_of_consideration[(start_counter + 1):end_counter, ][VMT_unique_dest_conditional_1 & VMT_unique_dest_conditional_2, (21 + counterMonth)])

  # The following code 
    
  locations_of_consideration[(start_counter + 1):end_counter, ][VMT_unique_dest_conditional_1 & VMT_unique_dest_conditional_2, (33 + counterMonth)] <- 
    as.numeric(locations_of_consideration[(start_counter + 1):end_counter, ][VMT_unique_dest_conditional_1 & VMT_unique_dest_conditional_2, ]$distance_from_home) * 
    as.numeric(locations_of_consideration[(start_counter + 1):end_counter, ][VMT_unique_dest_conditional_1 & VMT_unique_dest_conditional_2, ]$raw_visit_counts) *
    factor_twoWayTrip * conv_MeterToMile * NHTS_MeanMedConv * NHTS_LinkedTripsConv * NHTS_carpoolVehicleFilter
  
  ##########
  # I would not call the above complete. Based on this analysis, I am assuming that the ratio of the origin population to the number of devices residing is 1:1.
  # This is however not the case. I would recommend implementing code such as the following:
  # 
  # * as.numeric( pop_bg_stockton[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), "origin_population"][[1]] )
  # / as.numeric( pop_bg_stockton[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), "number_devices_residing"][[1]] )
  # 
  # This would make it so that the ratio mentioned previously woud not be 1:1.
  ##########
  
  # I then take these same values that were stored in the previous column, I find the mean and I evenly distribute these values across all of Stockton's origins.
  
  VMT_unique_dest_avg <- (locations_of_consideration[(start_counter + 1):end_counter, ][VMT_unique_dest_conditional_1 & VMT_unique_dest_conditional_2, (33 + counterMonth)])
  VMT_unique_dest_avg <- mean(VMT_unique_dest_avg[[1]], na.rm = TRUE)
  VMT_Origin_otherdest_nonrecorded[, (1 + counterMonth)] <- VMT_unique_dest_avg * otherdest_rows / nrow(pop_bg_stockton)
  
  # At the end of each loop, I delete the m_patterns_new data. This includes "m_origin_matrix_sf", "m_dest_matrix_sf" and "m_patterns_new".
  # This is done so that the next month's data can be loaded in a clean manner.
  
  rm(m_origin_matrix_sf, m_dest_matrix_sf, m_patterns_new)
  
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

mapshot(VMT_all_mapview, url = "S:/CCF/dashboard/Stockton_Safegraph_VMT.html")
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

location_VMT <- 
  locations_of_consideration %>%
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
                                     na.rm = TRUE)
     )
