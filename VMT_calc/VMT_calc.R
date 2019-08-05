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

NHTS_df_final <- read.csv(file = "C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/NHTS_df_final.csv", header = TRUE)

source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/patterns_choice.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/safegraphplaces_cleanse.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/m_patterns_cleanse.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/m_patterns_join_cleanse.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/pop_blockgroup_stockton.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/month_patterns_new.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/home_panel_summary.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/origin_trips.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/dest_trips.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/origin_locations.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/dest_locations.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/geocodeSL.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/nudge_MeanMedConv.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/nudge_LinkedTrips.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/nudge_CarpoolVehicleFilter.R")

# Using the census API to create block groups and use census information in Stockton.
census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab", overwrite = TRUE)
acs5_17 <- load_variables(2017, "acs5")
ca_bgs <- block_groups("CA", cb = TRUE)

# Uploading safegraph Places .csv file & data cleansing.
safegraphplaces <- read.csv("C:/Users/Derek/Desktop/safegraph/poi/safegraphplaces.csv", header=TRUE, stringsAsFactors = FALSE)
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

for(num in 7:8){

  patterns_text <- patterns_choice(num)
  m_patterns <- m_patterns_cleanse(patterns_text)
  m_patterns_join <- m_patterns_join_cleanse(m_patterns, safegraphplaces)

  # m_hps from "home_panel_summary"
  m_hps <- home_panel_summary(num)

  # Looping through the data to disaggregate the census blocks from the monthly patterns .csv files.
  m_patterns_new <- month_patterns_new(m_patterns_join, pop_bg_stockton)
  
  # Finding the census population for each block group goint to amenities in Stockton.
  pop_bg_stockton <- pop_blockgroup_stockton(m_hps)
  
  # Origin Points for Locations
  # This includes finding the census population for each block group going to amenities in Stockton.
  m_origin_matrix_sf <- month_origin_matrix(m_patterns_new, m_hps, pop_bg_stockton)
  
  # Destination Points for Location
  dest_unique <- data.frame(unique(m_origin_matrix_sf$full_address))
  m_dest_matrix_sf <- month_dest_matrix(m_patterns_join, dest_unique, safegraphplaces)
 
  filename <- paste(substr(patterns_text, 0, 23), "Stockton_R_code/VMT_Calculation_new/", substr(patterns_text, 34, 45), "_new.RData", sep = "")
  
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

# Constant variables

conv_MeterToMile <- 0.000621371
factor_twoWayTrip <- 2

Stockton_longitude <- -121.2908
Stockton_latitude <- 37.9577
latlong_mile_conversion <- 69.132

VMT_Origin_recorded <- data.frame(matrix(0, nrow = nrow(pop_bg_stockton), ncol = 13))
VMT_Origin_recorded[, 1] <- pop_bg_stockton$origin
colnames(VMT_Origin_recorded) <- c("origin", "VMTs_m_1", "VMTs_m_2", "VMTs_m_3", "VMTs_m_4", "VMTs_m_5", "VMTs_m_6", "VMTs_m_7", "VMTs_m_8", "VMTs_m_9", "VMTs_m_10", "VMTs_m_11", "VMTs_m_12")
rownames(VMT_Origin_recorded) <- pop_bg_stockton$origin

VMT_Origin_nonrecorded <- data.frame(matrix(0, nrow = nrow(pop_bg_stockton), ncol = 13))
VMT_Origin_nonrecorded[, 1] <- pop_bg_stockton$origin
colnames(VMT_Origin_nonrecorded) <- c("origin", "VMTs_m_1", "VMTs_m_2", "VMTs_m_3", "VMTs_m_4", "VMTs_m_5", "VMTs_m_6", "VMTs_m_7", "VMTs_m_8", "VMTs_m_9", "VMTs_m_10", "VMTs_m_11", "VMTs_m_12")
rownames(VMT_Origin_nonrecorded) <- pop_bg_stockton$origin

VMT_Origin_otherdest_nonrecorded <- data.frame(matrix(0, nrow = nrow(pop_bg_stockton), ncol = 13))
VMT_Origin_otherdest_nonrecorded[, 1] <- pop_bg_stockton$origin
colnames(VMT_Origin_otherdest_nonrecorded) <- c("origin", "VMTs_m_1", "VMTs_m_2", "VMTs_m_3", "VMTs_m_4", "VMTs_m_5", "VMTs_m_6", "VMTs_m_7", "VMTs_m_8", "VMTs_m_9", "VMTs_m_10", "VMTs_m_11", "VMTs_m_12")
rownames(VMT_Origin_otherdest_nonrecorded) <- pop_bg_stockton$origin

months_in_year <- 12
VMT_considerations <- 3
months_considerations <- months_in_year * VMT_considerations

##########

for(counterMonth in 1:12){
  
  print(counterMonth)

  patterns_text <- patterns_choice(counterMonth)
  filename <- paste(substr(patterns_text, 0, 23), "Stockton_R_code/VMT_Calculation_new/", substr(patterns_text, 34, 45), "_new.RData", sep = "")
  load(filename)

  dest_amenities_matrix <- data.frame(unique(m_origin_matrix_sf$full_address))
  colnames(dest_amenities_matrix) <- "name_address"
  dest_num <- nrow(dest_amenities_matrix)
  
  m_dest_matrix_VMT <- cbind(m_dest_matrix_sf, data.frame(matrix( NA, nrow = nrow(m_dest_matrix_sf), ncol = months_considerations )) )
  colnames(m_dest_matrix_VMT)[12:47] <- c("VMTs_recorded_m_1", "VMTs_recorded_m_2", "VMTs_recorded_m_3", "VMTs_recorded_m_4", "VMTs_recorded_m_5", "VMTs_recorded_m_6",
                                          "VMTs_recorded_m_7", "VMTs_recorded_m_8", "VMTs_recorded_m_9", "VMTs_recorded_m_10", "VMTs_recorded_m_11", "VMTs_recorded_m_12",
                                          "VMTs_nonrecorded_m_1", "VMTs_nonrecorded_m_2", "VMTs_nonrecorded_m_3", "VMTs_nonrecorded_m_4", "VMTs_nonrecorded_m_5", "VMTs_nonrecorded_m_6",
                                          "VMTs_nonrecorded_m_7", "VMTs_nonrecorded_m_8", "VMTs_nonrecorded_m_9", "VMTs_nonrecorded_m_10", "VMTs_nonrecorded_m_11", "VMTs_nonrecorded_m_12",
                                          "VMTs_nonrecorded_otherdest_m_1", "VMTs_nonrecorded_otherdest_m_2", "VMTs_nonrecorded_otherdest_m_3", "VMTs_nonrecorded_otherdest_m_4",
                                          "VMTs_nonrecorded_otherdest_m_5", "VMTs_nonrecorded_otherdest_m_6", "VMTs_nonrecorded_otherdest_m_7", "VMTs_nonrecorded_otherdest_m_8",
                                          "VMTs_nonrecorded_otherdest_m_9", "VMTs_nonrecorded_otherdest_m_10", "VMTs_nonrecorded_otherdest_m_11","VMTs_nonrecorded_otherdest_m_12")
  
  locations_of_consideration <- m_dest_matrix_VMT[!is.na(m_dest_matrix_VMT$longitude), ]
  locations_of_consideration$distance_to_Stockton <- latlong_mile_conversion * sqrt( (as.numeric(locations_of_consideration$longitude) - Stockton_longitude)^2
                                                                                     + (as.numeric(locations_of_consideration$latitude) - Stockton_latitude)^2  )

  # count_dist_start <- 1
  # count_dist_end <- 0
  # distance <- OD_time_dist[, "distance"]

  distance_recorded <- data.frame( matrix(nrow = dest_num, ncol = 1, NA) )
  distance_nonrecorded <- data.frame( matrix(nrow = dest_num, ncol = 1, NA) )

  for(counterVMT in 1:dest_num){
  
    origin_matrix <- m_origin_matrix_sf[m_origin_matrix_sf$full_address == dest_amenities_matrix[counterVMT, "name_address"], ]
    dest_vector <- m_dest_matrix_VMT[m_dest_matrix_VMT$full_address == dest_amenities_matrix[counterVMT, "name_address"], ]
  
    ##########
    # ACF_safegraph <- as.numeric(dest_vector$distance_from_home)
    # ACF_OD <- as.numeric(median(st_distance(origin_matrix, dest_vector)))
    # count_dist_end <- count_dist_end + nrow(origin_matrix)
    # distance_new <- distance[count_dist_start:count_dist_end] * ACF_safegraph / ACF_OD * NHTS_MeanMedConv * NHTS_LinkedTripsConv
    ##########
  
    distance_origin <- as.numeric(origin_matrix$distance_from_home) * factor_twoWayTrip * conv_MeterToMile * NHTS_MeanMedConv * NHTS_LinkedTripsConv * NHTS_carpoolVehicleFilter  
    distance_dest <- as.numeric(dest_vector$distance_from_home) * factor_twoWayTrip * conv_MeterToMile * NHTS_MeanMedConv * NHTS_LinkedTripsConv * NHTS_carpoolVehicleFilter

    proportion_Stockton <- sum(origin_matrix[!is.na(origin_matrix$origin_population), "visit_count"]) / sum(origin_matrix[, "visit_count"])
  
    distance_bg_recorded <- data.frame( as.numeric(dest_vector$raw_visit_counts) / as.numeric(dest_vector$raw_visitor_counts) * origin_matrix$unique_visitor_count * distance_origin)
    distance_bg_recorded_sum <- sum(distance_bg_recorded) 
    distance_bg_nonrecorded <- ( distance_dest * as.numeric(dest_vector$raw_visit_counts) - sum(distance_bg_recorded) ) * proportion_Stockton
  
    # if(nrow(VMT_Origin_recorded) > 205){print(counterVMT)}
    # print(counterVMT)
    # print(proportion_Stockton)

    VMT_Origin_recorded_unique_dest <- distance_bg_recorded * as.numeric(origin_matrix$origin_population) / as.numeric(origin_matrix$number_devices_residing)
    VMT_Origin_recorded[(c(origin_matrix$origin)), (1 + counterMonth)] <- VMT_Origin_recorded[c(origin_matrix$origin), (1 + counterMonth)] + VMT_Origin_recorded_unique_dest
    VMT_Origin_recorded <- na.omit(VMT_Origin_recorded)
    
    subset_non_recorded <- subset(VMT_Origin_nonrecorded[, c(1, (1 + counterMonth))], !(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin))
    VMT_Origin_nonrecorded_unique_dest <- ( matrix(nrow = nrow(subset_non_recorded), ncol = 1, distance_bg_nonrecorded/nrow(subset_non_recorded))
                                            * as.numeric( pop_bg_stockton[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), "origin_population"][[1]] )
                                            / as.numeric( pop_bg_stockton[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), "number_devices_residing"][[1]] ) )
    VMT_Origin_nonrecorded[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), (1 + counterMonth)] <- (VMT_Origin_nonrecorded[!(VMT_Origin_nonrecorded$origin %in% origin_matrix$origin), (1 + counterMonth)]
                                                                                                       + VMT_Origin_nonrecorded_unique_dest)

    distance_recorded[counterVMT, 1] <- distance_bg_recorded_sum
    distance_nonrecorded[counterVMT, 1] <- distance_bg_nonrecorded/nrow(subset_non_recorded)
    
    m_dest_matrix_VMT[m_dest_matrix_VMT$full_address == dest_vector$full_address, (11 + counterMonth)] <- sum(VMT_Origin_recorded_unique_dest)
    locations_of_consideration[locations_of_consideration$full_address == dest_vector$full_address, (11 + counterMonth)] <- sum(VMT_Origin_recorded_unique_dest)

    m_dest_matrix_VMT[m_dest_matrix_VMT$full_address == dest_vector$full_address, (23 + counterMonth)] <- sum(VMT_Origin_nonrecorded_unique_dest)
    locations_of_consideration[locations_of_consideration$full_address == dest_vector$full_address, (23 + counterMonth)] <- sum(VMT_Origin_nonrecorded_unique_dest)
  
    ##########
    # count_dist_start <- count_dist_start + nrow(origin_matrix)
    ##########
  
  }

  # Calculating the VMTs associated with destinations not included within the ("dest_amenities_matrix")

  VMT_unique_dest_avg <- mean(VMT_Origin_nonrecorded[, (1 + counterMonth)])
  distance_cutoff <- 1 # miles

  otherdest_rows <- length(locations_of_consideration[is.na(locations_of_consideration[, (23 + counterMonth)]) & locations_of_consideration[, "distance_to_Stockton"] <= distance_cutoff, (23 + counterMonth)])
  locations_of_consideration[is.na(locations_of_consideration[, (23 + counterMonth)]) & locations_of_consideration[, "distance_to_Stockton"] <= distance_cutoff, (23 + counterMonth)] <- VMT_unique_dest_avg
  VMT_Origin_otherdest_nonrecorded[, (1 + counterMonth)] <- VMT_unique_dest_avg * otherdest_rows / nrow(pop_bg_stockton)

  rm(m_origin_matrix_sf, m_dest_matrix_sf, m_patterns_new)
  
}

# Operations for VMT mapping


pop_bg_stockton$number_devices_residing <- NULL
VMT_colnames <- c("VMTs_m_1", "VMTs_m_2", "VMTs_m_3", "VMTs_m_4", "VMTs_m_5", "VMTs_m_6",
                  "VMTs_m_7", "VMTs_m_8", "VMTs_m_9", "VMTs_m_10", "VMTs_m_11", "VMTs_m_12")

VMT_sum_recorded <- rowSums(VMT_Origin_recorded[, VMT_colnames])
VMT_sum_nonrecorded <- rowSums(VMT_Origin_nonrecorded[, VMT_colnames])
VMT_sum_otherdest_nonrecorded <- rowSums(VMT_Origin_otherdest_nonrecorded[, VMT_colnames])
VMT_sum_all <- VMT_sum_recorded + VMT_sum_nonrecorded + VMT_sum_otherdest_nonrecorded

VMT_all <- cbind(pop_bg_stockton, VMT_sum_recorded, VMT_sum_nonrecorded, VMT_sum_otherdest_nonrecorded, VMT_sum_all)
VMT_all$VMT_norm <- ( VMT_all$VMT_sum_all / as.numeric( as.character( VMT_all$origin_population ) ) )

mapview(VMT_all, zcol = "VMT_norm", legend = TRUE)

# colnames(VMT_perDest) <- "VMT_perDest"

# VMT_Avg <- mean(VMT_perDest$VMT_perDest)
# m_dest_matrix_sf[is.na(m_1_dest_matrix_sf$VMT), "VMT"] <- VMT_Avg
# VMT_tot <- sum(m_dest_matrix_sf$VMT)

# ratio_test <- sum(m_1_origin_matrix_sf[!is.na(m_1_origin_matrix_sf$origin_population),]$visit_count) / sum(m_1_origin_matrix_sf$visit_count)