### Use the libraries of the installed packages.

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

##########

# Local server to run the st_intersects package at a faster rate.
options(osrm.server = "http://127.0.0.1:5000/")

# Using the "tigris" package for use of shape files.
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

##########

# The following code uploads the data from the CCF package and uses this data to answer the questions below.
load("G:/My Drive/Stanford City Systems - Greg's Summer Work/00 Complete Communities Framework (CCF)/03 CCF Survey/month_patterns_join_all.RData")
load("G:/My Drive/Stanford City Systems - Greg's Summer Work/00 Complete Communities Framework (CCF)/03 CCF Survey/pre_survey.RData")
load("G:/My Drive/Stanford City Systems - Greg's Summer Work/00 Complete Communities Framework (CCF)/03 CCF Survey/amenities_persubcat_perblockgroup.RData")
load("G:/My Drive/Stanford City Systems - Greg's Summer Work/00 Complete Communities Framework (CCF)/03 CCF Survey/ca_blockgroups_pop.RData")

# The following code uploads scripts from the CCF survey project.
source("C:/Users/Derek/Documents/GitHub/Stockton/CCF_survey/month_patterns_new_CCF_survey.R")
source("C:/Users/Derek/Documents/GitHub/Stockton/CCF_survey/NAICS_NHTS_Lookup.R")

# The following code uploads scripts from the Stockton Green Economt Project - VMT Analysis.
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/home_panel_summary.R")
source("C:/Users/Derek/Documents/GitHub/greeneconomy/VMT_calc/VMT_calc_functions/pop_blockgroup_stockton.R")

##########

# Testing of the two possible OSRM approaches: (1) OSRM Isochrones and (2) OSRM Routing

# Uploading the South Stockton Promise Zone (SSPZ) and the Stockton Spheres of Influence. 
sspzboundary <- st_read("S:/CCF/sspzboundary/sspzboundary.shp") %>% st_transform(st_crs(4326))
stockton_boundary_influence <- (st_read("S:/CCF/SpheresOfInfluence/SpheresOfInfluence.shp") %>% filter(SPHERE == "STOCKTON") %>% st_transform(st_crs(4326)))[1,]

# Taking the block groups within Stockton and filtering them by those within SSPZ.
sjc_bgs <- block_groups("California", "San Joaquin County", cb = TRUE) %>% st_transform(st_crs(4326))
sspz_bgs <- sjc_bgs[which(sjc_bgs$GEOID %in% st_centroid(sjc_bgs)[sspzboundary,]$GEOID),]
sspz_centroid <- st_centroid(sspz_bgs)
# 13284 - Row number with block group in the center.

# Uploading the San Joaquin safegraphplaces dataset.
safegraphplaces <- read.csv("S:/Restricted Data Library/Safegraph/poi/safegraphplaces.csv", header=TRUE, stringsAsFactors = FALSE)
safegraphplaces <- safegraphplaces[!is.na(safegraphplaces$sub_category), ]

##########

# OSRM Approach #1: OSRM Isochrones

function_OSRMisochrone <- function(blocks_amount_for_testing){

  # These variables (1) intake the iterations factor and (2) start the counting of each iteration. 
  n <- blocks_amount_for_testing
  count <- 0
  
  # Creation of the "break" times used for the iscochrone creation.
  # break_times <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 30, 40, 50, 60)
  break_times <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 
                   11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 30, 40, 50, 60)
  # break_times <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 
  #                  10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15, 16, 17, 18, 19, 20, 25, 30, 40, 50, 60)
  
  # Creation of a data frame that holds in all block group isochrone - safegraphplaces points intersections.
  isochrone_block_Stockton <- data.frame(stringsAsFactors = FALSE)

  # This first for loop chooses each block group for analysis.
  for(counter_block in sspz_centroid$GEOID){

    # The following code creates an isochrone at the centroid of the block group being analyzed.
    # isochrone_block <- osrmIsochrone(sspz_centroid[(sspz_centroid$GEOID == counter_block),], breaks = break_times, returnclass = "sf")
    isochrone_block <- osrmIsochrone(sspz_centroid[(sspz_centroid$GEOID == counter_block),], breaks = break_times, res = 175, returnclass = "sf")
    isochrone_block <- st_set_crs(isochrone_block, '+proj=longlat +datum=WGS84')
    mapview(isochrone_block)
    
    # The following code chooses the safegraphplaces points associated with the sub-category being analyzed.
    isochrone_block_points <- st_as_sf(safegraphplaces, coords = c("longitude", "latitude"), crs = 4326)
    
    # The following code performs the intersection between the safegraph points and the generated isochrone block with time bands.
    isochrone_block_intersection <- data.frame(st_intersection(isochrone_block_points, isochrone_block), stringsAsFactors = FALSE)
    
    ##########
    # The following code takes the safegraphplaces that are not within the isochrones being analyzed and adds them to the analysis as placeholders.
    
    # Creation of the data frame called "missing_safegraphplaces" which includes safegraphplaces not included in the "isochrone_block_intersection".
    missing_safegraphplaces <- safegraphplaces[!(safegraphplaces$safegraph_place_id %in% isochrone_block_intersection$safegraph_place_id), ]
    
    # These columns are created so that they can include the "id", "min", "max", and "center" features involved within the "st_intersection" command.
    new_col <- data.frame( matrix(data = NA, nrow = nrow(missing_safegraphplaces), ncol = 4) )
    colnames(new_col) <- c("id", "min", "max", "center")
    new_col[, "id"] <- max(isochrone_block_intersection[, "id"]) + 1
    new_col[, "min"] <- max(isochrone_block_intersection[, "max"])
    new_col[, "max"] <- max(isochrone_block_intersection[, "max"]) + 30 # NA
    new_col[, "center"] <- max(isochrone_block_intersection[, "max"]) + 15 # NA
    
    # These commands append "new_col" to "missing_safegraphplaces".
    missing_safegraphplaces <- cbind(missing_safegraphplaces, new_col)
    missing_safegraphplaces <- data.frame( st_as_sf(missing_safegraphplaces, coords = c("longitude", "latitude"), crs = 4326), stringsAsFactors = FALSE)
    
    # This is the final "rbind" that binds "isochrone_block_intersection" and "missing_safegraphplaces" together.
    isochrone_block_intersection <- data.frame( rbind(isochrone_block_intersection, missing_safegraphplaces) )
    ##########
    
    # The following code associates the block being analyzed (through its FIPS code) to the isochrone block intersection with the safegraphplaces.
    isochrone_block_intersection <- cbind(counter_block, isochrone_block_intersection)
    
    # The following code take the individual block group isochrone - safegraphplaces points analysis and inserts the result into an aggregate data frame.
    isochrone_block_Stockton <- rbind(isochrone_block_Stockton, isochrone_block_intersection)
    
    # # I have commented this code out because, though it helps redue the potential for error, the specific error will almost never be found.
    # # By not including this code, though these will be potential for this error, we will enhance computational time. 
    #
    # # This code is necessary if there aren't any safegraphplaces within the isochrones. 
    # if(nrow(isochrone_block_intersection) != 0){
    #   
    #   # The following code performs an isochrone intersection between the block group isochrone and the 
    #   isochrone_block_intersection <- cbind(counter_block, isochrone_block_intersection)
    #   
    #   # The following code take the individual block group isochrone - safegraphplaces points analysis and inserts the result into an aggregate data frame.
    #   isochrone_block_Stockton <- rbind(isochrone_block_Stockton, isochrone_block_intersection)
    #   
    # }
    
    # This is evaluating each iteration.
    count <- count + 1
  
    # Once I have hit the amount of block iterations that I am interested in testing, this breaks the loop, and thus breaking the analysis. 
    if(count == n){break}
  
  }
  
}

blocks_amount_for_testing <- 1
res_OSRMisochrone <- microbenchmark(function_OSRMisochrone(blocks_amount_for_testing), times = 1L, unit = "s")
res_OSRMisochrone

maptest <- mapview(isochrone_block) + mapview(isochrone_block_points)

##########

# OSRM Approach #2: OSRM Routing

function_OSRMrouting <- function(blocks_amount_for_testing){

  # These variables (1) intake the iterations factor and (2) start the counting of each iteration. 
  n <- blocks_amount_for_testing
  count <- 0

  # Creation of a data frame that holds in all block group isochrone - safegraphplaces points intersections.
  route_OD_Stockton <- data.frame(stringsAsFactors = FALSE)

  # This first for loop chooses each block group for analysis.
  for(counter_block in sspz_centroid$GEOID){
  
    # The following code creates an isochrone at the centroid of the block group being analyzed.
    bg_centroid <- sspz_centroid[(sspz_centroid$GEOID == counter_block),]
  
    # This second for loop chooses each amenity for analysis.  
    for(counter_amenity in unique(safegraphplaces$sub_category)){
    
      # The following code chooses the safegraphplaces points associated with the sub-category being analyzed.
      safegraph_places <- st_as_sf(safegraphplaces[safegraphplaces$sub_category == counter_amenity, ], coords = c("longitude", "latitude"), crs = 4326)
    
        for(counter_point in 1:nrow(safegraph_places)){
        
          safegraph_point <- safegraph_places[counter_point, ]
          safegraph_point_filtered <- dplyr::select(safegraph_point, c("safegraph_place_id", "location_name", "sub_category", "naics_code",
                                                                       "street_address", "city", "state", "zip_code", "phone_number", "geometry"))
        
          OD_route <- t(osrmRoute(src = bg_centroid, dst = safegraph_point_filtered, overview = FALSE))
          OD_safegraph_route <- data.frame( cbind( counter_block, safegraph_point_filtered, OD_route ), stringsAsFactors = FALSE)

          route_OD_Stockton <- rbind(route_OD_Stockton, OD_safegraph_route)
        
        }
        
    }
  
    # This is the expression to be evaluted with the microbenchmark test.
    # test_microbenchmark <- route_OD_Stockton
  
    # This is evaluating each iteration.
    count <- count + 1
  
    # Once I have hit the amount of block iterations that I am interested in testing, this breaks the loop, and thus breaking the analysis. 
    if(count == n){break}
  
  }

}
  
##########

# Evaluation of the time it takes to run OSRM isochrone and OSRM routing for the above two functions.
# I will run these functions for both "function_OSRMisochrone" and "function_OSRMrouting".
# I have chosen to run these functions for the first 20 blocks in the sspz.

blocks_amount_for_testing <- 1

res_OSRMisochrone <- microbenchmark(function_OSRMisochrone(blocks_amount_for_testing), times = 1L, unit = "s")
print(res_OSRMisochrone)
boxplot(res_OSRMisochrone)

res_OSRMrouting <- microbenchmark(function_OSRMrouting(blocks_amount_for_testing), times = 1L, unit = "s")
print(res_OSRMrouting)
boxplot(res_OSRMrouting)

# OSRM Approach #3: OSRM Table

# test_osrm <- data.frame( osrmTable(src = sspz_centroid[, c("GEOID", "geometry")], dst = safegraphplaces[, c("safegraph_place_id", "longitude", "latitude")]) )
# I stopped using this approach because the "osrmTable" command would not work with such a large data set.

test <- left_join(isochrone_block_Stockton, route_OD_Stockton[, c("safegraph_place_id", "duration")], by = "safegraph_place_id")
test_cutoff <- test[test$max < 30, ]
test_cutoff$error_min <- (test_cutoff$center - test_cutoff$duration)
test_cutoff$error_perc <- abs( (test_cutoff$center - test_cutoff$duration) / test_cutoff$center) * 100
View(test_cutoff)

hist(test_cutoff$error_min, col = "blue", breaks = 50)
hist(test_cutoff[test_cutoff$max <= 20, "error_min"], col = "red", breaks = 50)

hist(test_cutoff$error_perc, col = "orange", breaks = 50)
hist(test_cutoff[test_cutoff$max <= 20, "error_perc"], col = "yellow", breaks = 50)

mean(test_cutoff$error_min)
mean(test_cutoff[test_cutoff$max <= 20, "error_min"])
mean(test_cutoff$error_perc)
mean(test_cutoff[test_cutoff$max <= 20, "error_perc"])

sd(test_cutoff$error_min)
sd(test_cutoff[test_cutoff$max <= 20, "error_min"])
sd(test_cutoff$error_perc)
sd(test_cutoff[test_cutoff$max <= 20, "error_perc"])
