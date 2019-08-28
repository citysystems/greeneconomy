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

# Creation of the "break" times used for the iscochrone creation.
break_times <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 30, 40, 50, 60)

# Creation of a data frame that holds in all block group isochrone - safegraphplaces points intersections.
isochrone_block_Stockton <- data.frame(stringsAsFactors = FALSE)

# This first for loop chooses each block group for analysis.
for(counter_block in sspz_centroid$GEOID){

  # The following code creates an isochrone at the centroid of the block group being analyzed.
  isochrone_block <- osrmIsochrone(sspz_centroid[(sspz_centroid$GEOID == counter_block[1]),], breaks = break_times, returnclass = "sf")
  isochrone_block <- st_set_crs(isochrone_block, '+proj=longlat +datum=WGS84')

  # This second for loop chooses each amenity for analysis.  
  for(counter_amenity in unique(safegraphplaces$sub_category)){
  
    # The following code chooses the safegraphplaces points associated with the sub-category being analyzed.
    isochrone_block_points <- st_as_sf(safegraphplaces[safegraphplaces$sub_category == counter_amenity[1], ], coords = c("longitude", "latitude"), crs = 4326)
    
    # The following code performs an isochrone intersection between the block group isochrone and the 
    isochrone_block_intersection <- cbind(counter_block[1], data.frame(st_intersection(isochrone_block_points, isochrone_block)), stringsAsFactors = FALSE)
   
    # The following code take the individual block group isochrone - safegraphplaces points analysis and inserts the result into an aggregate data frame.
    isochrone_block_Stockton <- rbind(isochrone_block_Stockton, isochrone_block_intersection)
     
  }
  
}

map_test <- mapview(isochrone_block_points) + mapview(isochrone_block)

##########

# OSRM Approach #2: OSRM Routing

# Creation of a data frame that holds in all block group isochrone - safegraphplaces points intersections.
isochrone_block_Stockton <- data.frame(stringsAsFactors = FALSE)
test <- data.frame()

count <- 0

# This first for loop chooses each block group for analysis.
for(counter_block in sspz_centroid$GEOID){
  
  # The following code creates an isochrone at the centroid of the block group being analyzed.
  bg_centroid <- sspz_centroid[(sspz_centroid$GEOID == counter_block),]
  
  # This second for loop chooses each amenity for analysis.  
  for(counter_amenity in unique(safegraphplaces$sub_category)){
    
    # The following code chooses the safegraphplaces points associated with the sub-category being analyzed.
    safegraph_places <- st_as_sf(safegraphplaces[safegraphplaces$sub_category == counter_amenity, ], coords = c("longitude", "latitude"), crs = 4326)
    
      for(counter_point in nrow(isochrone_block_points)){
        
        safegraph_point <- safegraph_places[counter_point, ]
        
        test <- rbind(test, osrmRoute(src = bg_centroid, dst = safegraph_point, overview = FALSE))
        
        count <- count + 1
        print(count)
        
      }
    
    # The following code performs an isochrone intersection between the block group isochrone and the 
    # isochrone_block_intersection <- cbind(counter_block[1], data.frame(st_intersection(isochrone_block_points, isochrone_block)), stringsAsFactors = FALSE)
    
    # The following code take the individual block group isochrone - safegraphplaces points analysis and inserts the result into an aggregate data frame.
    # isochrone_block_Stockton <- rbind(isochrone_block_Stockton, isochrone_block_intersection)
    
  }
  
}

##########