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

# Uploading the South Stockton Promise Zone (SSPZ) and the Stockton Spheres of Influence. 
sspzboundary <- st_read("C:/Users/Derek/Desktop/sspzboundary.shp") %>% st_transform(st_crs(4326))
stockton_boundary_influence <- (st_read("C:/Users/Derek/Desktop/SpheresOfInfluence/SpheresOfInfluence.shp") %>% filter(SPHERE == "STOCKTON") %>% st_transform(st_crs(4326)))[1,]

# Taking the block groups within Stockton and filtering them by those within the following:
# (1) Stockton Boundary of Influence (SBOI)
# (2) Stockton South Promise Zone (SSPZ)

sjc_bgs <- block_groups("California", "San Joaquin County", cb = TRUE) %>% st_transform(st_crs(4326))

sboi_bgs <- sjc_bgs[which(sjc_bgs$GEOID %in% st_centroid(sjc_bgs)[stockton_boundary_influence,]$GEOID),]
sboi_centroid <- st_centroid(sboi_bgs)

##########

##########

# Use only for when analyzing the South Stockton Promise Zone.

# sspz_bgs <- sjc_bgs[which(sjc_bgs$GEOID %in% st_centroid(sjc_bgs)[sspzboundary,]$GEOID),]
# sspz_centroid <- st_centroid(sspz_bgs)

##########

# Uploading the San Joaquin safegraphplaces dataset.
safegraphplaces <- read.csv("C:/Users/Derek/Desktop/safegraphplaces.csv", header=TRUE, stringsAsFactors = FALSE)
safegraphplaces <- safegraphplaces[!is.na(safegraphplaces$sub_category), ]
safegraphplaces$full_address <- paste(safegraphplaces$location_name, safegraphplaces$street_address,
                                      safegraphplaces$city, safegraphplaces$state, safegraphplaces$zip_code, sep = ", ")

##########

osrmTable_Stockton_VMT <- do.call(rbind, lapply(1:nrow(safegraphplaces), function(counterTable){
  
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = safegraphplaces[counterTable, c("safegraph_place_id", "longitude", "latitude")]) )
  
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, safegraphplaces[counterTable, c("safegraph_place_id", "top_category", "sub_category", "full_address", "location_name", "street_address", "city", "state", "zip_code")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "top_category", "sub_category", "full_address", "location_name", "street_address",
                                "city", "state", "zip_code", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

saveRDS(osrmTable_Stockton_VMT, "C:/Users/Derek/Desktop/osrmTable_Stockton_VMT.RData")

# osrmTable_Stockton_VMT <- left_join(osrmTable_Stockton_VMT, safegraphplaces[ , c("safegraph_place_id", "full_address")], by = "safegraph_place_id")
