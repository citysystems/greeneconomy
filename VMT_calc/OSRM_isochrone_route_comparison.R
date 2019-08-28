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

# Creation of a data frame that holds in all block group isochrone - safegraphplaces points.
isochrone_block_Stockton <- data.frame(stringsAsFactors = FALSE)

# This first for loop chooses each block group for analysis.
for(counter_block in sspz_centroid$GEOID){

  # The following code creates an isochrone at the centroid of the block group being analyzed.
  isochrone_block <- osrmIsochrone(sspz_centroid[(sspz_centroid$GEOID == counter_block[1]),], breaks = break_times, returnclass = "sf")
  isochrone_block <- st_set_crs(isochrone_block, '+proj=longlat +datum=WGS84')

  # This second for loop chooses each amenity for analysis.  
  for(counter_pivot in unique(sub_category_trvtime$sub_category)){
  
    # The following code chooses the safegraphplaces points associated with the sub-category being analyzed.
    isochrone_block_points <- st_as_sf(safegraphplaces[safegraphplaces$sub_category == counter_pivot[1], ], coords = c("longitude", "latitude"), crs = 4326)
    
    # The following code performs an isochrone intersection between the block group isochrone and the 
    isochrone_block_intersection <- cbind(counter_block[1], data.frame(st_intersection(isochrone_block_points, isochrone_block)), stringsAsFactors = FALSE)
   
    # 
    isochrone_block_Stockton <- rbind(isochrone_block_Stockton, isochrone_block_intersection)
     
  }
  
}

map_test <- mapview(isochrone_block_points) + mapview(isochrone_block)

##########

# OSRM Approach #2: OSRM Routing




##########

# Code used to create the "weights" table.

weights <- month_patterns_new(month_patterns_join_all)

##########

### Code used to answer the questions posed in the survey's "current average" column.

# Answer to question #1: "How long does it take to get there?"

col_filter <- NHTS_lookup(weights$NHTS) + 1
trav_dec_coeff <- t(NHTS_travelDecay[1, col_filter])
med_trav_time <- data.frame(weights$sub_category, log(0.5)/trav_dec_coeff)
rownames(med_trav_time) <- c(1:nrow(med_trav_time))
colnames(med_trav_time) <- c("sub_category", "med_trav_time")
sub_category_trvtime <- left_join(weights, med_trav_time, by = "sub_category")

# Answer to question #2: "How many unique options are available in an average trip?"

# Creating a vector of block ids for Stockton.
Stockton_blocks <- paste("0",as.character(scoreScenarioBaseline_AGG$id), sep = "")

# Creating block shapefiles for preparation of the isochrone analysis.
ca_blocks <- blocks("CA", county = "077")
ca_blocks <- st_centroid(ca_blocks)

# Using the census API to create block groups and use census information in Stockton.
census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab", overwrite = TRUE)
key_census <- "c8aa67e4086b4b5ce3a8717f59faa9a28f611dab"
blocks_pop <- getCensus(name = "dec/sf1", vintage = 2010, vars = "P001001", region = "block:*", regionin = "state:06+county:077+tract:*", key = key_census)
blocks_pop <- data.frame(cbind(paste(blocks_pop$state, blocks_pop$county, blocks_pop$tract, blocks_pop$block, sep = ""), blocks_pop$P001001))
colnames(blocks_pop) <- c("GEOID10", "population")
# "P001001" is the populaiton variable that quantifies the population per block. 

# Inserting the "South Stockton Promise Zone" boundary.
sspzboundary <- st_read("S:/CCF/sspzboundary/sspzboundary.shp") %>% st_transform(st_crs(4326))

# Creation of the block shapefile with the population count.
ca_blocks_pop <- left_join(ca_blocks, blocks_pop, by = "GEOID10")
ca_blocks_pop <- select(ca_blocks_pop, c("GEOID10", "INTPTLAT10", "INTPTLON10", "population", "geometry"))
ca_blocks_pop <- subset(ca_blocks_pop, (ca_blocks_pop$GEOID10 %in% Stockton_blocks)) # %>% st_transform(st_crs(4326)

# Filtering Stockton's blocks to include only those within the "South Stockton Promise Zone" boundary.
# ca_blocks_pop <- data.frame(st_intersection(ca_blocks_pop, sspzboundary))

# Process to understand how many amenities are within the median driving distance to a block's centroid, per amenity type.
amenities_persubcat_perblock <- data.frame(matrix(nrow = nrow(ca_blocks_pop), ncol = nrow(sub_category_trvtime), NA))
rownames(amenities_persubcat_perblock) <- ca_blocks_pop$GEOID10
colnames(amenities_persubcat_perblock) <- sub_category_trvtime$sub_category

count <- 0

for(counter_pivot in unique(sub_category_trvtime$sub_category)){
  
  isochrone_block_points <- st_as_sf(safegraphplaces[safegraphplaces$sub_category == counter_pivot, ], coords = c("longitude", "latitude"), crs = 4326)
  med_trav_time_cat <- sub_category_trvtime[sub_category_trvtime$sub_category == counter_pivot, "med_trav_time"]
  
  # Consider getting rid of the following in the "for" loop for future iterations: [ca_blocks_pop$population != 0, ]
  
  for(counter_block in ca_blocks_pop$GEOID10){
    
    isochrone_block <- osrmIsochrone(ca_blocks_pop[(ca_blocks_pop$GEOID10 == counter_block), ], breaks = med_trav_time_cat, returnclass = "sf")    
    isochrone_block <- st_set_crs(isochrone_block, '+proj=longlat +datum=WGS84')
    
    isochrone_block_intersection <- data.frame(st_intersects(isochrone_block_points, isochrone_block, sparce = FALSE))  
    # Another way to perform the above line: isochrone_block_intersection <- isochrone_block_points[isochrone_block,]
    
    amenities_persubcat_perblock[counter_block, counter_pivot] <- sum(isochrone_block_intersection$col.id)   
    
    count <- count + 1
    print(count)
    
  } 
  
}

avg_amenity_num <- data.frame(colSums(ca_blockgroups_pop$population * amenities_persubcat_perblock) / sum(ca_blockgroups_pop$population))
avg_amenity_num <- cbind(sub_category_trvtime$sub_category, avg_amenity_num)
rownames(avg_amenity_num) <- c(1:nrow(avg_amenity_num))
colnames(avg_amenity_num) <- c("sub_category", "avg_amenity_num")

sub_category_trvtime <- left_join(sub_category_trvtime, avg_amenity_num, by = "sub_category")

##########

# Extra plotting.

long_Stockton <- -121.2908
lat_Stockton <- 37.9577

isochrone_block_plot <- safegraphplaces
plot(st_geometry(isochrone_block), col = c('grey80', 'grey60', 'grey50', 'grey40', 'grey30', 'grey20'))
points(isochrone_block_plot$latitude ~ isochrone_block_plot$longitude, col = "red", cex = 1)

isochrone_block$drive_time <- factor(paste(isochrone_block$min, "to", isochrone_block$max, "min"))
factpal <- colorFactor(rev(heat.colors(5)), isochrone_block$drive_time)

leaflet() %>%
  setView(long_Stockton, lat_Stockton, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron", group = "Greyscale") %>%
  addMarkers(lng = long_Stockton, lat = lat_Stockton) %>%
  addPolygons(fill = TRUE, stroke = TRUE, color = "black",
              fillColor = ~factpal(isochrone_block$drive_time),
              weight = 0.5, fillOpacity = 0.2,
              data = isochrone_block, popup = isochrone_block$drive_time,
              group = "Drive Time") %>%
  addLegend("bottomright", pal = factpal, values = isochrone_block$drive_time, title = "Drive Time")

##########

# Do this at some point.

### osrmRoute time & distance prep.

# OD_time_dist <- do.call(rbind,lapply(1:nrow(m_1_patterns_new),function(row){
#   osrmRoute(src = m_origin_sf[row, ],
#             dst = m_dest_sf[row, ], overview = FALSE)
# }))

# Answer to question #3: "About how many trips are taken to this amenity type each month?"

trips_permonth <- data.frame(sub_category_trvtime$sub_category, ( sub_category_trvtime$tot_visit_counts/(12 * nrow(ca_blockgroups_pop)) * (sub_category_trvtime$avg_amenity_num/sub_category_trvtime$count_places) ), stringsAsFactors = FALSE)
colnames(trips_permonth) <- c("sub_category", "visit_counts_permonth")
# Take a look at the "tot_visit_counts" column.

trips_permonth_peramenity <- data.frame(sub_category_trvtime$sub_category, ( sub_category_trvtime$tot_visit_counts/(12 * nrow(ca_blockgroups_pop)) * (1/sub_category_trvtime$count_places) ), stringsAsFactors = FALSE)
colnames(trips_permonth_peramenity) <- c("sub_category", "visit_counts_permonth_peramen")

trip_to_amenity <- med_trav_time %>% 
  left_join(avg_amenity_num, by = "sub_category") %>%
  left_join(trips_permonth, by = "sub_category") %>%
  left_join(trips_permonth_peramenity, by = "sub_category")

sub_category_trvtime <- sub_category_trvtime %>%
  left_join(trips_permonth, by = "sub_category") %>%
  left_join(trips_permonth_peramenity, by = "sub_category")

# Answer to question #4: "What proportion of trips are taken without a car?"

prop_drive <- t(NHTS_filter[1, col_filter]/colSums(NHTS_filter[1:4, col_filter]))
prop_no_drive <- data.frame(weights$sub_category, (1 - prop_car) * 100)
rownames(prop_no_drive) <- c(1:nrow(prop_no_drive))
colnames(prop_no_drive) <- c("sub_category", "perc_no_drive")

trip_to_amenity <- left_join(trip_to_amenity, prop_no_drive, by = "sub_category")
sub_category_trvtime <- left_join(sub_category_trvtime, prop_no_drive, by = "sub_category")

# Answer to question #5: "How would you divide this proportion by biking, walking, and public transit?"
### You may not need to answer this question for the "current average" column. 

prop_other <- t(NHTS_filter[2:4, col_filter])/colSums(NHTS_filter[1:4, col_filter])
prop_bike_pubtrans_walk <- data.frame(weights$sub_category, prop_other * 100)
rownames(prop_bike_pubtrans_walk) <- c(1:nrow(prop_bike_pubtrans_walk))
colnames(prop_bike_pubtrans_walk) <- c("sub_category", "perc_bike", "perc_pubtrans", "perc_walk")

trip_to_amenity <- left_join(trip_to_amenity, prop_bike_pubtrans_walk, by = "sub_category")
sub_category_trvtime <- left_join(sub_category_trvtime, prop_bike_pubtrans_walk, by = "sub_category")

##########

# Answer to Derek's question: Can you check to see what the discrepancy is? What are those other sub-categories?

other_discrep <-unique(subset(safegraphplaces$sub_category, !(safegraphplaces$sub_category %in% sub_category_trvtime$sub_category)))
write.csv(other_discrep, file = "C:/Users/Derek/Desktop/other_discrep.csv")
