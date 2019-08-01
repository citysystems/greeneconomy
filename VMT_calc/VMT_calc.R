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

##########

# Downloading Safegraph patterns .csv files.

num = 1 # Change this to a for loop once you have figured out how to calculate the nudges.

patterns_text <- patterns_choice(num)
m_patterns <- m_patterns_cleanse(patterns_text)
m_patterns_join <- m_patterns_join_cleanse(m_patterns, safegraphplaces)

# Finding the census population for each block group goint to amenities in Stockton.
pop_bg_stockton <- pop_blockgroup_stockton

# m_hps from "home_panel_summary"
m_hps <- home_panel_summary(num)

# Looping through the data to disaggregate the census blocks from the monthly patterns .csv files.
m_patterns_new <- month_patterns_new(m_patterns_join, pop_bg_stockton)

  ### Creation of the "m_origins" and "m_dest" variables.

# Origin Points for Trips (for osrmRoute)
m_origin_sf <- month_origin(m_patterns_new)

# Destination Points for Trips (for osrmRoute)
m_dest_sf <- month_dest(m_patterns_new, m_origin_sf)

# Origin Points for Locations
# This includes finding the census population for each block group going to amenities in Stockton.
m_origin_matrix_sf <- month_origin_matrix(m_patterns_new, m_hps, pop_bg_stockton)

# Destination Points for Location
m_dest_matrix_sf <- month_dest_matrix(m_patterns_join)

##########

  ### osrmRoute time & distance prep.

##########

OD_time_dist <- do.call(rbind,lapply(1:nrow(m_1_patterns_new),function(row){
  osrmRoute(src = m_origin_sf[row, ],
            dst = m_dest_sf[row, ], overview = FALSE)
}))

##########

  ### Calculation of NHTS nudge values for the VMT calculations.

##########

### Nudge: Performing the linked trips conversion factor.

NHTS_LinkedTripsConv <- nudge_LinkedTrips(NHTS_df_final)

### Nudge: Peforming the median to mean converison.

NHTS_MeanMedConv <- nudge_MeanMedConv(NHTS_df_final)

### Nudge: Peforming the carpooling and vehicle vs. other transit mode nudge.

NHTS_carpoolVehicleFilter <- NHTS_carpoolVehicleFilter(NHTS_df_final)

##########

  ### Calculation of the VMTs per origin, per destination, and in total.
  ### "as-crow-flies" (ACF) calculation for origin-destination pairs.

##########

dest_amenities_matrix <- data.frame(unique(m_origin_matrix_sf$full_address))
colnames(dest_amenities_matrix) <- "name_address"
dest_num <- nrow(dest_amenities_matrix)

# count_dist_start <- 1
# count_dist_end <- 0

# distance <- OD_time_dist[, "distance"]

VMT_Origin_recorded <- data.frame(stringsAsFactors = FALSE)
VMT_Origin_non_recorded <- 0
# VMT_Dest <- data.frame(stringsAsFactors = FALSE)

conv_MeterToMile <- 0.000621371
factor_twoWayTrip <- 2

# m_dest_matrix_sf$VMT <- NA

for(counterVMT in 1:dest_num){
  
  origin_matrix <- m_origin_matrix_sf[m_origin_matrix_sf$full_address == dest_amenities_matrix[counterVMT, ], ]
  dest_vector <- m_dest_matrix_sf[m_dest_matrix_sf$full_address == dest_amenities_matrix[counterVMT, ], ]
  # dest_matrix_data <- m_1_dest_matrix_sf[m_1_dest_matrix_sf$name_address == dest_amenities_matrix[counterVMT, ], ]
  
  # ACF_safegraph <- as.numeric(dest_vector$distance_from_home)
  # ACF_OD <- as.numeric(median(st_distance(origin_matrix, dest_vector)))
  
  # count_dist_end <- count_dist_end + nrow(origin_matrix)
  
  # distance_new <- distance[count_dist_start:count_dist_end] * ACF_safegraph / ACF_OD * NHTS_MeanMedConv * NHTS_LinkedTripsConv
  distance_new <- as.numeric(origin_matrix$distance_from_home) * factor_twoWayTrip * conv_MeterToMile * NHTS_MeanMedConv * NHTS_LinkedTripsConv * NHTS_carpoolVehicleFilter
  
  VMT_bg <- data.frame(origin_matrix$visit_count * distance_new * as.numeric(origin_matrix$origin_population) / origin_matrix$number_devices_residing)
  VMT_Origin_recorded <- rbind(VMT_Origin_recorded, VMT_bg)
  
  proportion_Stockton <- sum(!is.na(origin_matrix$origin_population)) / nrow(data.frame(origin_matrix$origin_population))
  VMT_non_recorded <- sum(VMT_bg[!is.na(origin_matrix$origin_population), ]) * proportion_Stockton * factor_twoWayTrip
  VMT_Origin_non_recorded <- VMT_Origin_non_recorded + VMT_non_recorded
  # VMT_Origin_non_recorded <- VMT_Origin_non_recorded + (VMT_non_recorded * 1/nrow(pop_bg_stockton))
  
  # ??? ... I'm a little skeptical about this analysis. I forget why I included "VMT_scaleFullPop" and "VMT_dest".
  # VMT_bgSum <- sum(VMT_bg)
  # VMT_scaleFullPop <- as.numeric(dest_vector$raw_visit_counts) / sum(origin_matrix$visit_count)
  # VMT_dest <- VMT_scaleFullPop * VMT_bgSum
  # ???
  
  # count_dist_start <- count_dist_start + nrow(origin_matrix)
  
  # m_dest_matrix_sf[m_dest_matrix_sf$full_address == dest_amenities_matrix[counterVMT, ], "VMT"] <- VMT_dest

  # VMT_Dest <- rbind(VMT_Dest,VMT_dest)
    
}

colnames(VMT_Origin_recorded) <- "VMT_Origin"
m_origin_matrix_sf <- cbind(m_origin_matrix_sf, VMT_Origin_recorded)

m_origin_Stockton <- m_origin_matrix_sf[!is.na(m_origin_matrix_sf$origin_population), ]
m_origin_StocktonBG_freq <- count(m_origin_Stockton, vars = "origin")
m_origin_StocktonBG_VMT <- count(m_origin_Stockton, vars = "origin", wt_var = "VMT_Origin")

VMT_Stockton_bg <- data.frame(pop_bg_stockton$origin); colnames(VMT_Stockton_bg) <- "origin"
VMT_Stockton_bg <- left_join(VMT_Stockton_bg, m_origin_StocktonBG_freq, by = "origin")
VMT_Stockton_bg <- left_join(VMT_Stockton_bg, m_origin_StocktonBG_VMT, by = "origin")
VMT_Stockton_bg <- cbind(VMT_Stockton_bg, (VMT_Origin_non_recorded / nrow(pop_bg_stockton)))

colnames(VMT_Stockton_bg) <- c("origin", "frequency", "VMTs_recorded", "VMTs_unrecorded")
VMT_Stockton_bg[is.na(VMT_Stockton_bg$frequency), "frequency"] <- 0
VMT_Stockton_bg[is.na(VMT_Stockton_bg$VMTs_recorded), "VMTs_recorded"] <- 0
VMT_Stockton_bg$VMT_total <- VMT_Stockton_bg$VMTs_recorded + VMT_Stockton_bg$VMTs_unrecorded
VMT_Stockton_bg <- cbind(VMT_Stockton_bg, pop_bg_stockton$geometry)

#Outlier
VMT_Stockton_bg[94, "VMT_total"] <- 0

VMT_Stockton_bg_sf <- st_as_sf(VMT_Stockton_bg)
mapview(VMT_Stockton_bg_sf, zcol = "VMT_total")

# colnames(VMT_perDest) <- "VMT_perDest"

# VMT_Avg <- mean(VMT_perDest$VMT_perDest)
# m_dest_matrix_sf[is.na(m_1_dest_matrix_sf$VMT), "VMT"] <- VMT_Avg
# VMT_tot <- sum(m_dest_matrix_sf$VMT)

# ratio_test <- sum(m_1_origin_matrix_sf[!is.na(m_1_origin_matrix_sf$origin_population),]$visit_count) / sum(m_1_origin_matrix_sf$visit_count)

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
