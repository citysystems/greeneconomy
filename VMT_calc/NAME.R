### [NAME]

### Libraries

library(tidycensus)
library(dplyr)
library(censusapi)
library(tigris)
library(sf)
library(osrm)

### GeocodSL & token

token <- "iAnaXl7WPxEBOkbZRwlOFSpvjZo649W4mNt_gko387mIdt8MzjYy52umIX8aydDH"
source("https://raw.githubusercontent.com/cengel/ArcGIS_geocoding/master/SUL_gcFunctions.R")

### Functions for later use.

patterns_choice <- function(num){
  
  if(num == 1){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_01_patterns.csv"}
  else if(num == 2){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_02_patterns.csv"}
  else if(num == 3){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_03_patterns.csv"}
  else if(num == 4){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_04_patterns.csv"}
  else if(num == 5){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_05_patterns.csv"}
  else if(num == 6){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_06_patterns.csv"}
  else if(num == 7){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_07_patterns.csv"}
  else if(num == 8){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_08_patterns.csv"}
  else if(num == 9){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_09_patterns.csv"}
  else if(num == 10){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_10_patterns.csv"}
  else if(num == 11){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_11_patterns.csv"}
  else if(num == 12){patterns_text <- "C:/Users/Derek/Desktop/VMT_calc_important_files/m_patterns_new/m_12_patterns.csv"}
  else{patterns_text <-"error"}
  
  # if(num == 1){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_01_patterns.csv"}
  # else if(num == 2){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_02_patterns.csv"}
  # else if(num == 3){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_03_patterns.csv"}
  # else if(num == 4){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_04_patterns.csv"}
  # else if(num == 5){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_05_patterns.csv"}
  # else if(num == 6){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_06_patterns.csv"}
  # else if(num == 7){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_07_patterns.csv"}
  # else if(num == 8){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_08_patterns.csv"}
  # else if(num == 9){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_09_patterns.csv"}
  # else if(num == 10){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_10_patterns.csv"}
  # else if(num == 11){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_11_patterns.csv"}
  # else if(num == 12){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_12_patterns.csv"}
  # else{patterns_text <-"error"}
  
  return(patterns_text)
  
}
safegraphplaces_cleanse <- function(safegraphplaces){
  
  safegraphplaces <- dplyr::select(safegraphplaces, c("safegraph_place_id", "location_name", "latitude", "longitude", "street_address", "city", "state", "zip_code"))
  
  name_address <- paste(safegraphplaces$street_address, safegraphplaces$city, safegraphplaces$state, safegraphplaces$zip_code, sep = ", ")
  full_address <- paste(safegraphplaces$location_name, safegraphplaces$street_address, safegraphplaces$city, safegraphplaces$state, safegraphplaces$zip_code, sep = ", ")
  
  safegraphplaces <- cbind(safegraphplaces$safegraph_place_id, safegraphplaces$longitude, safegraphplaces$latitude, name_address, full_address)
  colnames(safegraphplaces) <- c("safegraph_place_id", "longitude", "latitude", "name_address", "full_address")
  
  safegraphplaces <- subset(safegraphplaces, !duplicated(safegraphplaces[,"full_address"]))
  
  safegraphplaces_cleanse <- data.frame(safegraphplaces, stringsAsFactors = FALSE)
  
  return(safegraphplaces_cleanse)
  
}

### Local server to run the st_intersects package at a faster rate.
options(osrm.server = "http://127.0.0.1:5000/")

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

### sboi_centroid generation

sjc_bgs <- block_groups("California", "San Joaquin County", cb = TRUE) %>% st_transform(st_crs(4326))

stockton_boundary <- 
  places("CA", cb = TRUE) %>% 
  filter(NAME == "Stockton")

stockton_boundary_buffer <-
  stockton_boundary %>% 
  st_transform(26910) %>% 
  st_buffer(1600) %>% 
  st_transform(st_crs(stockton_boundary))

# includes unincorporated areas on periphery that have addresses in Stockton, but removing Lodi and Manteca areas
stockton_bgs_full <- 
  block_groups("CA", cb = TRUE)[stockton_boundary_buffer,c("GEOID")] %>% 
  filter(!(GEOID %in% c("060770051351","060770040011","060770041061","060770041022")))

sjc_bgs_bounded <- sjc_bgs[sjc_bgs$GEOID %in% stockton_bgs_full$GEOID,]

sboi_centroid <- st_centroid(sjc_bgs_bounded)

### Creating the "append_matrix" variable, which inputs all locations of consideration.

append_matrix <- NULL
safegraphplaces_SJ <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/safegraphplaces.csv", header=TRUE, stringsAsFactors = FALSE)
safegraphplaces_CA <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/safegraphplaces_ca_edit.csv", header=TRUE, stringsAsFactors = FALSE)
safegraphplaces_tot <- rbind(safegraphplaces_SJ, safegraphplaces_CA)
safegraphplaces <- safegraphplaces_cleanse(safegraphplaces_tot)

for(counterMonth in 1:12){
  
  patterns_text <- patterns_choice(counterMonth)
  # filename <- paste("C:/Users/Derek/Desktop/m_patterns_new", substr(patterns_text, 38, 51), "_new.RData", sep = "")
  filename <- paste("C:/Users/Derek/Desktop/m_patterns_new", substr(patterns_text, 63, 76), "_new.RData", sep = "")
  load(filename)
  
  # unique_dest <- unique(m_patterns_new[, c("location_name", "name_address")])
  unique_dest <- unique(m_dest_matrix_sf[, c("location_name", "name_address")])
  
  append_matrix <- rbind(append_matrix, unique_dest)
  
  rm(m_origin_matrix_sf, m_dest_matrix_sf, m_patterns_new)
  
}

append_matrix_unique <- unique(append_matrix)

append_matrix_unique$full_address <- paste(append_matrix_unique$location_name, append_matrix_unique$name_address, sep = ", ")
# append_matrix_unique$location_name <- NULL
# append_matrix_unique$name_address <- NULL

all_dest <- left_join(append_matrix_unique, safegraphplaces[, c("safegraph_place_id", "longitude", "latitude", "full_address")], by = "full_address")

for (index_all_dest in 1:nrow(all_dest)){
  
  if(is.na(all_dest[index_all_dest, "longitude"])){
    
    location_name_iter <- all_dest[index_all_dest, c("name_address")]
    geo_coordinates <- geocodeSL(location_name_iter, token)
    
    all_dest[index_all_dest, c("longitude")] <- geo_coordinates[1]
    all_dest[index_all_dest, c("latitude")] <- geo_coordinates[2]
    
  }
  
  # print(index_all_dest)
  
}

# save(all_dest, file = "C:/Users/Derek/Desktop/all_dest.RData")

load(file = "C:/Users/Derek/Desktop/VMT_calc_important_files/all_dest.RData")
# load(file = "C:/Users/Derek/Desktop/all_dest.RData")

# Fixing the "NA" values within the "all_dest" data frame.

# NA 1

all_dest[15364, "name_address"] <- "234 broadway, san diego, ca, 92101"
all_dest[15364, "longitude"] <- geocodeSL(all_dest[15364, "name_address"], token)[1]
all_dest[15364, "latitude"] <- geocodeSL(all_dest[15364, "name_address"], token)[2]

# NA 2

all_dest[18282, "name_address"] <- "1980 College Blvd, Oceanside, CA 92054"
all_dest[18282, "longitude"] <- geocodeSL(all_dest[18282, "name_address"], token)[1]
all_dest[18282, "latitude"] <- geocodeSL(all_dest[18282, "name_address"], token)[2]

# NA 3

all_dest[34596, "name_address"] <- "6361 Wilshire Blvd Between San Vicente Blvd And Crescent Heights Blvd, Los Angeles, CA 90048"
all_dest[34596, "longitude"] <- geocodeSL(all_dest[34596, "name_address"], token)[1]
all_dest[34596, "latitude"] <- geocodeSL(all_dest[34596, "name_address"], token)[2]

# NA 4

all_dest[35301, "name_address"] <- "1335 E 103rd St, Los Angeles, CA 90002"
all_dest[35301, "longitude"] <- geocodeSL(all_dest[35301, "name_address"], token)[1]
all_dest[35301, "latitude"] <- geocodeSL(all_dest[35301, "name_address"], token)[2]

# NA 5

all_dest[52383, "name_address"] <- "701 E El Camino Real 3rd Floor, Mountain View, CA 94040"
all_dest[52383, "longitude"] <- geocodeSL(all_dest[52383, "name_address"], token)[1]
all_dest[52383, "latitude"] <- geocodeSL(all_dest[52383, "name_address"], token)[2]

# NA 6

all_dest[57379, "name_address"] <- "228 stanford shopping center, palo alto, ca, 94304"
all_dest[57379, "longitude"] <- geocodeSL(all_dest[57379, "name_address"], token)[1]
all_dest[57379, "latitude"] <- geocodeSL(all_dest[57379, "name_address"], token)[2]

# NA 7

all_dest[65307, "name_address"] <- "101 The City Dr S Building 22C, Orange, CA 92868"
all_dest[65307, "longitude"] <- geocodeSL(all_dest[65307, "name_address"], token)[1]
all_dest[65307, "latitude"] <- geocodeSL(all_dest[65307, "name_address"], token)[2]

# NA 8

all_dest[120953, "name_address"] <- "2054 W Redlands Blvd, Redlands, CA 92373"
all_dest[120953, "longitude"] <- geocodeSL(all_dest[120953, "name_address"], token)[1]
all_dest[120953, "latitude"] <- geocodeSL(all_dest[120953, "name_address"], token)[2]

# NA 9

all_dest[121557, "name_address"] <- "1300 university avenue, claremont, ca, 91711"
all_dest[121557, "longitude"] <- geocodeSL(all_dest[121557, "name_address"], token)[1] 
all_dest[121557, "latitude"] <- geocodeSL(all_dest[121557, "name_address"], token)[2]

# NA 10

all_dest[130932, "name_address"] <- "701 east el camino real, mountain view, ca, 94040"
all_dest[130932, "longitude"] <- geocodeSL(all_dest[130932, "name_address"], token)[1] 
all_dest[130932, "latitude"] <- geocodeSL(all_dest[130932, "name_address"], token)[2]

# NA 11

all_dest[143837, "name_address"] <- "116 Broadway Lane, Walnut Creek, CA 94596"
all_dest[143837, "longitude"] <- geocodeSL(all_dest[143837, "name_address"], token)[1] 
all_dest[143837, "latitude"] <- geocodeSL(all_dest[143837, "name_address"], token)[2]

# NA 12

all_dest[152229, "name_address"] <- "5400 date avenue, sacramento, ca, 95841"
all_dest[152229, "longitude"] <- geocodeSL(all_dest[152229, "name_address"], token)[1] 
all_dest[152229, "latitude"] <- geocodeSL(all_dest[152229, "name_address"], token)[2]

# NA 13

all_dest[160352, "name_address"] <- "600 indian wells lane, indian wells, ca, 92210"
all_dest[160352, "longitude"] <- geocodeSL(all_dest[160352, "name_address"], token)[1] 
all_dest[160352, "latitude"] <- geocodeSL(all_dest[160352, "name_address"], token)[2]

# NA 14

all_dest[161050, "name_address"] <- "7102 north fresno street, fresno, ca, 93720"
all_dest[161050, "longitude"] <- geocodeSL(all_dest[161050, "name_address"], token)[1] 
all_dest[161050, "latitude"] <- geocodeSL(all_dest[161050, "name_address"], token)[2]

# NA 15

all_dest[165214, "name_address"] <- "0 heathcliff drive, tiburon, ca, 94920"
all_dest[165214, "longitude"] <- geocodeSL(all_dest[165214, "name_address"], token)[1] 
all_dest[165214, "latitude"] <- geocodeSL(all_dest[165214, "name_address"], token)[2]

# NA 16

all_dest[168029, "name_address"] <- "350 girard street san francisco ca 94134"
all_dest[168029, "longitude"] <- geocodeSL(all_dest[168029, "name_address"], token)[1] 
all_dest[168029, "latitude"] <- geocodeSL(all_dest[168029, "name_address"], token)[2]

# NA 17

all_dest[174279, "name_address"] <- "cotai macau, los angeles, ca, 90272"
all_dest[174279, "longitude"] <- geocodeSL(all_dest[174279, "name_address"], token)[1] 
all_dest[174279, "latitude"] <- geocodeSL(all_dest[174279, "name_address"], token)[2]

# NA 18

all_dest[175526, "name_address"] <- "1 world way, los angeles, ca, 90045"
all_dest[175526, "longitude"] <- geocodeSL(all_dest[175526, "name_address"], token)[1] 
all_dest[175526, "latitude"] <- geocodeSL(all_dest[175526, "name_address"], token)[2]

# NA 19

all_dest[192653, "name_address"] <-  "310 easy street bunker school, mountain view, ca, 94129"
all_dest[192653, "longitude"] <- geocodeSL(all_dest[192653, "name_address"], token)[1] 
all_dest[192653, "latitude"] <- geocodeSL(all_dest[192653, "name_address"], token)[2]

# NA 20

all_dest[193795, "name_address"] <- "5110 foothills blvd roseville ca"
all_dest[193795, "longitude"] <- geocodeSL(all_dest[193795, "name_address"], token)[1] 
all_dest[193795, "latitude"] <- geocodeSL(all_dest[193795, "name_address"], token)[2]

# NA 21

all_dest[194479, "name_address"] <- "11301 wilshire boulevard, los angeles, ca, 90073"
all_dest[194479, "longitude"] <- geocodeSL(all_dest[194479, "name_address"], token)[1] 
all_dest[194479, "latitude"] <- geocodeSL(all_dest[194479, "name_address"], token)[2]

# NA 22

all_dest[198934, "name_address"] <- "10250 santa monica boulevard, los angeles, ca, 90067"
all_dest[198934, "longitude"] <- geocodeSL(all_dest[198934, "name_address"], token)[1] 
all_dest[198934, "latitude"] <- geocodeSL(all_dest[198934, "name_address"], token)[2]

# NA 23

all_dest[199363, "name_address"] <- "2664 griffith park boulevard, los angeles, ca, 90039"
all_dest[199363, "longitude"] <- geocodeSL(all_dest[199363, "name_address"], token)[1] 
all_dest[199363, "latitude"] <- geocodeSL(all_dest[199363, "name_address"], token)[2]

# NA 24

all_dest[210775, "name_address"] <- "2051 Marengo St, Los Angeles, CA 90033"
all_dest[210775, "longitude"] <- geocodeSL(all_dest[210775, "name_address"], token)[1] 
all_dest[210775, "latitude"] <- geocodeSL(all_dest[210775, "name_address"], token)[2]

# NA 25

all_dest[220288, "name_address"] <- "0 gilman street, albany, ca, 94710"
all_dest[220288, "longitude"] <- geocodeSL(all_dest[220288, "name_address"], token)[1] 
all_dest[220288, "latitude"] <- geocodeSL(all_dest[220288, "name_address"], token)[2]

# NA 26

all_dest[222887, "name_address"] <- "11190 warner avenue, fountain valley, ca, 92708"
all_dest[222887, "longitude"] <- geocodeSL(all_dest[222887, "name_address"], token)[1] 
all_dest[222887, "latitude"] <- geocodeSL(all_dest[222887, "name_address"], token)[2]

# NA 27

all_dest[227695, "name_address"] <- "640 roosevelt, irvine, ca, 92620"
all_dest[227695, "longitude"] <- geocodeSL(all_dest[227695, "name_address"], token)[1] 
all_dest[227695, "latitude"] <- geocodeSL(all_dest[227695, "name_address"], token)[2]

# NA 28

all_dest[232999, "name_address"] <- "46940 manises valencia espagne, valencia, ca, 91354"
all_dest[232999, "longitude"] <- geocodeSL(all_dest[232999, "name_address"], token)[1] 
all_dest[232999, "latitude"] <- geocodeSL(all_dest[232999, "name_address"], token)[2]

# NA 29

all_dest[247305, "name_address"] <- "calle 16, rancho cucamonga, ca, 91729"
all_dest[247305, "longitude"] <- geocodeSL(all_dest[247305, "name_address"], token)[1] 
all_dest[247305, "latitude"] <- geocodeSL(all_dest[247305, "name_address"], token)[2]

# NA 30

all_dest[249613, "name_address"] <- "2300 sweetwater road, national city, ca, 91950"
all_dest[249613, "longitude"] <- geocodeSL(all_dest[249613, "name_address"], token)[1] 
all_dest[249613, "latitude"] <- geocodeSL(all_dest[249613, "name_address"], token)[2]

# NA 31

all_dest[259702, "name_address"] <- "204 hampton drive, los angeles, ca, 90291"
all_dest[259702, "longitude"] <- geocodeSL(all_dest[259702, "name_address"], token)[1] 
all_dest[259702, "latitude"] <- geocodeSL(all_dest[259702, "name_address"], token)[2]

# NA 32

all_dest[260571, "name_address"] <- "interstate 15, san diego, ca, 92126"
all_dest[260571, "longitude"] <- geocodeSL(all_dest[260571, "name_address"], token)[1] 
all_dest[260571, "latitude"] <- geocodeSL(all_dest[260571, "name_address"], token)[2]

# NA 33

all_dest[260678, "name_address"] <- "101 south brand boulevard, glendale, ca, 91210"
all_dest[260678, "longitude"] <- geocodeSL(all_dest[260678, "name_address"], token)[1] 
all_dest[260678, "latitude"] <- geocodeSL(all_dest[260678, "name_address"], token)[2]

# NA 34

all_dest[276975, "name_address"] <- "84245 indio springs drive, indio, ca, 92203"
all_dest[276975, "longitude"] <- geocodeSL(all_dest[276975, "name_address"], token)[1] 
all_dest[276975, "latitude"] <- geocodeSL(all_dest[276975, "name_address"], token)[2]

# NA 35

all_dest[297306, "name_address"] <- "3243 south la cienega boulevard, los angeles, ca, 90016"
all_dest[297306, "longitude"] <- geocodeSL(all_dest[297306, "name_address"], token)[1] 
all_dest[297306, "latitude"] <- geocodeSL(all_dest[297306, "name_address"], token)[2]

# NA 36

all_dest[304685, "name_address"] <- "2198 filbert street, san francisco, ca, 94123"
all_dest[304685, "longitude"] <- geocodeSL(all_dest[304685, "name_address"], token)[1] 
all_dest[304685, "latitude"] <- geocodeSL(all_dest[304685, "name_address"], token)[2]

# NA 37

all_dest[310950, "name_address"] <- "10152 rancho carmel drive, san diego, ca, 92128"
all_dest[310950, "longitude"] <- geocodeSL(all_dest[310950, "name_address"], token)[1] 
all_dest[310950, "latitude"] <- geocodeSL(all_dest[310950, "name_address"], token)[2]

# NA 38

all_dest[328877, "name_address"] <- "701 east el camino real north wing second floor, mountain view, ca, 94040"
all_dest[328877, "longitude"] <- geocodeSL(all_dest[328877, "name_address"], token)[1] 
all_dest[328877, "latitude"] <- geocodeSL(all_dest[328877, "name_address"], token)[2]

# NA 39

all_dest[331796, "name_address"] <- "6382 east pacific coast highway, long beach, ca, 90803"
all_dest[331796, "longitude"] <- geocodeSL(all_dest[331796, "name_address"], token)[1] 
all_dest[331796, "latitude"] <- geocodeSL(all_dest[331796, "name_address"], token)[2]

# NA 40

all_dest[339936, "name_address"] <- "280 charles east young drive, los angeles, ca, 90095"
all_dest[339936, "longitude"] <- geocodeSL(all_dest[339936, "name_address"], token)[1] 
all_dest[339936, "latitude"] <- geocodeSL(all_dest[339936, "name_address"], token)[2]

# NA 41

all_dest[341723, "name_address"] <- "5453 b hollywood boulevard, los angeles, ca, 90027"
all_dest[341723, "longitude"] <- geocodeSL(all_dest[341723, "name_address"], token)[1] 
all_dest[341723, "latitude"] <- geocodeSL(all_dest[341723, "name_address"], token)[2]

# NA 42

all_dest[350985, "name_address"] <- "701 east el camino real, mountain view, ca, 94040"
all_dest[350985, "longitude"] <- geocodeSL(all_dest[350985, "name_address"], token)[1] 
all_dest[350985, "latitude"] <- geocodeSL(all_dest[350985, "name_address"], token)[2]

# NA 43

all_dest[369936, "name_address"] <- "4770 natomas boulevard, sacramento, ca, 95835"
all_dest[369936, "longitude"] <- geocodeSL(all_dest[369936, "name_address"], token)[1] 
all_dest[369936, "latitude"] <- geocodeSL(all_dest[369936, "name_address"], token)[2]

# NA 44

all_dest[370061, "name_address"] <- "6600 topanga boulevard, los angeles, ca, 91303"
all_dest[370061, "longitude"] <- geocodeSL(all_dest[370061, "name_address"], token)[1] 
all_dest[370061, "latitude"] <- geocodeSL(all_dest[370061, "name_address"], token)[2]

# NA 45

all_dest[385319, "name_address"] <- "11601 main street, sunol, ca, 94586"
all_dest[385319, "longitude"] <- geocodeSL(all_dest[385319, "name_address"], token)[1] 
all_dest[385319, "latitude"] <- geocodeSL(all_dest[385319, "name_address"], token)[2]

# NA 46

all_dest[400767, "name_address"] <- "790 inland center drive, san bernardino, ca, 92404"
all_dest[400767, "longitude"] <- geocodeSL(all_dest[400767, "name_address"], token)[1] 
all_dest[400767, "latitude"] <- geocodeSL(all_dest[400767, "name_address"], token)[2]

# NA 47

all_dest[405244, "name_address"] <- "5151 west channel islands boulevard, port hueneme, ca, 93041"
all_dest[405244, "longitude"] <- geocodeSL(all_dest[405244, "name_address"], token)[1] 
all_dest[405244, "latitude"] <- geocodeSL(all_dest[405244, "name_address"], token)[2]

# NA 48

all_dest[408271, "name_address"] <- "1675 owens street, san francisco, ca, 94158"
all_dest[408271, "longitude"] <- geocodeSL(all_dest[408271, "name_address"], token)[1] 
all_dest[408271, "latitude"] <- geocodeSL(all_dest[408271, "name_address"], token)[2]

# NA 49

all_dest[429857, "name_address"] <- "carretera poza rica cazones km 50 4706 col, bakersfield, ca, 93306"
all_dest[429857, "longitude"] <- geocodeSL(all_dest[429857, "name_address"], token)[1] 
all_dest[429857, "latitude"] <- geocodeSL(all_dest[429857, "name_address"], token)[2]

# NA 50

all_dest[431969, "name_address"] <- "juan de la luz enriquez 707 col, lookout, ca, 96054"
all_dest[431969, "longitude"] <- geocodeSL(all_dest[431969, "name_address"], token)[1] 
all_dest[431969, "latitude"] <- geocodeSL(all_dest[431969, "name_address"], token)[2]

# NA 51

all_dest[440631, "name_address"] <- "street cyril of jerusalem school, 4548 haskell avenue, encino, ca, 91436"
all_dest[440631, "longitude"] <- geocodeSL(all_dest[440631, "name_address"], token)[1] 
all_dest[440631, "latitude"] <- geocodeSL(all_dest[440631, "name_address"], token)[2]

# NA 52

all_dest[458309, "name_address"] <- "39000 bob hope drive, rancho mirage, ca, 92270"
all_dest[458309, "longitude"] <- geocodeSL(all_dest[458309, "name_address"], token)[1] 
all_dest[458309, "latitude"] <- geocodeSL(all_dest[458309, "name_address"], token)[2]

# NA 53

all_dest[459768, "name_address"] <- "701 east el camino real, mountain view, ca, 94040"
all_dest[459768, "longitude"] <- geocodeSL(all_dest[459768, "name_address"], token)[1] 
all_dest[459768, "latitude"] <- geocodeSL(all_dest[459768, "name_address"], token)[2]

# NA 54

all_dest[459804, "name_address"] <- "701 east el camino real, mountain view, ca, 94040"
all_dest[459804, "longitude"] <- geocodeSL(all_dest[459804, "name_address"], token)[1] 
all_dest[459804, "latitude"] <- geocodeSL(all_dest[459804, "name_address"], token)[2]

#####

osrmTable_Stockton_VMT_v3_10000 <- NULL
osrmTable_Stockton_VMT_v3_20000 <- NULL
osrmTable_Stockton_VMT_v3_30000 <- NULL
osrmTable_Stockton_VMT_v3_40000 <- NULL
osrmTable_Stockton_VMT_v3_50000 <- NULL
osrmTable_Stockton_VMT_v3_60000 <- NULL
osrmTable_Stockton_VMT_v3_70000 <- NULL
osrmTable_Stockton_VMT_v3_80000 <- NULL
osrmTable_Stockton_VMT_v3_90000 <- NULL
osrmTable_Stockton_VMT_v3_100000 <- NULL
osrmTable_Stockton_VMT_v3_110000 <- NULL
osrmTable_Stockton_VMT_v3_120000 <- NULL
osrmTable_Stockton_VMT_v3_130000 <- NULL
osrmTable_Stockton_VMT_v3_140000 <- NULL
osrmTable_Stockton_VMT_v3_150000 <- NULL
osrmTable_Stockton_VMT_v3_160000 <- NULL
osrmTable_Stockton_VMT_v3_170000 <- NULL
osrmTable_Stockton_VMT_v3_180000 <- NULL
osrmTable_Stockton_VMT_v3_190000 <- NULL
osrmTable_Stockton_VMT_v3_200000 <- NULL
osrmTable_Stockton_VMT_v3_210000 <- NULL
osrmTable_Stockton_VMT_v3_220000 <- NULL
osrmTable_Stockton_VMT_v3_230000 <- NULL
osrmTable_Stockton_VMT_v3_240000 <- NULL
osrmTable_Stockton_VMT_v3_250000 <- NULL
osrmTable_Stockton_VMT_v3_260000 <- NULL
osrmTable_Stockton_VMT_v3_270000 <- NULL
osrmTable_Stockton_VMT_v3_280000 <- NULL
osrmTable_Stockton_VMT_v3_290000 <- NULL
osrmTable_Stockton_VMT_v3_300000 <- NULL
osrmTable_Stockton_VMT_v3_310000 <- NULL
osrmTable_Stockton_VMT_v3_320000 <- NULL
osrmTable_Stockton_VMT_v3_330000 <- NULL
osrmTable_Stockton_VMT_v3_340000 <- NULL
osrmTable_Stockton_VMT_v3_350000 <- NULL
osrmTable_Stockton_VMT_v3_360000 <- NULL
osrmTable_Stockton_VMT_v3_370000 <- NULL
osrmTable_Stockton_VMT_v3_380000 <- NULL
osrmTable_Stockton_VMT_v3_390000 <- NULL
osrmTable_Stockton_VMT_v3_400000 <- NULL
osrmTable_Stockton_VMT_v3_410000 <- NULL
osrmTable_Stockton_VMT_v3_420000 <- NULL
osrmTable_Stockton_VMT_v3_430000 <- NULL
osrmTable_Stockton_VMT_v3_440000 <- NULL
osrmTable_Stockton_VMT_v3_450000 <- NULL
osrmTable_Stockton_VMT_v3_460000 <- NULL
osrmTable_Stockton_VMT_v3_470000 <- NULL

osrmTable_Stockton_VMT_v3_10000 <- do.call(rbind, lapply( 1:10000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_20000 <- do.call(rbind, lapply( 10001:20000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_30000 <- do.call(rbind, lapply( 20001:30000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_40000 <- do.call(rbind, lapply( 30001:40000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_50000 <- do.call(rbind, lapply( 40001:50000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_60000 <- do.call(rbind, lapply( 50001:60000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_70000 <- do.call(rbind, lapply( 60001:70000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_80000 <- do.call(rbind, lapply( 70001:80000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_90000 <- do.call(rbind, lapply( 80001:90000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_100000 <- do.call(rbind, lapply( 90001:100000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_110000 <- do.call(rbind, lapply( 100001:110000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_120000 <- do.call(rbind, lapply( 110001:120000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_130000 <- do.call(rbind, lapply( 120001:130000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_140000 <- do.call(rbind, lapply( 130001:140000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_150000 <- do.call(rbind, lapply( 140001:150000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_160000 <- do.call(rbind, lapply( 150001:160000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_170000 <- do.call(rbind, lapply( 160001:170000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_180000 <- do.call(rbind, lapply( 170001:180000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_190000 <- do.call(rbind, lapply( 180001:190000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_200000 <- do.call(rbind, lapply( 190001:200000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_210000 <- do.call(rbind, lapply( 200001:210000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_220000 <- do.call(rbind, lapply( 210001:220000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_230000 <- do.call(rbind, lapply( 220001:230000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_240000 <- do.call(rbind, lapply( 230001:240000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_250000 <- do.call(rbind, lapply( 240001:250000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_260000 <- do.call(rbind, lapply( 250001:260000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_270000 <- do.call(rbind, lapply( 260001:270000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_280000 <- do.call(rbind, lapply( 270001:280000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_290000 <- do.call(rbind, lapply( 280001:290000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_300000 <- do.call(rbind, lapply( 290001:300000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_310000 <- do.call(rbind, lapply( 300001:310000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_320000 <- do.call(rbind, lapply( 310001:320000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_330000 <- do.call(rbind, lapply( 320001:330000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_340000 <- do.call(rbind, lapply( 330001:340000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_350000 <- do.call(rbind, lapply( 340001:350000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_360000 <- do.call(rbind, lapply( 350001:360000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_370000 <- do.call(rbind, lapply( 360001:370000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_380000 <- do.call(rbind, lapply( 370001:380000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_390000 <- do.call(rbind, lapply( 380001:390000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_400000 <- do.call(rbind, lapply( 390001:400000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_410000 <- do.call(rbind, lapply( 400001:410000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_420000 <- do.call(rbind, lapply( 410001:420000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_430000 <- do.call(rbind, lapply( 420001:430000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_440000 <- do.call(rbind, lapply( 430001:440000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_450000 <- do.call(rbind, lapply( 440001:450000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_460000 <- do.call(rbind, lapply( 450001:460000, function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3_470000 <- do.call(rbind, lapply( 460001:nrow(all_dest), function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

osrmTable_Stockton_VMT_v3 <- rbind(osrmTable_Stockton_VMT_v3_10000, osrmTable_Stockton_VMT_v3_20000, osrmTable_Stockton_VMT_v3_30000, osrmTable_Stockton_VMT_v3_40000,
                                   osrmTable_Stockton_VMT_v3_50000, osrmTable_Stockton_VMT_v3_60000, osrmTable_Stockton_VMT_v3_70000, osrmTable_Stockton_VMT_v3_80000,
                                   osrmTable_Stockton_VMT_v3_90000, osrmTable_Stockton_VMT_v3_100000, osrmTable_Stockton_VMT_v3_110000, osrmTable_Stockton_VMT_v3_120000,
                                   osrmTable_Stockton_VMT_v3_130000, osrmTable_Stockton_VMT_v3_140000, osrmTable_Stockton_VMT_v3_150000, osrmTable_Stockton_VMT_v3_160000,
                                   osrmTable_Stockton_VMT_v3_170000, osrmTable_Stockton_VMT_v3_180000, osrmTable_Stockton_VMT_v3_190000, osrmTable_Stockton_VMT_v3_200000,
                                   osrmTable_Stockton_VMT_v3_210000, osrmTable_Stockton_VMT_v3_220000, osrmTable_Stockton_VMT_v3_230000, osrmTable_Stockton_VMT_v3_240000,
                                   osrmTable_Stockton_VMT_v3_250000, osrmTable_Stockton_VMT_v3_260000, osrmTable_Stockton_VMT_v3_270000, osrmTable_Stockton_VMT_v3_280000,
                                   osrmTable_Stockton_VMT_v3_290000, osrmTable_Stockton_VMT_v3_300000, osrmTable_Stockton_VMT_v3_310000, osrmTable_Stockton_VMT_v3_320000,
                                   osrmTable_Stockton_VMT_v3_330000, osrmTable_Stockton_VMT_v3_340000, osrmTable_Stockton_VMT_v3_350000, osrmTable_Stockton_VMT_v3_360000,
                                   osrmTable_Stockton_VMT_v3_370000, osrmTable_Stockton_VMT_v3_380000, osrmTable_Stockton_VMT_v3_390000, osrmTable_Stockton_VMT_v3_400000,
                                   osrmTable_Stockton_VMT_v3_410000, osrmTable_Stockton_VMT_v3_420000, osrmTable_Stockton_VMT_v3_430000, osrmTable_Stockton_VMT_v3_440000,
                                   osrmTable_Stockton_VMT_v3_450000, osrmTable_Stockton_VMT_v3_460000, osrmTable_Stockton_VMT_v3_470000)

save(osrmTable_Stockton_VMT_v3, file = "C:/Users/Derek/Desktop/osrmTable_Stockton_VMT_v3.RData")

####################################################################################################
####################################################################################################
####################################################################################################

### Old code. This worked well. However, once finished, the data took days (no exaggeration) to load.
### I want to think it has to do with the warnings produced at the end of this analysis, but I honestly have no idea.
### I did not have the time to find a formal solution to this problem, but the solution would truly enhance the whole VMT analysis.

osrmTable_Stockton_VMT_v3 <- NULL

osrmTable_Stockton_VMT_v3 <- do.call(rbind, lapply( 1:nrow(all_dest), function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

save(osrmTable_Stockton_VMT_v3, file = "C:/Users/Derek/Desktop/osrmTable_Stockton_VMT_v3.RData")

# Old code. The following was used for debugging. Ignore.

osrmTable_Stockton_VMT_v3 <- NULL

for (counterTable in 1:nrow(all_dest)){
  
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  osrmTable_Stockton_VMT_v3 <- rbind(osrmTable_Stockton_VMT_v3, safegraph_osrm)
  
  print(counterTable)
  
}

##########

# Old code. Ignore.

missing_dest <- all_dest[is.na(all_dest[,c("latitude"),]),]
missing_dest$name_address.y <- NULL
colnames(missing_dest)[2] <- "name_address"

num_missing_dest <- nrow(missing_dest)

for (index_missing_dest in 1:num_missing_dest){
  
  location_name_iter <- missing_dest[index_missing_dest, c("name_address")]
  geo_coordinates <- geocodeSL(location_name_iter)
  
  all_dest[all_dest[,c("full_address")] == missing_dest[index_missing_dest, c("full_address")], c("latitude")] = geo_coordinates[2]
  all_dest[all_dest[,c("full_address")] == missing_dest[index_missing_dest, c("full_address")], c("longitude")] = geo_coordinates[1]
  
}

all_dest$name_address.y <- NULL
colnames(all_dest)[2] <- "name_address"

osrm_drive_stockton_vmt <- do.call(rbind, lapply( 1:nrow(all_dest), function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("safegraph_place_id", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))