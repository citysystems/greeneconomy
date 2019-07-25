library(tidycensus)
library(censusapi)
library(tigris)
library(dplyr)
library(ggplot2)
library(lehdr)
library(sf)
library(osrm)
library(mapview)
library(readr)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
options(osrm.server = "http://127.0.0.1:5000/")
census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab", overwrite = TRUE)

acs5_17 <- load_variables(2017, "acs5")

california_place_medincome <- get_acs(geography = "place", 
              variables = c(medincome = "B19013_001", population = "B01003_001"),
              state = "CA", output = "wide")

california_place_medincome <- california_place_medincome %>% arrange(desc(populationE))

california_place_medincome %>% top_n(20, populationE) %>% 
  mutate(NAME = gsub(" city, California", "", NAME)) %>%
  ggplot(aes(x = medincomeE, y = reorder(NAME, populationE))) +
  geom_errorbarh(aes(xmin = medincomeE - medincomeM, xmax = medincomeE + medincomeM)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income of 20 most populous cities in California",
       subtitle = "2013-2017 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

compare_medincome <- get_acs(geography = "place", variables = c(medincome = "B19013_001", population = "B01003_001", laborforce= "B23025_002"), output = "wide") %>% filter(NAME %in% c("Stockton city, California", 
                                                                                                                  "Fort Collins city, Colorado",
                                                                                                                  "Denver city, Colorado",
                                                                                                                  "St. Louis city, Missouri", 
                                                                                                                  "Cleveland city, Ohio", 
                                                                                                                  "Pittsburgh city, Pennsylvania", 
                                                                                                                  "Honolulu city, Hawaii",
                                                                                                                  "St. Paul city, Minnesota",
                                                                                                                  "Corpus Christi city, Texas",
                                                                                                                  "Sacramento city, California",
                                                                                                                  "Jackson city, Mississippi",
                                                                                                                  "South Bend city, Indiana")) 

compare_medincome %>% mutate(NAME = gsub(" city", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Median household income",
       subtitle = "2013-2017 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

ca_estimates_housing <- get_estimates(geography = "county", state = "CA", county = "San Joaquin", product = "housing", time_series = TRUE)


#LODES

ca_lodes <- grab_lodes(state = "ca", year = 2015, lodes_type = "od", job_type = "JT01", 
           segment = "S000", state_part = "main", agg_geo = "bg")

ca_rac <- grab_lodes(state = "ca", year = 2015, lodes_type = "rac", job_type = "JT01", 
                     segment = "S000", state_part = "main", agg_geo = "bg")

ca_wac <- grab_lodes(state = "ca", year = 2015, lodes_type = "wac", job_type = "JT01", 
                     segment = "S000", state_part = "main", agg_geo = "bg")

ca_bgs <- block_groups("CA", cb = TRUE)
ca_counties <- counties("CA", cb = TRUE)
stockton_boundary <- places("CA", cb = TRUE) %>% filter(NAME == "Stockton")
stockton_bgs <- ca_bgs[which(ca_bgs$GEOID %in% st_centroid(ca_bgs)[stockton_boundary,]$GEOID),c("GEOID")]
stockton_bgs_full <- ca_bgs[stockton_boundary,c("GEOID")] %>% filter(!(GEOID %in% c("060770040011","060770039001","060770041061","060770039001","060770039002","060770051311","060770051351","060770041022")))

stockton_lodes_w <- ca_lodes[which(ca_lodes$w_bg %in% stockton_bgs$GEOID),]
stockton_lodes_h <- ca_lodes[which(ca_lodes$h_bg %in% stockton_bgs$GEOID),]
stockton_rac <- stockton_bgs_full %>% geo_join(ca_rac, "GEOID", "h_bg")
stockton_wac <- stockton_bgs_full %>% geo_join(ca_wac, "GEOID", "w_bg")

save(stockton_lodes_w, stockton_lodes_h, stockton_rac, stockton_wac, file = "C:\\Users\\Derek Ouyang\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes.R")
load("C:\\Users\\Derek Ouyang\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes.R")

stockton_lodes_origin_centroids <- st_centroid(ca_bgs[which(ca_bgs$GEOID %in% stockton_lodes_h$h_bg),])
stockton_lodes_dest_centroids <- st_centroid(ca_bgs[which(ca_bgs$GEOID %in% stockton_lodes_h$w_bg),])
stockton_lodes_dest_bg <- ca_bgs[which(ca_bgs$GEOID %in% stockton_lodes_h$w_bg),]

# below is an alternate method of creating an OD matrix using a specific function in the osrm package, but it's less useful because it doesn't include distance as an output. the osrmRoute below it take longer but it gets both duration and distance.

# od_matrix <- do.call(cbind,lapply(1:56,function(x){
#   rbind(osrmTable(src = stockton_lodes_origin_centroids[1:100,],
#                        dst = stockton_lodes_dest_centroids[(x*100-99):(x*100),])$durations,
#                    osrmTable(src = stockton_lodes_origin_centroids[101:161,],
#                              dst = stockton_lodes_dest_centroids[(x*100-99):(x*100),])$durations)}))
# od_matrix <- cbind(od_matrix,
#                    rbind(osrmTable(src = stockton_lodes_origin_centroids[1:100,],
#                                    dst = stockton_lodes_dest_centroids[5601:5691,])$durations,
#                    osrmTable(src = stockton_lodes_origin_centroids[101:161,],
#                              dst = stockton_lodes_dest_centroids[5601:5691,])$durations))
# 
# stockton_lodes_h$duration <- lapply(1:nrow(stockton_lodes_h),function(row){
#   od_matrix[which(stockton_lodes_origin_centroids$GEOID %in% stockton_lodes_h[row,"h_bg"]), which(stockton_lodes_dest_centroids$GEOID %in% stockton_lodes_h[row,"w_bg"])]

prep <- do.call(rbind,lapply(1:nrow(stockton_lodes_h),function(row){
  osrmRoute(src = stockton_lodes_origin_centroids[which(stockton_lodes_origin_centroids$GEOID %in% stockton_lodes_h[row,"h_bg"]),],
            dst = stockton_lodes_dest_centroids[which(stockton_lodes_dest_centroids$GEOID %in% stockton_lodes_h[row,"w_bg"]),], overview = FALSE)
}))


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
