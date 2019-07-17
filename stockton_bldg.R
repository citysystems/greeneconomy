library(readr)
library(dplyr)
library(sf)
library(tigris)
library(mapview)

# library(tidycensus)
# library(censusapi)
# library(ggplot2)
# library(lehdr)
# library(osrm)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
# census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab", overwrite = TRUE)

sjc_bldg <- read_csv("C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/USBuildingFootprints/ca_06077_footprints.csv") %>% st_as_sf(wkt = "WKT") %>% st_set_crs(4326) %>% mutate(id = row_number())

sjc_bgs <- block_groups("California", "San Joaquin County", cb = TRUE) %>% st_transform(st_crs(4326))
# stockton_boundary <- places("CA", cb = TRUE) %>% filter(NAME == "Stockton") %>% st_transform(st_crs(4326))
# stockton_bgs <- sjc_bgs[stockton_boundary,] %>% filter(!(GEOID %in% c("060770040011","060770039001","060770041061","060770039001","060770039002","060770051311","060770051351","060770041022","060770035001",)))
# stockton_bgs <- sjc_bgs[which(sjc_bgs$GEOID %in% st_centroid(sjc_bgs)[stockton_boundary,]$GEOID),]
# stockton_bldg <- sjc_bldg[which(sjc_bldg$id %in% st_centroid(sjc_bldg)[stockton_bgs,]$id | sjc_bldg$id %in% st_centroid(sjc_bldg)[stockton_boundary,]$id),]

stockton_boundary_influence <- st_read("C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/SpheresOfInfluence/SpheresOfInfluence.shp") %>% filter(SPHERE == "STOCKTON") %>% st_transform(st_crs(4326))
stockton_bldg <- sjc_bldg[which(sjc_bldg$id %in% st_centroid(sjc_bldg)[stockton_boundary_influence,]$id),]

sjc_parcels <- st_read("C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/Parcels/Parcels.shp") %>% st_transform(st_crs(4326))
bldg_parcel_join <- st_join(st_centroid(stockton_bldg), sjc_parcels) %>% select(APN, id) %>% st_set_geometry(NULL)

save.image("C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/A.Rdata")
