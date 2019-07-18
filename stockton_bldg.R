library(readr)
library(dplyr)
library(sf)
library(tigris)
library(mapview)
library(lwgeom)
library(magrittr)

# library(tidycensus)
# library(censusapi)
# library(ggplot2)
# library(lehdr)
# library(osrm)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
# census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab", overwrite = TRUE)

sjc_bldg <- read_csv("C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/USBuildingFootprints/ca_06077_footprints.csv") %>% st_as_sf(wkt = "WKT") %>% st_set_crs(4326) %>% mutate(id = row_number())

# stockton_boundary <- places("CA", cb = TRUE) %>% filter(NAME == "Stockton") %>% st_transform(st_crs(4326))
# stockton_bgs <- sjc_bgs[stockton_boundary,] %>% filter(!(GEOID %in% c("060770040011","060770039001","060770041061","060770039001","060770039002","060770051311","060770051351","060770041022","060770035001",)))
# stockton_bgs <- sjc_bgs[which(sjc_bgs$GEOID %in% st_centroid(sjc_bgs)[stockton_boundary,]$GEOID),]
# stockton_bldg <- sjc_bldg[which(sjc_bldg$id %in% st_centroid(sjc_bldg)[stockton_bgs,]$id | sjc_bldg$id %in% st_centroid(sjc_bldg)[stockton_boundary,]$id),]

stockton_boundary_influence <- st_read("C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/SpheresOfInfluence/SpheresOfInfluence.shp") %>% filter(SPHERE == "STOCKTON") %>% st_transform(st_crs(4326))
stockton_bldg <- sjc_bldg[which(sjc_bldg$id %in% st_centroid(sjc_bldg)[stockton_boundary_influence,]$id),]

sjc_parcels <- st_read("C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/Parcels/Parcels.shp") %>% st_transform(st_crs(4326))
# sjc_parcels_valid <- sjc_parcels %>% mutate(valid = st_is_valid(.))
sjc_parcels_valid <- st_make_valid(sjc_parcels)
stockton_parcels <- sjc_parcels_valid[stockton_boundary_influence,]
bldg_parcel_join <- st_join(st_centroid(stockton_bldg), stockton_parcels) %>% select(APN, id, STAREA__) %>% rename(area = STAREA__) %>% st_set_geometry(NULL)

sjc_zoning <- st_read("C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/Zoning/Zoning.shp") %>% st_transform(st_crs(4326)) %>% filter(ZNLABEL != "STOCKTON")
stockton_zoning <- st_read("C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/Stockton_Zoning/Zoning.shp") %>% st_transform(st_crs(4326))
bldg_zoning_join <- st_join(st_centroid(stockton_bldg), stockton_zoning) %>% select(ZONE, id) %>% st_set_geometry(NULL)
bldg_zoning_join_uninc <- st_join(st_centroid(stockton_bldg), sjc_zoning) %>% select(ZNCODE, id) %>% st_set_geometry(NULL)
bldg_zoning_join %<>% merge(bldg_zoning_join_uninc) %>% mutate(ZONE = ifelse(is.na(ZONE),as.character(ZNCODE),as.character(ZONE))) %>% select(ZONE, id)

sjc_bgs <- block_groups("California", "San Joaquin County", cb = TRUE) %>% st_transform(st_crs(4326))
stockton_bgs <- sjc_bgs[stockton_boundary_influence,]
bldg_bg_join <- st_join(st_centroid(stockton_bldg), stockton_bgs) %>% select(GEOID, id) %>% st_set_geometry(NULL)

#this takes a long time, so you can load sjc_assessor.Rdata instead
tax_col_specs <- read_csv("S:/Restricted Data Library/CoreLogic/Stanford_University_TAX_06_CALIFORNIA/tax_cols.csv")
col_specs_list <- as.list(tax_col_specs$col_spec)
names(col_specs_list) <- tax_col_specs$Field
f <- function(x, pos) filter(x, `FIPS CODE` %in% c("06077"))
sjc_assessor <- read_delim_chunked( "S:/Restricted Data Library/CoreLogic/Stanford_University_Tax_06_CALIFORNIA/Stanford_University_Tax_06_CALIFORNIA.TXT", col_types = do.call(cols, col_specs_list), DataFrameCallback$new(f), chunk_size = 1000000, delim = "|")

save(sjc_assessor, file = "C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/sjc_assessor.Rdata")
load("C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/sjc_assessor.Rdata")

sjc_assessor <- sjc_assessor %>% mutate(APN = as.numeric(`UNFORMATTED APN`))

stockton_bldg_final <- stockton_bldg %>% left_join(bldg_parcel_join, by="id") %>% left_join(bldg_zoning_join, by="id") %>% left_join(bldg_bg_join, by="id") %>% left_join(sjc_assessor, by="APN")

save(stockton_bldg_final, file = "C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/stockton_bldg.Rdata")
load("C:/Users/Derek Ouyang/Google Drive/City Systems/Stockton Green Economy/stockton_bldg.Rdata")  

