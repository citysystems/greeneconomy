library(tidycensus)
library(censusapi)
library(tigris)
library(units)
library(lehdr)
library(sf)
library(osrm)
library(mapview)
library(tidyverse)
library(magrittr)
library(lwgeom)
library(data.table)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
options(osrm.server = "http://127.0.0.1:5000/") #This is using Max's new local instance of OSRM setup. See https://github.com/maxo16/osrm_stuff/blob/master/Install%20and%20Run%20Notes.md
census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab", overwrite = TRUE) #this sets up the API for tidycensus
Sys.setenv(CENSUS_KEY="c8aa67e4086b4b5ce3a8717f59faa9a28f611dab") #this sets up the API for censusapi. the two are useful for slightly different things.



# acs5_17 <- load_variables(2017, "acs5") #this is only useful if you want to search for specific ACS variables, but i tend to find it easier to use factfinder online.



#next few steps are for PG&E analysis. first getting zip code tabulation areas, treating those as roughly the sam as zip codes, and then using them to pair PG&E zip code energy data with zip code level building summaries.

zcta <- zctas(starts_with="95") %>% 
  mutate(ZCTA5CE10 = as.numeric(ZCTA5CE10))
# zips_stockton <- st_read("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/ZipCodes/ZipCodes.shp") %>% st_transform(st_crs(zcta)) #this was downloaded from Stockton GIS site to do a quick visual check of the difference between ZCTA and zip code. you can read up on it if you want. i determined they were pretty much the same. you don't need to use zips_stockton for anything.

stockton_boundary <- places("CA") %>% 
  filter(NAME == "Stockton")

stockton_boundary_influence <- st_read("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/SpheresOfInfluence/SpheresOfInfluence.shp") %>% 
  filter(SPHERE == "STOCKTON") %>% 
  st_transform(st_crs(zcta)) #stockton boundary is legit but really spotty, so i prefer using sphere of influence from the County GIS page.

#2 of the 3 shapes are tiny little triangles to get rid of.
stockton_boundary_influence <- stockton_boundary_influence[1,] 

zcta_stockton <- zcta[stockton_boundary_influence,] %>% 
  filter(!ZCTA5CE10 %in% c("95242","95240","95336","95330")) %>% 
  select(ZCTA5CE10) %>% 
  rename(ZIPCODE = ZCTA5CE10) %>% 
  mutate(ZIPCODE = as.numeric(ZIPCODE))
  #these are some extraneous zip codes that just barely touch the sphere of influence that i manually decided to remove.
# zcta_stockton <- zcta[which(zcta$ZCTA5CE10 %in% st_centroid(zcta)[stockton_boundary_influence,]$ZCTA5CE10),] #this would be the go-to script to do a location by centroid within, but it's not as useful in this specific case.



#the following pulls all the downloaded PG&E zip code datasets from S drive and does various processing at the zip code level. You can skip to load() zcta_stockton_joined.Rdata. some notes:

#Manual edits made to PG&E data downloaded from public site:
#2014_Q3_Gas, fields "Total Therms" and "Average Therms" renamed
#2017_Q4_Electricity and 2017_Q4_Gas, remove duplicate m=9 values

#Elec- Industrial mostly 0's, 95206 suddenly shows up with ~70 customers in 2014 Q3/Q4, 2015 Q1, 2016 M2/M3, 2018 M6/M9

pge_elec_emissions_factor <- data.frame(year = 2013:2018, factor = c(427,434.92,404.51,293.67,210,210)) #these emissions factors come from ICLEI: https://docs.google.com/spreadsheets/d/1y3WfMLRzeINdGEtOVI0S2jgXVkJHWpID8vxT86JwdGM/edit#gid=0. read more about them at https://www.ca-ilg.org/sites/main/files/file-attachments/ghg_emission_factor_guidance.pdf 

pge_stockton <- do.call(rbind,lapply(2013:2018,function(year){
  
  factor <- pge_elec_emissions_factor[match(year,pge_elec_emissions_factor$year),2]
  
  df_year <- do.call(rbind,lapply(1:4,function(quarter){
    
    df_quarter <- do.call(rbind,lapply(c("Electric","Gas"),function(type){
      
      filename <- paste("S:/Data Library/PG&E/PGE_",year,"_Q",quarter,"_",type,"UsageByZip.csv",sep = "")
      
      df_type <- read_csv(filename) %>% 
        rename_all(toupper) %>% 
        filter(ZIPCODE %in% zcta_stockton$ZIPCODE) %>% 
        group_by(ZIPCODE, CUSTOMERCLASS) %>% 
        summarize(TOTALKBTU = ifelse(type == "Electric",sum(TOTALKWH)*3.4121416331,sum(TOTALTHM)*99.9761), 
                  TOTALMTCO2 = ifelse(type == "Electric",sum(TOTALKWH)*factor/1000*0.000453592,sum(TOTALTHM)*0.00531), 
                  TOTALCUSTOMERS = sum(TOTALCUSTOMERS)) %>%  
        dplyr::select(ZIPCODE, CUSTOMERCLASS, TOTALKBTU, TOTALMTCO2, TOTALCUSTOMERS) # note the numbers here are mostly unit conversions. 
      
    }))
    
  })) %>% 
    group_by(ZIPCODE, CUSTOMERCLASS) %>% 
    summarize(TOTALKBTU = sum(TOTALKBTU), 
              TOTALMTCO2 = sum(TOTALMTCO2), 
              TOTALCUSTOMERS = sum(TOTALCUSTOMERS)) %>%  
    mutate(YEAR = year, 
           KBTUPERCUST = TOTALKBTU/TOTALCUSTOMERS, 
           MTCO2PERCUST = TOTALMTCO2/TOTALCUSTOMERS,
           CUSTOMERCLASS = ifelse(CUSTOMERCLASS == "Elec- Residential","ER",ifelse(CUSTOMERCLASS == "Gas- Residential","GR",ifelse(CUSTOMERCLASS == "Elec- Commercial","EC",ifelse(CUSTOMERCLASS == "Elec- Industrial","EI",ifelse(CUSTOMERCLASS == "Elec- Agricultural","EA","GC"))))))
  
}))



#the next two lines are just to create a summary graph showing electricity vs. gas over the years. this was an earlier analysis. 
summary_mtco2_average <- pge_stockton %>% 
  filter(!(CUSTOMERCLASS %in% c("Elec- Industrial","Elec- Agricultural"))) %>% 
  mutate(ENERGYTYPE = substr(CUSTOMERCLASS,1,1)) %>% 
  group_by(ENERGYTYPE, YEAR) %>% 
  summarize(annual_average = sum(TOTALMTCO2)/sum(TOTALCUSTOMERS)*12)

ggplot(summary_mtco2_average, aes(as.factor(YEAR), annual_average)) + 
  geom_bar(stat = "identity", aes(fill = ENERGYTYPE), position = "dodge") + 
  labs(x = "Year", y = "MTCO2e/customer", title = "PG&E Territory Annual Energy Usage, 2013 to 2018") + 
  scale_fill_discrete(name="Energy Type",labels = c("Electricity","Gas"))



#here's the newer tidyverse work to better disaggregate energy type (gas vs. electric), class (residential vs. commercial), and output (kbtu vs. mtco2), in every potential meaningful combination. the use of gather/unite/spread is to turn individual outputs of kbtu, mtco2, kbtu/cust, mtco2/cust for every combination of type and class into separate "wide" columns which helps for easy leaflet mapping later on. however i don't have lots of familiarity with best practice here so if the next few steps can be achieved much more elegantly, please make changes!

#note at this point i've been mostly removing industrial because it's "compromised" by having a lot of missing data because of privacy rules, and agricultural because it's a more negligible amount unrelated to our work. if we were to get better PG&E industrial data, then we'd want to include it as a meaningful category in the subsequent steps.
#only viewing one year at a time, in this case 2016. there could have been even further disaggregation by year but then there'd be hundreds of columns.
pge_stockton_filtered <- pge_stockton %>% 
  filter(!CUSTOMERCLASS %in% c("EI","EA") & (YEAR == 2016)) %>%
  select(-YEAR)

zips_spread <- pge_stockton_filtered %>% 
  gather(key = "type", value, TOTALKBTU:MTCO2PERCUST) %>% 
  unite(temp,CUSTOMERCLASS,type) %>% 
  spread(temp,value) 

#next, since the previous line fully disaggregates by type AND class, but i also consider it valuable to disaggregate by type OR class individually on their own, i do a second and third spread. all of these "spreads" will be joined back to an original in the final step.
zips_spread_2 <- pge_stockton_filtered %>% 
  mutate(ENERGYTYPE = substr(CUSTOMERCLASS,1,1)) %>% 
  select(-CUSTOMERCLASS) %>% 
  group_by(ZIPCODE, ENERGYTYPE) %>% 
  summarise(TOTALKBTU = sum(TOTALKBTU),
            TOTALMTCO2 = sum(TOTALMTCO2),
            TOTALCUSTOMERS = sum(TOTALCUSTOMERS)) %>% 
  mutate(KBTUPERCUST = TOTALKBTU/TOTALCUSTOMERS,
         MTCO2PERCUST = TOTALMTCO2/TOTALCUSTOMERS) %>% 
  gather(key = "type", value, TOTALKBTU:MTCO2PERCUST) %>% 
  unite(temp,ENERGYTYPE,type) %>% 
  spread(temp,value)

#note that when combining into SECTOR, say electricity and gas for commercial, the summary function for TOTALCUSTOMERS switches to max(), with the reasoning that many of these commercial customers have both electricity and gas, so we would assume that the correct total # of customers is closer to the max of either, than to add them together (which presumes that there are no overlapping electricity and gas customers).
zips_spread_3 <- pge_stockton_filtered %>%
  mutate(SECTOR = substr(CUSTOMERCLASS,2,2)) %>% 
  select(-CUSTOMERCLASS) %>% 
  group_by(ZIPCODE, SECTOR) %>% 
  summarise(TOTALKBTU = sum(TOTALKBTU),
            TOTALMTCO2 = sum(TOTALMTCO2),
            TOTALCUSTOMERS = max(TOTALCUSTOMERS)) %>% 
  mutate(KBTUPERCUST = TOTALKBTU/TOTALCUSTOMERS,
         MTCO2PERCUST = TOTALMTCO2/TOTALCUSTOMERS) %>% 
  gather(key = "type", value, TOTALKBTU:MTCO2PERCUST) %>% 
  unite(temp,SECTOR,type) %>% 
  spread(temp,value)

#the final step here is to get the actual total totals for kbtu and mtco2 indicators, and then join all the "subtotals" from the previous 3 spreads.
zips_mtco2_total <- pge_stockton_filtered %>% 
  group_by(ZIPCODE) %>% 
  summarize(TOTALKBTU = sum(TOTALKBTU), 
            TOTALMTCO2 = sum(TOTALMTCO2)) %>% 
  left_join(zips_spread_3, by = "ZIPCODE") %>% 
  mutate(TOTALCUSTOMERS = R_TOTALCUSTOMERS + C_TOTALCUSTOMERS) %>% 
  left_join(zips_spread_2, by = "ZIPCODE") %>% 
  left_join(zips_spread, by = "ZIPCODE")

#join this massive summary back to the zcta geometries
zcta_stockton_joined <- zcta_stockton %>% 
  left_join(zips_mtco2_total, by="ZIPCODE")

save(zcta_stockton_joined, file = "C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/zcta_stockton_joined.Rdata")

load("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/zcta_stockton_joined.Rdata")





#below I've brought in all the code from stockton_bldg.R, but it can all be skipped by going to the load() of stockton_bldg.Rdata at the bottom. Kevin, whatever refinements you've made, go ahead and make them here.

sjc_bldg <- read_csv("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/USBuildingFootprints/ca_06077_footprints.csv") %>% 
  st_as_sf(wkt = "WKT") %>% 
  st_set_crs(4326) %>% 
  mutate(id = row_number())

stockton_boundary_influence %<>% 
  st_transform(st_crs(4326))

stockton_bldg <- sjc_bldg[which(sjc_bldg$id %in% st_centroid(sjc_bldg)[stockton_boundary_influence,]$id),]

sjc_parcels <- st_read("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/Parcels/Parcels.shp") %>% 
  st_transform(st_crs(4326))

sjc_parcels_valid <- st_make_valid(sjc_parcels)

stockton_parcels <- sjc_parcels_valid[stockton_boundary_influence,]

bldg_parcel_join <- st_join(st_centroid(stockton_bldg), stockton_parcels) %>% 
  dplyr::select(APN, id, STAREA__) %>% 
  rename(area = STAREA__) %>% 
  st_set_geometry(NULL)

sjc_zoning <- st_read("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/Zoning/Zoning.shp") %>% st_transform(st_crs(4326)) %>% 
  filter(ZNLABEL != "STOCKTON")

stockton_zoning <- st_read("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/Stockton_Zoning/Zoning.shp") %>% 
  st_transform(st_crs(4326))

bldg_zoning_join <- st_join(st_centroid(stockton_bldg), stockton_zoning) %>% 
  dplyr::select(ZONE, id) %>% 
  st_set_geometry(NULL)

bldg_zoning_join_uninc <- st_join(st_centroid(stockton_bldg), sjc_zoning) %>% 
  dplyr::select(ZNCODE, id) %>% 
  st_set_geometry(NULL)

bldg_zoning_join %<>% merge(bldg_zoning_join_uninc) %>% 
  mutate(ZONE = ifelse(is.na(ZONE),as.character(ZNCODE),as.character(ZONE))) %>% 
  dplyr::select(ZONE, id)

sjc_bgs <- block_groups("California", "San Joaquin County") %>% 
  st_transform(st_crs(4326))

stockton_bgs <- sjc_bgs[stockton_boundary_influence,]

bldg_bg_join <- st_join(st_centroid(stockton_bldg), stockton_bgs) %>% 
  dplyr::select(GEOID, id) %>% 
  st_set_geometry(NULL)

zcta_stockton %<>% st_transform(st_crs(4326))

bldg_zcta_join <- st_join(st_centroid(stockton_bldg), zcta_stockton) %>% 
  dplyr::select(ZIPCODE, id) %>% 
  st_set_geometry(NULL)

zcta_stockton %<>% st_transform(st_crs(zcta))

# the reading of assessor data takes a long time, so you can load sjc_assessor.Rdata instead
tax_col_specs <- read_csv("S:/Restricted Data Library/CoreLogic/Stanford_University_TAX_06_CALIFORNIA/tax_cols.csv")

col_specs_list <- as.list(tax_col_specs$col_spec)

names(col_specs_list) <- tax_col_specs$Field

f <- function(x, pos) filter(x, `FIPS CODE` %in% c("06077"))

sjc_assessor <- read_delim_chunked( "S:/Restricted Data Library/CoreLogic/Stanford_University_Tax_06_CALIFORNIA/Stanford_University_Tax_06_CALIFORNIA.TXT", col_types = do.call(cols, col_specs_list), DataFrameCallback$new(f), chunk_size = 1000000, delim = "|")

# save(sjc_assessor, file = "C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/sjc_assessor.Rdata")

load("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/sjc_assessor.Rdata")

sjc_assessor <- sjc_assessor %>% 
  mutate(APN = as.numeric(`UNFORMATTED APN`))

stockton_bldg_final <- stockton_bldg %>% 
  left_join(bldg_parcel_join, by="id") %>% 
  left_join(bldg_zoning_join, by="id") %>% 
  left_join(bldg_bg_join, by="id") %>% 
  left_join(bldg_zcta_join, by="id") %>% 
  left_join(sjc_assessor, by="APN") %>% 
  st_transform(st_crs(zcta))

stockton_boundary_influence %<>% st_transform(st_crs(zcta))

save(stockton_bldg_final, file = "C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/stockton_bldg.Rdata")

load("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/stockton_bldg.Rdata")  



  

# the following takes the bldg data, computes bldgground (ground footprint, where the factor conversion is sqm to sqft) and bldgtot (total sqft). O is "Other" zoning, though we may want to disaggregate more.
zcta_bldg_summary <- stockton_bldg_final %>% 
  mutate(bldgground = as.numeric(st_area(WKT))*10.7639,
         bldgtot = ifelse(is.na(`STORIES NUMBER`), bldgground, bldgground * `STORIES NUMBER`), 
         ZIPCODE = as.numeric(ZIPCODE), 
         ZONING = ifelse(substr(ZONE,1,1)=="R","R", ifelse(substr(ZONE,1,1)=="C","C","O"))) %>% 
  st_set_geometry(NULL)

#disaggregating by zipcode and zoning type of the buildings to get summaries of # of buildings, total ground footprint, and total bldg sqft. the filter removes NAs in the data, which could be missing zones in specific zipcodes.
zcta_bldg_spread <- zcta_bldg_summary %>% 
  group_by(ZIPCODE, ZONING) %>% 
  summarise(NUMBLDG = n(), 
            TOTSQFTGROUND = sum(bldgground), 
            TOTSQFT = sum(bldgtot)) %>% 
  filter(!is.na(ZIPCODE) & !is.na(ZONING)) %>% 
  gather(key, value, NUMBLDG:TOTSQFT) %>% 
  unite(temp,ZONING,key) %>% 
  spread(temp,value)

#summarize totals by zipcode (without breaking down to zoning type), then join the zoning-specific subtotals to it.
zcta_bldg_summary %<>% group_by(ZIPCODE) %>% 
  summarise(NUMBLDG = n(), 
            TOTSQFTGROUND = sum(bldgground), 
            TOTSQFT = sum(bldgtot)) %>% 
  left_join(zcta_bldg_spread, by = "ZIPCODE")

#the following gets quite messy, basically normalizing everything by sqft, where before we normalized by customer. there is probably a more elegant way to do this which requires getting the sqft data in much earlier, basically right at the beginning, and then creating sqft-normalized values BEFORE doing the first spreads earlier in the script. One would just need to be careful about how R_TOTSQFT and C_TOTSQFT are used in the right cases. Any attempt to better organize this would be appreciated.
zcta_bldg_stockton_joined <- zcta_stockton_joined %>% 
  filter(ZIPCODE != 95211) %>% 
  left_join(zcta_bldg_summary, by = "ZIPCODE") %>% 
  mutate(ER_KBTUperSQFT = ER_TOTALKBTU/R_TOTSQFT, 
         GR_KBTUperSQFT = GR_TOTALKBTU/R_TOTSQFT, 
         EC_KBTUperSQFT = EC_TOTALKBTU/C_TOTSQFT, 
         GC_KBTUperSQFT = GC_TOTALKBTU/C_TOTSQFT, 
         R_KBTUperSQFT = R_TOTALKBTU/R_TOTSQFT, 
         C_KBTUperSQFT = C_TOTALKBTU/C_TOTSQFT, 
         E_KBTUperSQFT = E_TOTALKBTU/TOTSQFT, 
         G_KBTUperSQFT = G_TOTALKBTU/TOTSQFT, 
         KBTUperSQFT = TOTALKBTU/TOTSQFT,
         ER_MTCO2perSQFT = ER_TOTALMTCO2/R_TOTSQFT, 
         GR_MTCO2perSQFT = GR_TOTALMTCO2/R_TOTSQFT, 
         EC_MTCO2perSQFT = EC_TOTALMTCO2/C_TOTSQFT, 
         GC_MTCO2perSQFT = GC_TOTALMTCO2/C_TOTSQFT, 
         R_MTCO2perSQFT = R_TOTALMTCO2/R_TOTSQFT, 
         C_MTCO2perSQFT = C_TOTALMTCO2/C_TOTSQFT, 
         E_MTCO2perSQFT = E_TOTALMTCO2/TOTSQFT, 
         G_MTCO2perSQFT = G_TOTALMTCO2/TOTSQFT, 
         MTCO2perSQFT = TOTALMTCO2/TOTSQFT)

map <- mapview(zcta_bldg_stockton_joined, zcol= c("ER_MTCO2perSQFT", "GR_MTCO2perSQFT", "EC_MTCO2perSQFT", "GC_MTCO2perSQFT", "R_MTCO2perSQFT","C_MTCO2perSQFT","E_MTCO2perSQFT","G_MTCO2perSQFT","MTCO2perSQFT"), map.types = c("OpenStreetMap"), legend = TRUE, hide = TRUE)

mapshot(map, url = "stockton_bldg_energy.html")

#the next is just to get full summaries without zip code aggregation for stockton. there is almost certainly a more efficient way to run this. also note that this is used specifically to create the summary table at https://docs.google.com/spreadsheets/d/1X-cEwK-G53NMp3BqQ9encnjceU0dVsSmopQeeecffqM/edit#gid=152068039, but i manually reorganized the wide output into the matrix on Google Sheets. It'd be great if the direct output of the R script is the table in the right format, since that is the most useful format for visualization in discussion, but i didn't have the time to figure out how to set that up in R. help would be appreciated.
zcta_bldg_stockton_summary <- 
  zcta_bldg_stockton_joined %>% 
  st_set_geometry(NULL) %>% 
  summarise_at(c("NUMBLDG", "R_NUMBLDG", "C_NUMBLDG","TOTSQFTGROUND","TOTSQFT","R_TOTSQFT","C_TOTSQFT","R_TOTSQFTGROUND","C_TOTSQFTGROUND","TOTALKBTU","E_TOTALKBTU","G_TOTALKBTU","R_TOTALKBTU","C_TOTALKBTU", "ER_TOTALKBTU", "GR_TOTALKBTU", "EC_TOTALKBTU", "GC_TOTALKBTU", "TOTALMTCO2", "E_TOTALMTCO2", "G_TOTALMTCO2", "R_TOTALMTCO2", "C_TOTALMTCO2", "ER_TOTALMTCO2", "GR_TOTALMTCO2", "EC_TOTALMTCO2", "GC_TOTALMTCO2"), sum) %>% 
  mutate(ER_KBTUperSQFT = ER_TOTALKBTU/R_TOTSQFT, 
         GR_KBTUperSQFT = GR_TOTALKBTU/R_TOTSQFT, 
         EC_KBTUperSQFT = EC_TOTALKBTU/C_TOTSQFT, 
         GC_KBTUperSQFT = GC_TOTALKBTU/C_TOTSQFT, 
         R_KBTUperSQFT = R_TOTALKBTU/R_TOTSQFT, 
         C_KBTUperSQFT = C_TOTALKBTU/C_TOTSQFT, 
         E_KBTUperSQFT = E_TOTALKBTU/TOTSQFT, 
         G_KBTUperSQFT = G_TOTALKBTU/TOTSQFT, 
         KBTUperSQFT = TOTALKBTU/TOTSQFT,
         ER_MTCO2perSQFT = ER_TOTALMTCO2/R_TOTSQFT, 
         GR_MTCO2perSQFT = GR_TOTALMTCO2/R_TOTSQFT, 
         EC_MTCO2perSQFT = EC_TOTALMTCO2/C_TOTSQFT, 
         GC_MTCO2perSQFT = GC_TOTALMTCO2/C_TOTSQFT, 
         R_MTCO2perSQFT = R_TOTALMTCO2/R_TOTSQFT, 
         C_MTCO2perSQFT = C_TOTALMTCO2/C_TOTSQFT, 
         E_MTCO2perSQFT = E_TOTALMTCO2/TOTSQFT, 
         G_MTCO2perSQFT = G_TOTALMTCO2/TOTSQFT, 
         MTCO2perSQFT = TOTALMTCO2/TOTSQFT)

write.csv(zcta_bldg_stockton_summary, file = "bldg_stockton_summary.csv")

save(zcta_bldg_stockton_joined, zcta_bldg_stockton_summary, file = "C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/zcta_bldg_stockton_joined.Rdata")

load("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/zcta_bldg_stockton_joined.Rdata")

#the following lines are used to create the pop + jobs time series from 2010 to 2016. the jobs data is based on the selection of zctas, which is a larger set than Stockton geography, while the pop data is using the Stockton place geography from ACS, so they aren't an exact geographic comparison, but close enough in my opinion.

# zbp_stockton <- data.frame(matrix(ncol=5,nrow=0))
# colnames(zbp_stockton) <- c("zipcode","EMP","ESTAB","PAYANN","year")
# 
# for(year in 2010:2016){
#   
#   temp <- 
#     getCensus(
#       name = "zbp",
#       vintage = year,
#       region = "zipcode:*",
#       vars = c(
#         "EMP",
#         "ESTAB",
#         "PAYANN"
#       )
#     ) %>% 
#     filter(zipcode %in% zcta_stockton$ZIPCODE) %>% 
#     mutate(year = year)
#   
#   zbp_stockton<- 
#     rbind(zbp_stockton,temp)
#   
# }
# 
# 
# 
# sjc_boundary <- 
#   counties("CA") %>% 
#   filter(NAME == "San Joaquin")
# 
# sjc_bgs %<>%
#   st_transform(st_crs(zcta_stockton))
# 
# #matching county subdivision for zctas
# county_subdivision <- 
#   county_subdivisions("CA", "San Joaquin County") %>% 
#   filter(NAME == "Stockton")
# 
# #matching sample of block groups for zctas
# zcta_bgs <- 
#   sjc_bgs[which(sjc_bgs$GEOID %in% st_centroid(sjc_bgs)[zcta_stockton,]$GEOID),]
# 
# temp <- 
#   getCensus(
#     name = "acs/acs1",
#     vintage = year,
#     vars = c("B01003_001E"),
#     region = "place:75000",
#     regionin = "state:06"
#   )
# 
# 
# pop_stockton <- data.frame(matrix(ncol=2,nrow=0))
# 
# colnames(pop_stockton) <- c("POP","year")
# 
# for(year in 2010:2016){ 
#   
#   temp <- 
#     getCensus(
#       name = "acs/acs1",
#       vintage = year,
#       vars = c("B01003_001E"),
#       region = "place:75000",
#       regionin = "state:06"
#     ) %>% 
#     mutate(
#       POP = B01003_001E, 
#       year = year
#     ) %>% 
#     select(
#       POP,
#       year
#     )
#   
#   pop_stockton<- rbind(pop_stockton,temp)
#   
# }
# 
# pop_jobs_stockton <- zbp_stockton %>% 
#   group_by(year) %>% 
#   summarize(EMP=sum(as.numeric(EMP)),
#             ESTAB=sum(as.numeric(ESTAB)),
#             PAYANN=sum(as.numeric(PAYANN))) %>% 
#   left_join(pop_stockton, by="year")
# 
# ggplot(pop_jobs_stockton, aes(x = year)) + 
#   geom_line(aes(y = POP, colour = "Population")) + 
#   geom_line(aes(y = EMP*3, colour = "Employment")) + 
#   scale_y_continuous(sec.axis = sec_axis(~./3, name = "Employment")) + 
#   scale_colour_manual(values = c("blue","red")) + 
#   labs(title = "Stockton, CA", y = "Population", x = "Year", colour = "Parameter")
# 
# nonemp_sjc <-
#   getCensus(
#     name = "nonemp",
#     vintage = 2010,
#     region = "county:077",
#     regionin = "state:06",
#     vars = c(
#       "NESTAB",
#       "NRCPTOT"
#     )
#   )


#change to county scale for consistency for now

sjc_boundary <- 
  counties("CA") %>% 
  filter(NAME == "San Joaquin")

cbp_sjc <- data.frame(matrix(ncol=6,nrow=0))
colnames(cbp_sjc) <- c("EMP","ESTAB","PAYANN","NESTAB","NRCPTOT","year")

for(year in 2010:2016){
  
  temp <- 
    getCensus(
      name = "cbp",
      vintage = year,
      region = "county:077",
      regionin = "state:06",
      vars = c(
        "EMP",
        "ESTAB",
        "PAYANN"
      )
    ) %>% 
    select(-c(state,county))
  
  nonemp <-
    getCensus(
      name = "nonemp",
      vintage = year,
      region = "county:077",
      regionin = "state:06",
      vars = c(
        "NESTAB",
        "NRCPTOT"
      )
    ) %>%
    mutate(year = year) %>% 
    select(-c(state,county))
  
  cbp_sjc<- 
    rbind(cbp_sjc,cbind(temp,nonemp))
  
}


label_industry <- 
  read_csv("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/label_industry.csv")

qwi_sjc <- data.frame(matrix(ncol=5,nrow=0))
colnames(qwi_sjc) <- c("year","industry","label","EmpS","EarnS")

for(years in 2010:2018){
  qwi<- 
    getCensus(
      name = "timeseries/qwi/sa",
      region = "county:077",
      regionin = "state:06",
      vars = c("EmpS","EarnS","industry","ind_level"),
      time = years
    ) %>% 
    filter(ind_level == 4) %>% 
    mutate(
      year = substr(time,1,4)
    ) %>% 
    left_join(label_industry, by= "industry") %>% 
    group_by(year,industry,label) %>% 
    summarize(
      EmpS = round(mean(as.numeric(EmpS), na.rm = TRUE),0),
      EarnS = round(mean(as.numeric(EarnS), na.rm = TRUE),0)
    ) %>% 
    filter(!is.na(EmpS) & EmpS != 0)
  
  qwi_sjc<- 
    bind_rows(qwi_sjc,qwi)
}

write_csv(qwi_sjc, "C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/qwi_sjc.csv")




pop_sjc <- data.frame(matrix(ncol=2,nrow=0))
colnames(pop_sjc) <- c("POP","year")

for(year in 2010:2017){ 
  
  temp <- 
    getCensus(
      name = "acs/acs1",
      vintage = year,
      vars = c("B01003_001E"),
      region = "county:077",
      regionin = "state:06"
    ) %>% 
    mutate(
      POP = B01003_001E, 
      year = year
    ) %>% 
    select(
      POP,
      year
    )
  
  pop_sjc<- rbind(pop_sjc,temp)
  
}

pop_jobs_sjc <- cbp_sjc %>% 
  group_by(year) %>% 
  summarize(
    EMP=sum(as.numeric(EMP)),
    ESTAB=sum(as.numeric(ESTAB)),
    PAYANN=sum(as.numeric(PAYANN)),
    NESTAB=sum(as.numeric(NESTAB)),
    NRCPTOT=sum(as.numeric(NRCPTOT))
  ) %>% 
  mutate(
    totaljobs = EMP + NESTAB
  ) %>% 
  left_join(pop_sjc, by="year")

ggplot(pop_jobs_sjc, aes(x = year)) + 
  geom_line(aes(y = POP, colour = "Population")) + 
  geom_line(aes(y = totaljobs*3, colour = "Employment")) + 
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Jobs")) + 
  scale_colour_manual(values = c("blue","red")) + 
  labs(title = "San Joaquin County, CA", y = "Population", x = "Year", colour = "Parameter")



#next step: add future year projections

sjc_projection <- 
  read_csv("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/SSP_asrc_STATEfiles/DATA-PROCESSED/SPLITPROJECTIONS/CA.csv") %>% 
  filter(COUNTY == "077") %>% 
  group_by(YEAR) %>% 
  summarize(
    population = sum(SSP2)
  ) %>% 
  filter(YEAR %in% c(2020,2025,2030,2035,2040)) %>% 
  select(POP = population, year = YEAR)

pop_sjc_w_projection <-
  bind_rows(pop_sjc,sjc_projection)

jobs_sjc <- cbp_sjc %>% 
  group_by(year) %>% 
  summarize(
    EMP=sum(as.numeric(EMP)),
    ESTAB=sum(as.numeric(ESTAB)),
    PAYANN=sum(as.numeric(PAYANN)),
    NESTAB=sum(as.numeric(NESTAB)),
    NRCPTOT=sum(as.numeric(NRCPTOT))
  ) %>% 
  mutate(
    totaljobs = EMP + NESTAB
  )


nonemp_sjc <- data.frame(matrix(ncol=3,nrow=0))
colnames(nonemp_sjc) <- c("NESTAB","NRCPTOT","year")

for(year in 2010:2017){
  
  nonemp <-
    getCensus(
      name = "nonemp",
      vintage = year,
      region = "county:077",
      regionin = "state:06",
      vars = c(
        "NESTAB",
        "NRCPTOT"
      )
    ) %>%
    mutate(year = as.numeric(year)) %>% 
    select(-c(state,county))
  
  nonemp_sjc<- 
    rbind(nonemp_sjc,nonemp)
  
}

jobs_sjc <- qwi_sjc %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year) %>% 
  summarize(
    EmpS=sum(as.numeric(EmpS))
  ) %>% 
  left_join(nonemp_sjc, by = "year") %>% 
  mutate(
    totaljobs = EmpS + as.numeric(NESTAB)
  )

pop_jobs_sjc_w_projection <-
  pop_sjc_w_projection %>% 
  left_join(jobs_sjc, by = "year") %>% 
  mutate(
    ratio = POP/EmpS
  )

pop_jobs_sjc_w_projection <-
  pop_sjc_w_projection %>% 
  left_join(jobs_sjc, by = "year") %>% 
  mutate(
    ratio = POP/EmpS,
    EmpS = ifelse(!is.na(EmpS),EmpS,POP/3.5)
  )

ggplot(pop_jobs_sjc_w_projection, aes(x = year)) + 
  geom_line(aes(y = POP, colour = "Population")) + 
  geom_line(aes(y = EmpS*3, colour = "Jobs")) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Jobs")) + 
  scale_colour_manual(values = c("blue","red")) + 
  labs(title = "San Joaquin County, CA", y = "Population", x = "Year", colour = "Parameter")



#add LODES for multiple years

#LODES

ca_lodes <- 
  grab_lodes(
    state = "ca", 
    year = 2017, 
    lodes_type = "od", 
    job_type = "JT01",
    segment = "S000", 
    state_part = "main", 
    agg_geo = "bg"
  )

ca_rac <- 
  grab_lodes(
    state = "ca", 
    year = 2017, 
    lodes_type = "rac", 
    job_type = "JT01",
    segment = "S000", 
    state_part = "main", 
    agg_geo = "bg"
  )

ca_wac <- 
  grab_lodes(
    state = "ca", 
    year = 2017, 
    lodes_type = "wac", 
    job_type = "JT01", 
    segment = "S000", 
    state_part = "main", 
    agg_geo = "bg"
  )

ca_bgs <- block_groups("CA", cb = TRUE)
ca_counties <- counties("CA", cb = TRUE)

# bbox_ca <- st_as_sfc(st_bbox(ca_counties))

#grab OSRM map from 
# https://download.geofabrik.de/north-america/us/california-latest.osm.pbf

stockton_boundary <- 
  places("CA", cb = TRUE) %>% 
  filter(NAME == "Stockton")

stockton_bgs <- 
  ca_bgs[which(ca_bgs$GEOID %in% st_centroid(ca_bgs)[stockton_boundary,]$GEOID),c("GEOID")]

stockton_bgs_full <- 
  ca_bgs[stockton_boundary,c("GEOID")] %>% 
  filter(!(GEOID %in% c("060770040011","060770039001","060770041061","060770039001","060770039002","060770051311","060770051351","060770041022")))

stockton_lodes_w <- 
  ca_lodes[which(ca_lodes$w_bg %in% stockton_bgs_full$GEOID),]

stockton_lodes_h <- 
  ca_lodes[which(ca_lodes$h_bg %in% stockton_bgs_full$GEOID),]

stockton_rac <- 
  stockton_bgs_full %>% geo_join(ca_rac, "GEOID", "h_bg")

stockton_wac <- 
  stockton_bgs_full %>% geo_join(ca_wac, "GEOID", "w_bg")

save(stockton_lodes_w, stockton_lodes_h, stockton_rac, stockton_wac, file = "C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/LODES/stockton_lodes_prep.R")
load("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/LODES/stockton_lodes_prep.R")

stockton_lodes_origin_centroids <- 
  st_centroid(ca_bgs[which(ca_bgs$GEOID %in% stockton_lodes_h$h_bg),])

stockton_lodes_dest_centroids <- 
  st_centroid(ca_bgs[which(ca_bgs$GEOID %in% stockton_lodes_h$w_bg),])

# bbox <- st_as_sfc(st_bbox(c(xmin = -122.870447, xmax = -119.975740, ymax = 38.949398, ymin = 36.642453), crs = st_crs(ca_bgs)))

# stockton_lodes_dest_centroids_subset <-
#   stockton_lodes_dest_centroids[bbox,]

stockton_lodes_dest_bg <- 
  ca_bgs[which(ca_bgs$GEOID %in% stockton_lodes_h$w_bg),]

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

# 1:nrow(stockton_lodes_h)

timer <- proc.time()
prep <- 
  lapply(
    1:nrow(stockton_lodes_h),
    function(row){
      osrmRoute(
        src = stockton_lodes_origin_centroids[which(stockton_lodes_origin_centroids$GEOID %in% stockton_lodes_h[row,"h_bg"]),],
        dst = stockton_lodes_dest_centroids[which(stockton_lodes_dest_centroids$GEOID %in% stockton_lodes_h[row,"w_bg"]),], 
        overview = FALSE
      )
    }
  )
elapsed <- proc.time() - timer
#25 minutes

# test <- osrmRoute(
#   src = stockton_lodes_origin_centroids[which(stockton_lodes_origin_centroids$GEOID %in% stockton_lodes_h[13485,"h_bg"]),],
#   dst = stockton_lodes_dest_centroids[which(stockton_lodes_dest_centroids$GEOID %in% stockton_lodes_h[13485,"w_bg"]),], 
#   overview = "full",
#   returnclass = "sf"
# )

save(prep, file = "C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/LODES/stockton_lodes_osrm.R")
load("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/LODES/stockton_lodes_osrm.R")

# for(row in 1:length(prep)) {
#   if(length(prep[row][[1]])==0)
#     prep[[row]] <- c(duration = NA, distance = NA)
# }

prep <- do.call(rbind,prep)

stockton_lodes_h <- cbind(stockton_lodes_h, prep)

stockton_lodes_h_join_wac <- 
  stockton_lodes_h %>% 
  select(-c(year,state)) %>% 
  left_join(ca_wac, by = "w_bg")

stockton_lodes_h_join_wac_normalize <-
  stockton_lodes_h_join_wac %>% 
  mutate(
    goodsproducing = CNS01+CNS02+CNS04+CNS05,
    tradetransportutil = CNS03+CNS06+CNS07+CNS08,
    services = CNS09+CNS10+CNS11+CNS12+CNS13+CNS14+CNS15+CNS16+CNS17+CNS18+CNS19+CNS20
  ) %>% 
  mutate_at(
    .vars = vars(CNS01,CNS02,CNS04,CNS05),
    .funs = list(~ SI01*./goodsproducing)
  ) %>% 
  mutate_at(
    .vars = vars(CNS03,CNS06,CNS07,CNS08),
    .funs = list(~ SI02*./tradetransportutil)
  ) %>%
  mutate_at(
    .vars = vars(CNS09:CNS20),
    .funs = list(~ SI03*./services)
  ) %>% 
  select(-c(SA01:SA03,C000:CE03,CR01:CFS05),SE01,SE02,SE03) %>% 
  mutate(low=SE01,mid=SE02,high=SE03) %>% 
  gather(key = "type", value, low:high) %>% 
  mutate_at(
    .vars = vars(CNS01:CNS20),
    .funs = list(~ ./S000*value)
  ) %>% 
  gather(key = "type2", value2, CNS01:CNS20) %>% 
  unite(temp,type,type2) %>% 
  spread(temp,value2) %>% 
  filter(value > 0) %>% 
  select(-value)

stockton_lodes_w_counties <-
  stockton_lodes_h_join_wac_normalize %>%
  mutate(
    COUNTY = substr(w_bg,3,5),
    person_miles = S000*as.numeric(distance)/1.60934,
    person_hours = S000*as.numeric(duration)/60
  ) %>%
  group_by(COUNTY) %>%
  summarise_at(
    vars(S000,SE01,SE02,SE03,person_miles,person_hours,high_CNS01:mid_CNS20),
    sum, na.rm=T
  ) %>%
  mutate_at(
    .vars = vars(person_miles:mid_CNS20),
    .funs = list(~ round(.,0))
  ) %>%
  mutate(
    avg_distance = person_miles/S000,
    avg_duration = person_hours/S000,
    person_miles_rm_excessive = ifelse(
      avg_duration < 3,
      person_miles,
      0
    ),
    `Percent Low Wage Jobs in County` = SE01/S000,
    `Percent Low Wage Jobs Overall` = SE01/sum(SE01,na.rm = TRUE),
    `Percent High Wage Jobs in County` = SE03/S000,
    `Percent High Wage Jobs Overall` = SE03/sum(SE03,na.rm = TRUE),
    `Percent Total Jobs` = S000/sum(S000,na.rm = TRUE),
    `Percent VMT` = person_miles_rm_excessive/sum(person_miles_rm_excessive,na.rm = TRUE),
    `GHG Annual` = (person_miles_rm_excessive*0.82*2+person_miles_rm_excessive*.116/2*2)*369.39*0.00035812,
    `Percent GHG` = `GHG Annual`/sum(`GHG Annual`),
    `Average GHG` = `GHG Annual`/S000
  ) %>%
  rename(
    Jobs = S000,
    `Number of jobs with earnings $1250/month or less` = SE01,
    `Number of jobs with earnings $1251/month to $3333/month` = SE02,
    `Number of jobs with earnings greater than $3333/month` = SE03,
    `Average Distance` = avg_distance,
    `Average Duration` = avg_duration)

# stockton_lodes_w_counties <- 
#   stockton_lodes_h %>% 
#   mutate(
#     COUNTY = substr(w_bg,3,5), 
#     person_miles = S000*as.numeric(distance)/1.60934, 
#     person_hours = S000*as.numeric(duration)/60
#   ) %>% 
#   group_by(COUNTY) %>% 
#   summarise_at(
#     c("S000","SA01","SA02","SA03","SE01","SE02","SE03","SI01","SI02","SI03","person_miles", "person_hours"), 
#     sum
#   ) %>% 
#   mutate(
#     avg_distance = person_miles/S000, 
#     avg_duration = person_hours/S000, 
#     person_miles_rm_excessive = ifelse(
#       avg_duration < 3, 
#       person_miles, 
#       0
#     ), 
#     `Percent Low Wage Jobs in County` = SE01/S000, 
#     `Percent Low Wage Jobs Overall` = SE01/sum(SE01,na.rm = TRUE), 
#     `Percent High Wage Jobs in County` = SE03/S000, 
#     `Percent High Wage Jobs Overall` = SE03/sum(SE03,na.rm = TRUE),
#     `Percent Goods Jobs in County` = SI01/S000, 
#     `Percent Goods Jobs Overall` = SI01/sum(SI01,na.rm = TRUE), 
#     `Percent Trade Transport Utility Jobs in County` = SI02/S000, 
#     `Percent Trade Transport Utility Jobs Overall` = SI02/sum(SI02,na.rm = TRUE), 
#     `Percent Service Jobs in County` = SI03/S000, 
#     `Percent Service Jobs Overall` = SI03/sum(SI03,na.rm = TRUE), 
#     `Percent Total Jobs` = S000/sum(S000,na.rm = TRUE), 
#     `Percent VMT` = person_miles_rm_excessive/sum(person_miles_rm_excessive,na.rm = TRUE),
#     `GHG Annual` = (person_miles_rm_excessive*0.82*2+person_miles_rm_excessive*.116/2*2)*369.39*0.00035812, 
#     `Percent GHG` = `GHG Annual`/sum(`GHG Annual`, na.rm = TRUE), 
#     `Average GHG` = `GHG Annual`/S000
#   ) %>% 
#   rename(
#     Jobs = S000,
#     `Number of jobs of workers age 29 or younger` = SA01,
#     `Number of jobs for workers age 30 to 54` = SA02,
#     `Number of jobs for workers age 55 or older` = SA03,
#     `Number of jobs with earnings $1250/month or less` = SE01,
#     `Number of jobs with earnings $1251/month to $3333/month` = SE02,
#     `Number of jobs with earnings greater than $3333/month` = SE03,
#     `Number of jobs in Goods Producing industry sectors` = SI01,
#     `Number of jobs in Trade, Transportation, and Utilities industry sectors` = SI02,
#     `Number of jobs in All Other Services industry sectors` = SI03,
#     `Average Distance` = avg_distance)

stockton_lodes_w_counties_filter <- 
  ca_counties %>% 
  select(COUNTYFP, NAME) %>% 
  left_join(stockton_lodes_w_counties, by = c("COUNTYFP" = "COUNTY")) %>% 
  filter(person_miles_rm_excessive > 0) %>% 
  arrange(desc(Jobs))

stockton_lodes_w_top_counties <-
  stockton_lodes_w_counties_filter[1:15,]

percJobsintopcounties <- sum(stockton_lodes_w_top_counties$Jobs)/
  sum(stockton_lodes_w_counties$Jobs, na.rm=t)

m1 <- mapview(stockton_lodes_w_top_counties, burst = TRUE, map.types = c("OpenStreetMap"), legend = TRUE, hide = TRUE)

# m1 <- mapview(stockton_lodes_w_top_counties, zcol=c("Jobs","Percent Low Wage Jobs in County","Percent High Wage Jobs in County","Average Distance","Average GHG","Percent GHG"), map.types = c("OpenStreetMap"), legend = TRUE, hide = TRUE)

m1

# mapshot(m1, url = "stockton_lodes_w_top_counties.html")
# l1 <- addStaticLabels(m1, label = stockton_lodes_w_top_counties$NAME)

# mapview(stockton_lodes_w_counties, zcol='avg_distance')

write_csv(stockton_lodes_w_counties, "C:\\Users\\derek\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes_w_counties.csv")

save(stockton_lodes_w, stockton_lodes_h, stockton_rac, ca_wac, stockton_wac, stockton_lodes_w_counties, stockton_lodes_w_counties_filter, stockton_lodes_w_top_counties, prep, file = "C:\\Users\\derek\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes.R")
load("C:\\Users\\derek\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes.R")

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
















#the rest is old stuff i'm not using anymore.

qwi_2005 <- getCensus(name = "timeseries/qwi/sa",
            region = "county:077",
            regionin = "state:06",
            vars = c("EmpS","EarnS","industry"),
            time = "2005") 

jobs_2005 <- qwi_2005 %>% mutate(year = substr(time,1,4), Emp = as.numeric(Emp)) %>% group_by(year) %>% summarise(Emp = sum(Emp))

qwi_2016 <- getCensus(name = "timeseries/qwi/sa",
                       region = "county:077",
                       regionin = "state:06",
                       vars = c("Emp"),
                       time = "2016")

jobs_2016 <- qwi_2016 %>% mutate(year = substr(time,1,4), Emp = as.numeric(Emp)) %>% group_by(year) %>% summarise(Emp = sum(Emp))

pop_2005 <- getCensus(name = "acs/acs5",
                      vintage = 2010,
                      vars = c("B01003_001E"),
                      region = "county:077",
                      regionin = "state:06")
#only 2006-2010 working at the moment

pop_2016 <- getCensus(name = "acs/acs1",
                      vintage = 2016,
                      vars = c("B01003_001E"),
                      region = "county:077",
                      regionin = "state:06")


# getCensus(name = "timeseries/qwi/sa",
#           region = "state:06",
#           vars = c("Emp", "sex"),
#           year = 2016,
#           quarter = 1,
#           agegrp = "A07",
#           ownercode = "A05",
#           firmsize = 1,
#           seasonadj = "U",
#           industry = 21)

#look at county business patterns



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
