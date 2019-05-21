install.packages("tidycensus")
install.packages("tigris")
install.packages("tidyverse")
#lehdr 

library(tidycensus)
library(tidyverse)
library(readr)
library(stringr)
library(dplyr)
library(tigris)
library(raster)
library(sf)
library(leaflet)
library(mapview)
library(maptools)
library(rgeos)
library(data.table)
library(ggplot2)
library(rgdal)
options(tigris_use_cache = TRUE)
census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab")

# https://github.com/walkerke/tigris-zip-income/blob/master/prep.R
# https://journal.r-project.org/archive/2016/RJ-2016-043/RJ-2016-043.pdf



zips <- zctas(starts_with="95", cb = TRUE)
# places <- places("CA", cb = TRUE)
# places_stockton <- places[places@data$NAME == 'Stockton',]

stockton_boundary <- readOGR(dsn = "C:\\Users\\derek\\Google Drive\\City Systems\\Stockton Green Economy\\StocktonBoundary", layer = "Boundary")
stockton_boundary <- spTransform(stockton_boundary,proj4string(zips))
# stockton_parcels_dissolve <- unionSpatialPolygons(gBuffer(stockton_parcels, byid=TRUE, width=50),stockton_parcels@data$Shape_STAr)

zips_stockton <- zips[stockton_boundary,]
zips_stockton@data$proportion <- area(intersect(zips_stockton,stockton_boundary))/area(zips_stockton)
zips_stockton@data$ZCTA5CE10 <- as.numeric(zips_stockton@data$ZCTA5CE10)

#Manual edits made to PG&E data downloaded from public site:
#2014_Q3_Gas, fields "Total Therms" and "Average Therms" renamed
#2017_Q4_Electricity and 2017_Q4_Gas, remove duplicate m=9 values

#Elec- Industrial mostly 0's, 95206 suddenly shows up with ~70 customers in 2014 Q3/Q4, 2015 Q1, 2016 M2/M3, 2018 M6/M9

pge_elec_emissions_factor <- data.frame(year = 2013:2018, factor = c(427,434.92,404.51,293.67,210,210))

pge_stockton <- do.call(rbind,lapply(2013:2018,function(year){
  factor <- pge_elec_emissions_factor[match(year,pge_elec_emissions_factor$year),2]
  df_year <- do.call(rbind,lapply(1:4,function(quarter){
    df_quarter <- do.call(rbind,lapply(c("Electric","Gas"),function(type){
      filename <- paste("S:\\Data Library\\PG&E\\PGE_",year,"_Q",quarter,"_",type,"UsageByZip.csv",sep = "")
      df_type <- read_csv(filename) %>% rename_all(toupper) %>% filter(ZIPCODE %in% zips_stockton$ZCTA5CE10) %>% mutate(ZIPCODE = as.numeric(ZIPCODE)) %>% group_by(ZIPCODE, CUSTOMERCLASS) %>% summarize(TOTALKBTU = ifelse(type == "Electric",sum(TOTALKWH)*3.4121416331,sum(TOTALTHM)*99.9761), TOTALMTCO2 = ifelse(type == "Electric",sum(TOTALKWH)*factor/1000*0.000453592,sum(TOTALTHM)*0.00531), TOTALCUSTOMERS = sum(TOTALCUSTOMERS)) %>%  dplyr::select(ZIPCODE, CUSTOMERCLASS, TOTALKBTU, TOTALMTCO2, TOTALCUSTOMERS)
    }))
  })) %>% group_by(ZIPCODE, CUSTOMERCLASS) %>% summarize(TOTALKBTU = sum(TOTALKBTU), TOTALMTCO2 = sum(TOTALMTCO2), TOTALCUSTOMERS = sum(TOTALCUSTOMERS)) %>%  mutate(YEAR = year)
}))

pge_stockton_proportioned <- pge_stockton %>% left_join(zips_stockton@data[,c("ZCTA5CE10","proportion")],by = c("ZIPCODE" = "ZCTA5CE10")) %>% mutate(TOTALKBTU = TOTALKBTU*proportion, TOTALMTCO2 = TOTALMTCO2*proportion, TOTALCUSTOMERS = TOTALCUSTOMERS*proportion)





#The following segment is used to find data errors
# test_df <- data.frame(matrix(ncol=4,nrow=0))
# x <- c("year","quarter","type","col")
# colnames(test_df) <- x
# for(year in 2013:2018){
#   for(quarter in 1:4){
#     for(type in c("Electric","Gas")){
#       filename <- paste("S:\\Data Library\\PG&E\\PGE_",year,"_Q",quarter,"_",type,"UsageByZip.csv",sep = "")
#       test <- read_csv(filename) %>% rename_all(toupper) %>% filter(CUSTOMERCLASS == "Gas- Residential") %>% filter(ZIPCODE %in% zips_stockton$ZCTA5CE10)
#       View(test)
#       test_df <- rbind(test_df,data.frame(year = year, quarter = quarter, type = type, count = nrow(test)))
#     }
#   }
# }


  
summary_kbtu_byclass <- pge_stockton_proportioned %>% filter(!(CUSTOMERCLASS %in% c("Elec- Industrial","Elec- Agricultural"))) %>% group_by(CUSTOMERCLASS, YEAR) %>% summarize(annual_total = sum(TOTALKBTU))
ggplot() + geom_area(aes(x = YEAR, y = annual_total, fill = CUSTOMERCLASS), data = summary_kbtu_byclass) + labs(x = "Year", y = "kBTU", title = "PG&E Territory Annual Energy Usage, 2013 to 2018", fill = "Energy Type")

summary_mtco2_byclass <- pge_stockton_proportioned %>% filter(!(CUSTOMERCLASS %in% c("Elec- Industrial","Elec- Agricultural"))) %>% group_by(CUSTOMERCLASS, YEAR) %>% summarize(annual_total = sum(TOTALMTCO2)) 
ggplot() + geom_area(aes(x = YEAR, y = annual_total, fill = CUSTOMERCLASS), data = summary_mtco2_byclass) + labs(x = "Year", y = "MTCO2e", title = "PG&E Territory Annual Energy Usage, 2013 to 2018", fill = "Energy Type")


summary_kbtu_total <- pge_stockton_proportioned %>% filter(!(CUSTOMERCLASS %in% c("Elec- Industrial","Elec- Agricultural"))) %>% group_by(YEAR) %>% summarize(annual_total = sum(TOTALKBTU))
ggplot(summary_kbtu_total, aes(as.factor(YEAR), annual_total)) + geom_bar(stat = "identity", position = "dodge") + labs(x = "Year", y = "kBTU", title = "PG&E Territory Annual Energy Usage, 2013 to 2018")

summary_mtco2_total <- pge_stockton_proportioned %>% filter(!(CUSTOMERCLASS %in% c("Elec- Industrial","Elec- Agricultural"))) %>% group_by(YEAR) %>% summarize(annual_total = sum(TOTALMTCO2))
ggplot(summary_mtco2_total, aes(as.factor(YEAR), annual_total)) + geom_bar(stat = "identity", position = "dodge") + labs(x = "Year", y = "MTCO2e", title = "PG&E Territory Annual Energy Usage, 2013 to 2018")


summary_kbtu_average <- pge_stockton_proportioned %>% filter(!(CUSTOMERCLASS %in% c("Elec- Industrial","Elec- Agricultural"))) %>% mutate(type = substr(CUSTOMERCLASS,1,1)) %>% group_by(type, YEAR) %>% summarize(annual_average = sum(TOTALKBTU)/sum(TOTALCUSTOMERS)*12)
ggplot(summary_kbtu_average, aes(as.factor(YEAR), annual_average)) + geom_bar(stat = "identity", aes(fill = type), position = "dodge") + labs(x = "Year", y = "kBTU/customer", title = "PG&E Territory Annual Energy Usage, 2013 to 2018") + scale_fill_discrete(name="Energy Type",labels = c("Electricity","Gas"))

summary_mtco2_average <- pge_stockton_proportioned %>% filter(!(CUSTOMERCLASS %in% c("Elec- Industrial","Elec- Agricultural"))) %>% mutate(type = substr(CUSTOMERCLASS,1,1)) %>% group_by(type, YEAR) %>% summarize(annual_average = sum(TOTALMTCO2)/sum(TOTALCUSTOMERS)*12)
ggplot(summary_mtco2_average, aes(as.factor(YEAR), annual_average)) + geom_bar(stat = "identity", aes(fill = type), position = "dodge") + labs(x = "Year", y = "MTCO2e/customer", title = "PG&E Territory Annual Energy Usage, 2013 to 2018") + scale_fill_discrete(name="Energy Type",labels = c("Electricity","Gas"))


zips_mtco2_total <- pge_stockton %>% filter((CUSTOMERCLASS %in% c("Gas- Residential")) & (YEAR == 2016)) %>% group_by(ZIPCODE) %>% summarize(TOTALMTCO2 = sum(TOTALMTCO2))

zips_stockton_joined_intersect <- geo_join(intersect(zips_stockton,stockton_boundary), zips_mtco2_total, "ZCTA5CE10","ZIPCODE")
zips_stockton_joined <- geo_join(zips_stockton, zips_mtco2_total, "ZCTA5CE10","ZIPCODE")

mapview(stockton_boundary, alpha.regions = 0, lwd = 3) +
  mapview(zips_stockton_joined, zcol = "TOTALMTCO2", legend = TRUE)

sum(zips_mtco2_total$TOTALMTCO2)
  



# v17 <- load_variables(2017, "acs5")
# write.csv(v17, file = "v17_acs5.csv")

pop_zcta <- get_acs(geography = "zcta", variables = "B01003_001", geometry = TRUE)
#cb = FALSE for TIGER shapefiles

stockton <- get_acs(state = "CA", place = "Stockton", geography = "place", variables = "B01003_001", geometry = TRUE)

orange %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") 


harris %>%
  mutate(pct = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = pct)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  coord_sf(crs = 26915) + 
  scale_fill_viridis_c()