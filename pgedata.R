install.packages("tidycensus")
install.packages("tigris")
install.packages("tidyverse")
install.packages("spData")
#lehdr 

library(tidycensus)
library(tidyverse)
library(readr)
library(stringr)
library(dplyr)
library(tigris)
library(raster)
library(spData)
library(sf)
library(leaflet)
library(mapview)
library(data.table)
options(tigris_use_cache = TRUE)
census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab")

v17 <- load_variables(2017, "acs5")
write.csv(v17, file = "v17_acs5.csv")

# https://github.com/walkerke/tigris-zip-income/blob/master/prep.R
# https://journal.r-project.org/archive/2016/RJ-2016-043/RJ-2016-043.pdf

zips <- zctas(starts_with="95", cb = TRUE)
places <- places("CA", cb = TRUE)
places_stockton <- places[places@data$NAME == 'Stockton',]
zips_stockton <- zips[places_stockton,]

#Manual edits made to PG&E data downloaded from public site:
#2014_Q3_Gas, fields "Total Therms" and "Average Therms" renamed
#2017_Q4_Electricity and 2017_Q4_Gas, remove duplicate m=9 values

#Elec- Industrial mostly 0's, 95206 suddenly shows up with ~70 customers in 2014 Q3/Q4, 2015 Q1, 2016 M2/M3, 2018 M6/M9

pge <- do.call(rbind,lapply(2013:2018,function(year){
  
  df_year <- do.call(rbind,lapply(1:4,function(quarter){
    df_quarter <- do.call(rbind,lapply(c("Electric","Gas"),function(type){
      filename <- paste("S:\\Data Library\\PG&E\\PGE_",year,"_Q",quarter,"_",type,"UsageByZip.csv",sep = "")
      df_type <- read_csv(filename) %>% rename_all(toupper) %>% filter(ZIPCODE %in% zips_stockton$ZCTA5CE10) %>% group_by(ZIPCODE, CUSTOMERCLASS) %>% summarize(TOTALKBTU = ifelse(type == "Electric",sum(TOTALKWH)*3.4121416331,sum(TOTALTHM)*99.9761), TOTALCUSTOMERS = sum(TOTALCUSTOMERS)) %>%  dplyr::select(ZIPCODE, CUSTOMERCLASS, TOTALKBTU, TOTALCUSTOMERS)
    }))
  })) %>% group_by(ZIPCODE, CUSTOMERCLASS) %>% summarize(TOTALKBTU = sum(TOTALKBTU), TOTALCUSTOMERS = sum(TOTALCUSTOMERS)) %>% mutate(AVERAGEKBTU = TOTALKBTU/TOTALCUSTOMERS*12, YEAR = year)
}))

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


  
annual_summary_byclass <- pge %>% filter(!(CUSTOMERCLASS %in% c("Elec- Industrial"))) %>% group_by(CUSTOMERCLASS, YEAR) %>% summarize(annual_total = sum(TOTALKBTU))
ggplot(annual_summary_byclass, aes(as.factor(YEAR), annual_total)) + geom_bar(stat = "identity", aes(fill = CUSTOMERCLASS), position = "stack") + labs(x = "Year", y = "kBTU", title = "PG&E Territory Annual Energy Usage, 2013 to 2018", fill = "Energy Type")


annual_summary_total <- pge %>% filter(!(CUSTOMERCLASS %in% c("Elec- Industrial"))) %>% group_by(YEAR) %>% summarize(annual_total = sum(TOTALKBTU))
ggplot(annual_summary_total, aes(as.factor(YEAR), annual_total)) + geom_bar(stat = "identity", position = "dodge") + labs(x = "Year", y = "kBTU", title = "PG&E Territory Annual Energy Usage, 2013 to 2018")


zips_stockton_joined <- geo_join(zips_stockton, pge_2018_q1_elec, "ZCTA5CE10","ZIPCODE")

mapview(places_stockton, alpha.regions = 0, lwd = 3) +
  mapview(zips_stockton_joined, zcol = "SUM_KWH", legend = TRUE)
  


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