library(tidycensus)
library(censusapi)
library(tigris)
library(dplyr)
library(units)
library(ggplot2)
library(lehdr)
library(sf)
library(osrm)
library(mapview)
library(readr)
library(tidyverse)
library(magrittr)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
options(osrm.server = "http://127.0.0.1:5000/")
census_api_key("c8aa67e4086b4b5ce3a8717f59faa9a28f611dab", overwrite = TRUE)
Sys.setenv(CENSUS_KEY="c8aa67e4086b4b5ce3a8717f59faa9a28f611dab")
readRenviron("~/.Renviron")

acs5_17 <- load_variables(2017, "acs5")

zcta <- zctas(starts_with="95", cb = TRUE) %>% mutate(ZCTA5CE10 = as.numeric(ZCTA5CE10))
zips_stockton <- st_read("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/ZipCodes/ZipCodes.shp") %>% st_transform(st_crs(zcta))
stockton_boundary <- places("CA", cb = TRUE) %>% filter(NAME == "Stockton")
stockton_boundary_influence <- st_read("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/SpheresOfInfluence/SpheresOfInfluence.shp") %>% filter(SPHERE == "STOCKTON") %>% st_transform(st_crs(zcta))
stockton_boundary_influence <- stockton_boundary_influence[1,]
zcta_stockton <- zcta[stockton_boundary_influence,] %>% filter(!ZCTA5CE10 %in% c("95242","95240","95336","95330"))
# zcta_stockton <- zcta[which(zcta$ZCTA5CE10 %in% st_centroid(zcta)[stockton_boundary_influence,]$ZCTA5CE10),]



pge_elec_emissions_factor <- data.frame(year = 2013:2018, factor = c(427,434.92,404.51,293.67,210,210))

pge_stockton <- do.call(rbind,lapply(2013:2018,function(year){
  factor <- pge_elec_emissions_factor[match(year,pge_elec_emissions_factor$year),2]
  df_year <- do.call(rbind,lapply(1:4,function(quarter){
    df_quarter <- do.call(rbind,lapply(c("Electric","Gas"),function(type){
      filename <- paste("S:\\Data Library\\PG&E\\PGE_",year,"_Q",quarter,"_",type,"UsageByZip.csv",sep = "")
      df_type <- read_csv(filename) %>% rename_all(toupper) %>% filter(ZIPCODE %in% zcta_stockton$ZCTA5CE10) %>% mutate(ZIPCODE = as.numeric(ZIPCODE)) %>% group_by(ZIPCODE, CUSTOMERCLASS) %>% summarize(TOTALKBTU = ifelse(type == "Electric",sum(TOTALKWH)*3.4121416331,sum(TOTALTHM)*99.9761), TOTALMTCO2 = ifelse(type == "Electric",sum(TOTALKWH)*factor/1000*0.000453592,sum(TOTALTHM)*0.00531), TOTALCUSTOMERS = sum(TOTALCUSTOMERS)) %>%  dplyr::select(ZIPCODE, CUSTOMERCLASS, TOTALKBTU, TOTALMTCO2, TOTALCUSTOMERS)
    }))
  })) %>% group_by(ZIPCODE, CUSTOMERCLASS) %>% summarize(TOTALKBTU = sum(TOTALKBTU), TOTALMTCO2 = sum(TOTALMTCO2), TOTALCUSTOMERS = sum(TOTALCUSTOMERS)) %>%  mutate(YEAR = year, KBTUPERCUST = TOTALKBTU/TOTALCUSTOMERS, MTCO2PERCUST = TOTALMTCO2/TOTALCUSTOMERS) %>% mutate(CUSTOMERCLASS = ifelse(CUSTOMERCLASS == "Elec- Residential","ER",ifelse(CUSTOMERCLASS == "Gas- Residential","GR",ifelse(CUSTOMERCLASS == "Elec- Commercial","EC",ifelse(CUSTOMERCLASS == "Elec- Industrial","EI",ifelse(CUSTOMERCLASS == "Elec- Agricultural","EA","GC"))))))
}))

summary_mtco2_average <- pge_stockton %>% filter(!(CUSTOMERCLASS %in% c("Elec- Industrial","Elec- Agricultural"))) %>% mutate(type = substr(CUSTOMERCLASS,1,1)) %>% group_by(type, YEAR) %>% summarize(annual_average = sum(TOTALMTCO2)/sum(TOTALCUSTOMERS)*12)

ggplot(summary_mtco2_average, aes(as.factor(YEAR), annual_average)) + geom_bar(stat = "identity", aes(fill = type), position = "dodge") + labs(x = "Year", y = "MTCO2e/customer", title = "PG&E Territory Annual Energy Usage, 2013 to 2018") + scale_fill_discrete(name="Energy Type",labels = c("Electricity","Gas"))

zips_spread <- pge_stockton %>% filter(!CUSTOMERCLASS %in% c("EI","EA") & (YEAR == 2016)) %>% select(-YEAR) %>% gather(key = "type", value, TOTALKBTU:MTCO2PERCUST) %>% unite(temp,CUSTOMERCLASS,type) %>% spread(temp,value)

zips_mtco2_total <- pge_stockton %>% mutate(TYPE = substr(CUSTOMERCLASS,1,1), E_TOTALKBTU = ifelse(TYPE == "E",TOTALKBTU,0), E_TOTALMTCO2 = ifelse(TYPE == "E",TOTALMTCO2,0), E_TOTALCUSTOMERS = ifelse(TYPE == "E",TOTALCUSTOMERS,0), G_TOTALKBTU = ifelse(TYPE == "G",TOTALKBTU,0), G_TOTALMTCO2 = ifelse(TYPE == "G",TOTALMTCO2,0), G_TOTALCUSTOMERS = ifelse(TYPE == "G",TOTALCUSTOMERS,0)) %>% filter(YEAR == 2016) %>% group_by(ZIPCODE) %>% summarize(TOTALKBTU = sum(TOTALKBTU), TOTALMTCO2 = sum(TOTALMTCO2), TOTALCUSTOMERS = max(TOTALCUSTOMERS), E_TOTALKBTU = sum(E_TOTALKBTU), E_TOTALMTCO2 = sum(E_TOTALMTCO2), E_TOTALCUSTOMERS = max(E_TOTALCUSTOMERS), G_TOTALKBTU = sum(G_TOTALKBTU), G_TOTALMTCO2 = sum(G_TOTALMTCO2), G_TOTALCUSTOMERS = max(G_TOTALCUSTOMERS)) %>% left_join(zips_spread, by = "ZIPCODE")

zcta_stockton_joined <- zcta_stockton %>% left_join(zips_mtco2_total, by=c("ZCTA5CE10"="ZIPCODE")) %>% rename(ZIPCODE = ZCTA5CE10) %>% mutate(ZIPCODE = as.numeric(ZIPCODE))



# sjc_bldg <- read_csv("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/USBuildingFootprints/ca_06077_footprints.csv") %>% st_as_sf(wkt = "WKT") %>% st_set_crs(4326) %>% st_transform(st_crs(zcta)) %>% mutate(id = row_number())
# stockton_bldg <- sjc_bldg[which(sjc_bldg$id %in% st_centroid(sjc_bldg)[stockton_boundary_influence,]$id),]
load("C:/Users/derek/Google Drive/City Systems/Stockton Green Economy/stockton_bldg.Rdata")  
stockton_bldg_final %<>% st_transform(st_crs(zcta)) %>% mutate(ZIPCODE = substr(`SITUS ZIP CODE`,1,5)) #do spatial join in the future for zipcodes

zcta_bldg_summary <- stockton_bldg_final %>% mutate(bldgground = as.numeric(st_area(WKT))*10.7639,bldgtot = ifelse(is.na(`STORIES NUMBER`), bldgground, bldgground * `STORIES NUMBER`), ZIPCODE = as.numeric(ZIPCODE), ZONING = ifelse(substr(ZONE,1,1)=="R","R", ifelse(substr(ZONE,1,1)=="C","C","O"))) %>% st_set_geometry(NULL)

zcta_bldg_spread <- zcta_bldg_summary %>% group_by(ZIPCODE, ZONING) %>% summarise(NUMBLDG = n(), TOTSQFTGROUND = sum(bldgground), TOTSQFT = sum(bldgtot)) %>% filter(!is.na(ZIPCODE) & !is.na(ZONING)) %>% gather(key, value, NUMBLDG:TOTSQFT) %>% unite(temp,ZONING,key) %>% spread(temp,value)

zcta_bldg_summary %<>% group_by(ZIPCODE) %>% summarise(NUMBLDG = n(), TOTSQFTGROUND = sum(bldgground), TOTSQFT = sum(bldgtot)) %>% left_join(zcta_bldg_spread, by = "ZIPCODE")

zcta_bldg_stockton_joined <- zcta_stockton_joined %>% filter(ZIPCODE != 95211) %>% left_join(zcta_bldg_summary, by = "ZIPCODE") %>% 
  mutate(ER_KBTUperSQFT = ER_TOTALKBTU/R_TOTSQFT, 
         GR_KBTUperSQFT = GR_TOTALKBTU/R_TOTSQFT, 
         EC_KBTUperSQFT = EC_TOTALKBTU/C_TOTSQFT, 
         GC_KBTUperSQFT = GC_TOTALKBTU/C_TOTSQFT, 
         R_KBTUperSQFT = (ER_TOTALKBTU+GR_TOTALKBTU)/R_TOTSQFT, 
         C_KBTUperSQFT = (EC_TOTALKBTU+GC_TOTALKBTU)/C_TOTSQFT, 
         E_KBTUperSQFT = E_TOTALKBTU/TOTSQFT, 
         G_KBTUperSQFT = G_TOTALKBTU/TOTSQFT, 
         KBTUperSQFT = TOTALKBTU/TOTSQFT,
         ER_MTCO2perSQFT = ER_TOTALMTCO2/R_TOTSQFT, 
         GR_MTCO2perSQFT = GR_TOTALMTCO2/R_TOTSQFT, 
         EC_MTCO2perSQFT = EC_TOTALMTCO2/C_TOTSQFT, 
         GC_MTCO2perSQFT = GC_TOTALMTCO2/C_TOTSQFT, 
         R_MTCO2perSQFT = (ER_TOTALMTCO2+GR_TOTALMTCO2)/R_TOTSQFT, 
         C_MTCO2perSQFT = (EC_TOTALMTCO2+GC_TOTALMTCO2)/C_TOTSQFT, 
         E_MTCO2perSQFT = E_TOTALMTCO2/TOTSQFT, 
         G_MTCO2perSQFT = G_TOTALMTCO2/TOTSQFT, 
         MTCO2perSQFT = TOTALMTCO2/TOTSQFT)

mapview(zcta_bldg_stockton_joined, zcol= c("ER_MTCO2perSQFT", "GR_MTCO2perSQFT", "EC_MTCO2perSQFT", "GC_MTCO2perSQFT", "R_MTCO2perSQFT","C_MTCO2perSQFT","E_MTCO2perSQFT","G_MTCO2perSQFT","MTCO2perSQFT"), map.types = c("OpenStreetMap"), legend = TRUE, hide = TRUE)

zcta_bldg_stockton_summary <- zcta_bldg_stockton_joined %>% st_set_geometry(NULL) %>% 
  summarise(NUMBLDG = sum(NUMBLDG), 
            R_NUMBLDG = sum(R_NUMBLDG),
            C_NUMBLDG = sum(C_NUMBLDG),
            TOTSQFTGROUND = sum(TOTSQFTGROUND), 
            TOTSQFT = sum(TOTSQFT),
            R_TOTSQFT = sum(R_TOTSQFT),
            C_TOTSQFT = sum(C_TOTSQFT),
            R_TOTSQFTGROUND = sum(R_TOTSQFTGROUND),
            C_TOTSQFTGROUND = sum(C_TOTSQFTGROUND),
            TOTALKBTU = sum(TOTALKBTU),
            E_TOTALKBTU = sum(E_TOTALKBTU),
            G_TOTALKBTU = sum(G_TOTALKBTU),
            R_TOTALKBTU = sum(ER_TOTALKBTU)+sum(GR_TOTALKBTU),
            C_TOTALKBTU = sum(EC_TOTALKBTU)+sum(GC_TOTALKBTU),
            ER_TOTALKBTU = sum(ER_TOTALKBTU),
            GR_TOTALKBTU = sum(GR_TOTALKBTU),
            EC_TOTALKBTU = sum(EC_TOTALKBTU),
            GC_TOTALKBTU = sum(GC_TOTALKBTU),
            TOTALMTCO2 = sum(TOTALMTCO2),
            E_TOTALMTCO2 = sum(E_TOTALMTCO2),
            G_TOTALMTCO2 = sum(G_TOTALMTCO2),
            R_TOTALMTCO2 = sum(ER_TOTALMTCO2)+sum(GR_TOTALMTCO2),
            C_TOTALMTCO2 = sum(EC_TOTALMTCO2)+sum(GC_TOTALMTCO2),
            ER_TOTALMTCO2 = sum(ER_TOTALMTCO2),
            GR_TOTALMTCO2 = sum(GR_TOTALMTCO2),
            EC_TOTALMTCO2 = sum(EC_TOTALMTCO2),
            GC_TOTALMTCO2 = sum(GC_TOTALMTCO2)) %>% 
  mutate(ER_KBTUperSQFT = ER_TOTALKBTU/R_TOTSQFT, 
         GR_KBTUperSQFT = GR_TOTALKBTU/R_TOTSQFT, 
         EC_KBTUperSQFT = EC_TOTALKBTU/C_TOTSQFT, 
         GC_KBTUperSQFT = GC_TOTALKBTU/C_TOTSQFT, 
         R_KBTUperSQFT = (ER_TOTALKBTU+GR_TOTALKBTU)/R_TOTSQFT, 
         C_KBTUperSQFT = (EC_TOTALKBTU+GC_TOTALKBTU)/C_TOTSQFT, 
         E_KBTUperSQFT = E_TOTALKBTU/TOTSQFT, 
         G_KBTUperSQFT = G_TOTALKBTU/TOTSQFT, 
         KBTUperSQFT = TOTALKBTU/TOTSQFT,
         ER_MTCO2perSQFT = ER_TOTALMTCO2/R_TOTSQFT, 
         GR_MTCO2perSQFT = GR_TOTALMTCO2/R_TOTSQFT, 
         EC_MTCO2perSQFT = EC_TOTALMTCO2/C_TOTSQFT, 
         GC_MTCO2perSQFT = GC_TOTALMTCO2/C_TOTSQFT, 
         R_MTCO2perSQFT = (ER_TOTALMTCO2+GR_TOTALMTCO2)/R_TOTSQFT, 
         C_MTCO2perSQFT = (EC_TOTALMTCO2+GC_TOTALMTCO2)/C_TOTSQFT, 
         E_MTCO2perSQFT = E_TOTALMTCO2/TOTSQFT, 
         G_MTCO2perSQFT = G_TOTALMTCO2/TOTSQFT, 
         MTCO2perSQFT = TOTALMTCO2/TOTSQFT)

write.csv(zcta_bldg_stockton_summary, file = "bldg_stockton_summary.csv")





zbp_stockton <- data.frame(matrix(ncol=5,nrow=0))
colnames(zbp_stockton) <- c("zipcode","EMP","ESTAB","PAYANN","year")

for(year in 2010:2016){
  temp <- getCensus(name = "zbp",
                    vintage = year,
                    region = "zipcode:*",
                    vars = c("EMP","ESTAB","PAYANN")) %>% filter(zipcode %in% zcta_stockton$ZCTA5CE10) %>% mutate(year = year)
  
  zbp_stockton<- rbind(zbp_stockton,temp)
}

pop_stockton <- data.frame(matrix(ncol=2,nrow=0))
colnames(pop_stockton) <- c("POP","year")

for(year in 2010:2016){
  temp <- getCensus(name = "acs/acs1",
                  vintage = year,
                  vars = c("B01003_001E"),
                  region = "place:75000",
                  regionin = "state:06") %>% mutate(POP = B01003_001E, year = year) %>% select(POP,year)
  pop_stockton<- rbind(pop_stockton,temp)
}

pop_jobs_stockton <- zbp_stockton %>% group_by(year) %>% summarize(EMP=sum(as.numeric(EMP)),ESTAB=sum(as.numeric(ESTAB)),PAYANN=sum(as.numeric(PAYANN))) %>% left_join(pop_stockton, by="year")

ggplot(pop_jobs_stockton, aes(x = year)) + geom_line(aes(y = POP, colour = "Population")) + geom_line(aes(y = EMP*3, colour = "Employment")) + scale_y_continuous(sec.axis = sec_axis(~./3, name = "Employment")) + scale_colour_manual(values = c("blue","red")) + labs(title = "Stockton, CA", y = "Population", x = "Year", colour = "Parameter")



#add projections






qwi_2005 <- getCensus(name = "timeseries/qwi/sa",
            region = "county:077",
            regionin = "state:06",
            vars = c("Emp"),
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

save(stockton_lodes_w, stockton_lodes_h, stockton_rac, stockton_wac, file = "C:\\Users\\derek\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes.R")
load("C:\\Users\\derek\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes.R")

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

write_csv(stockton_lodes_w_counties, "C:\\Users\\derek\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes_w_counties.csv")

# save(stockton_lodes_w, stockton_lodes_h, stockton_rac, stockton_wac, stockton_lodes_summary, prep, file = "C:\\Users\\derek\\Google Drive\\City Systems\\Stockton Green Economy\\LODES\\stockton_lodes.R")
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
