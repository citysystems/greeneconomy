###

append_matrix <- NULL
safegraphplaces_SJ <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/safegraphplaces.csv", header=TRUE, stringsAsFactors = FALSE)
safegraphplaces_CA <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/safegraphplaces_ca_edit.csv", header=TRUE, stringsAsFactors = FALSE)
safegraphplaces_tot <- rbind(safegraphplaces_SJ, safegraphplaces_CA)
safegraphplaces <- safegraphplaces_cleanse(safegraphplaces_tot)

for(counterMonth in 1:12){

  patterns_text <- patterns_choice(counterMonth)
  filename <- paste("C:/Users/Derek/Desktop/m_patterns_new", substr(patterns_text, 38, 51), "_new.RData", sep = "")
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
    geo_coordinates <- geocodeSL(location_name_iter)
    
    all_dest[index_all_dest, c("longitude")] <- geo_coordinates[1]
    all_dest[index_all_dest, c("latitude")] <- geo_coordinates[2]
    
  }
  
  # print(index_all_dest)
  
}

# save(all_dest, file = "C:/Users/Derek/Desktop/all_dest.RData")

load(file = "C:/Users/Derek/Desktop/all_dest.RData")

# Fixing the "NA" values within the "all_dest" data frame.

# NA 1

all_dest[15364, "name_address"] <- "234 broadway, san diego, ca, 92101"
all_dest[15364, "full_address"] <- "The Dailey Method, 234 broadway, san diego, ca, 92101"
all_dest[15364, "longitude"] <- geocodeSL(all_dest[15364, "name_address"])[1]
all_dest[15364, "latitude"] <- geocodeSL(all_dest[15364, "name_address"])[2]

# NA 2

all_dest[18282, "name_address"] <- "1980 College Blvd, Oceanside, CA 92054"
all_dest[18282, "full_address"] <- "CVS, 1980 College Blvd, Oceanside, CA 92054"
all_dest[18282, "longitude"] <- geocodeSL(all_dest[18282, "name_address"])[1]
all_dest[18282, "latitude"] <- geocodeSL(all_dest[18282, "name_address"])[2]

# NA 3

all_dest[34596, "name_address"] <- "6361 Wilshire Blvd Between San Vicente Blvd And Crescent Heights Blvd, Los Angeles, CA 90048"
all_dest[34596, "full_address"] <- "Jersey Mike's, 6361 Wilshire Blvd Between San Vicente Blvd And Crescent Heights Blvd, Los Angeles, CA 90048"
all_dest[34596, "longitude"] <- geocodeSL(all_dest[34596, "name_address"])[1]
all_dest[34596, "latitude"] <- geocodeSL(all_dest[34596, "name_address"])[2]

# NA 4

all_dest[35301, "name_address"] <- "1335 E 103rd St, Los Angeles, CA 90002"
all_dest[35301, "full_address"] <- "Ted Watkins Memorial Park, 1335 E 103rd St, Los Angeles, CA 90002"
all_dest[35301, "longitude"] <- geocodeSL(all_dest[35301, "name_address"])[1]
all_dest[35301, "latitude"] <- geocodeSL(all_dest[35301, "name_address"])[2]

# NA 5

all_dest[52383, "name_address"] <- "701 E El Camino Real 3rd Floor, Mountain View, CA 94040"
all_dest[52383, "full_address"] <- "Palo Alto Medical Foundation, 701 E El Camino Real 3rd Floor, Mountain View, CA 94040"
all_dest[52383, "longitude"] <- geocodeSL(all_dest[52383, "name_address"])[1]
all_dest[52383, "latitude"] <- geocodeSL(all_dest[52383, "name_address"])[2]

# NA 6

all_dest[57379, "name_address"] <- "228 stanford shopping center, palo alto, ca, 94304"
all_dest[57379, "full_address"] <- "Barry's Bootcamp, 228 stanford shopping center, palo alto, ca, 94304"
all_dest[57379, "longitude"] <- geocodeSL(all_dest[57379, "name_address"])[1]
all_dest[57379, "latitude"] <- geocodeSL(all_dest[57379, "name_address"])[2]

# NA 7

all_dest[65307, "name_address"] <- "101 The City Dr S Building 22C, Orange, CA 92868"
all_dest[65307, "full_address"] <- "William E Karnes MD, 101 The City Dr S Building 22C, Orange, CA 92868"
all_dest[65307, "longitude"] <- geocodeSL(all_dest[65307, "name_address"])[1]
all_dest[65307, "latitude"] <- geocodeSL(all_dest[65307, "name_address"])[2]

# NA 8

all_dest[120953, "name_address"] <- "2054 W Redlands Blvd, Redlands, CA 92373"
all_dest[120953, "full_address"] <- "Spirit Halloween, 2054 W Redlands Blvd, Redlands, CA 92373"
all_dest[120953, "longitude"] <- geocodeSL(all_dest[120953, "name_address"])[1]
all_dest[120953, "latitude"] <- geocodeSL(all_dest[120953, "name_address"])[2]

# NA 9

all_dest[121557, "name_address"] <- "1300 university avenue, claremont, ca, 91711"
all_dest[121557, "full_address"] <- "172 medical sciences court, 1300 university avenue, claremont, ca, 91711"
all_dest[121557, "longitude"] <- geocodeSL(all_dest[121557, "name_address"])[1] 
all_dest[121557, "latitude"] <- geocodeSL(all_dest[121557, "name_address"])[2]

# NA 10

all_dest[130932, "name_address"] <- "701 east el camino real, mountain view, ca, 94040"
all_dest[130932, "full_address"] <- "Palo Alto Medical Foundation, 701 east el camino real, mountain view, ca, 94040"
all_dest[130932, "longitude"] <- geocodeSL(all_dest[130932, "name_address"])[1] 
all_dest[130932, "latitude"] <- geocodeSL(all_dest[130932, "name_address"])[2]

# NA 11

all_dest[143837, "name_address"] <- "116 Broadway Lane, Walnut Creek, CA 94596"
all_dest[143837, "full_address"] <- "Broadway Plaza, 116 Broadway Lane, Walnut Creek, CA 94596"
all_dest[143837, "longitude"] <- geocodeSL(all_dest[143837, "name_address"])[1] 
all_dest[143837, "latitude"] <- geocodeSL(all_dest[143837, "name_address"])[2]

# NA 12

all_dest[152229, "name_address"] <- "5400 date avenue, sacramento, ca, 95841"
all_dest[152229, "full_address"] <- "Vacancy, 5400 date avenue, sacramento, ca, 95841"
all_dest[152229, "longitude"] <- geocodeSL(all_dest[152229, "name_address"])[1] 
all_dest[152229, "latitude"] <- geocodeSL(all_dest[152229, "name_address"])[2]

# NA 13

all_dest[160352, "name_address"] <- "600 indian wells lane, indian wells, ca, 92210"
all_dest[160352, "full_address"] <- "Hyatt Regency Indian Wells Resort & Spa, 600 indian wells lane, indian wells, ca, 92210"
all_dest[160352, "longitude"] <- geocodeSL(all_dest[160352, "name_address"])[1] 
all_dest[160352, "latitude"] <- geocodeSL(all_dest[160352, "name_address"])[2]

# NA 14

all_dest[161050, "name_address"] <- "7102 north fresno street, fresno, ca, 93720"
all_dest[161050, "full_address"] <- "Wells Fargo Advisors, 7102 north fresno street, fresno, ca, 93720"
all_dest[161050, "longitude"] <- geocodeSL(all_dest[161050, "name_address"])[1] 
all_dest[161050, "latitude"] <- geocodeSL(all_dest[161050, "name_address"])[2]

# NA 15

all_dest[165214, "name_address"] <- "0 heathcliff drive, tiburon, ca, 94920"
all_dest[165214, "full_address"] <- "Top of Hill, 0 heathcliff drive, tiburon, ca, 94920"
all_dest[165214, "longitude"] <- geocodeSL(all_dest[165214, "name_address"])[1] 
all_dest[165214, "latitude"] <- geocodeSL(all_dest[165214, "name_address"])[2]

# NA 16

all_dest[168029, "name_address"] <- "350 girard street san francisco ca 94134"
all_dest[168029, "full_address"] <- "Saturday Location, 350 girard street san francisco ca 94134"
all_dest[168029, "longitude"] <- geocodeSL(all_dest[168029, "name_address"])[1] 
all_dest[168029, "latitude"] <- geocodeSL(all_dest[168029, "name_address"])[2]

# NA 17

all_dest[174279, "name_address"] <- "cotai macau, los angeles, ca, 90272"
all_dest[174279, "full_address"] <- "Sheraton Grand Macao Hotel, Cotai Central, cotai macau, los angeles, ca, 90272"
all_dest[174279, "longitude"] <- geocodeSL(all_dest[174279, "name_address"])[1] 
all_dest[174279, "latitude"] <- geocodeSL(all_dest[174279, "name_address"])[2]

# NA 18

all_dest[175526, "name_address"] <- "1 world way, los angeles, ca, 90045"
all_dest[175526, "full_address"] <- "tom bradley international terminal, 1 world way, los angeles, ca, 90045"
all_dest[175526, "longitude"] <- geocodeSL(all_dest[175526, "name_address"])[1] 
all_dest[175526, "latitude"] <- geocodeSL(all_dest[175526, "name_address"])[2]

# NA 19

all_dest[192653, "name_address"] <-  "310 easy street bunker school, mountain view, ca, 94129"
all_dest[192653, "full_address"] <- "german international school of silicon valley, 310 easy street bunker school, mountain view, ca, 94129"
all_dest[192653, "longitude"] <- geocodeSL(all_dest[192653, "name_address"])[1] 
all_dest[192653, "latitude"] <- geocodeSL(all_dest[192653, "name_address"])[2]

# NA 20

all_dest[193795, "name_address"] <- "5110 foothills blvd roseville ca"
all_dest[193795, "full_address"] <- "Ripped Fitness, 5110 foothills blvd roseville ca"
all_dest[193795, "longitude"] <- geocodeSL(all_dest[193795, "name_address"])[1] 
all_dest[193795, "latitude"] <- geocodeSL(all_dest[193795, "name_address"])[2]

# NA 21

all_dest[194479, "name_address"] <- "11301 wilshire boulevard, los angeles, ca, 90073"
all_dest[194479, "full_address"] <- "veterans affairs heathcare center, 11301 wilshire boulevard, los angeles, ca, 90073"
all_dest[194479, "longitude"] <- geocodeSL(all_dest[194479, "name_address"])[1] 
all_dest[194479, "latitude"] <- geocodeSL(all_dest[194479, "name_address"])[2]

# NA 22

all_dest[198934, "name_address"] <- "10250 santa monica boulevard, los angeles, ca, 90067"
all_dest[198934, "full_address"] <- "westfield century city, 10250 santa monica boulevard, los angeles, ca, 90067"
all_dest[198934, "longitude"] <- geocodeSL(all_dest[198934, "name_address"])[1] 
all_dest[198934, "latitude"] <- geocodeSL(all_dest[198934, "name_address"])[2]

# NA 23

all_dest[199363, "name_address"] <- "2664 griffith park boulevard, los angeles, ca, 90039"
all_dest[199363, "full_address"] <- "Z Pizza, 2664 griffith park boulevard, los angeles, ca, 90039"
all_dest[199363, "longitude"] <- geocodeSL(all_dest[199363, "name_address"])[1] 
all_dest[199363, "latitude"] <- geocodeSL(all_dest[199363, "name_address"])[2]

# NA 24

all_dest[210775, "name_address"] <- "2051 Marengo St, Los Angeles, CA 90033"
all_dest[210775, "full_address"] <- "LAC+USC Medical Center, 2051 Marengo St, Los Angeles, CA 90033"
all_dest[210775, "longitude"] <- geocodeSL(all_dest[210775, "name_address"])[1] 
all_dest[210775, "latitude"] <- geocodeSL(all_dest[210775, "name_address"])[2]

# NA 25

all_dest[220288, "name_address"] <- "0 gilman street, albany, ca, 94710"
all_dest[220288, "full_address"] <- "west of the freeway between the soccer fields and golden gate fields, 0 gilman street, albany, ca, 94710"
all_dest[220288, "longitude"] <- geocodeSL(all_dest[220288, "name_address"])[1] 
all_dest[220288, "latitude"] <- geocodeSL(all_dest[220288, "name_address"])[2]

# NA 26

all_dest[222887, "name_address"] <- "11190 warner avenue, fountain valley, ca, 92708"
all_dest[222887, "full_address"] <- "unit 401 sleep and research center of fountain valley, 11190 warner avenue, fountain valley, ca, 92708"
all_dest[222887, "longitude"] <- geocodeSL(all_dest[222887, "name_address"])[1] 
all_dest[222887, "latitude"] <- geocodeSL(all_dest[222887, "name_address"])[2]

# NA 27

all_dest[227695, "name_address"] <- "640 roosevelt, irvine, ca, 92620"
all_dest[227695, "full_address"] <- "jeffrey office park across from the cypress village shopping center, 640 roosevelt, irvine, ca, 92620"
all_dest[227695, "longitude"] <- geocodeSL(all_dest[227695, "name_address"])[1] 
all_dest[227695, "latitude"] <- geocodeSL(all_dest[227695, "name_address"])[2]

# NA 28

all_dest[232999, "name_address"] <- "46940 manises valencia espagne, valencia, ca, 91354"
all_dest[232999, "full_address"] <- "valencia airport carretera del aeropuerto south north, 46940 manises valencia espagne, valencia, ca, 91354"
all_dest[232999, "longitude"] <- geocodeSL(all_dest[232999, "name_address"])[1] 
all_dest[232999, "latitude"] <- geocodeSL(all_dest[232999, "name_address"])[2]

# NA 29

all_dest[247305, "name_address"] <- "calle 16, rancho cucamonga, ca, 91729"
all_dest[247305, "full_address"] <- "666 col positos y rivera entre echeven y hernan cortez cp, calle 16, rancho cucamonga, ca, 91729"
all_dest[247305, "longitude"] <- geocodeSL(all_dest[247305, "name_address"])[1] 
all_dest[247305, "latitude"] <- geocodeSL(all_dest[247305, "name_address"])[2]

# NA 30

all_dest[249613, "name_address"] <- "2300 sweetwater road, national city, ca, 91950"
all_dest[249613, "full_address"] <- "under interstate 805 in the caltrans park and ride parking lot, 2300 sweetwater road, national city, ca, 91950"
all_dest[249613, "longitude"] <- geocodeSL(all_dest[249613, "name_address"])[1] 
all_dest[249613, "latitude"] <- geocodeSL(all_dest[249613, "name_address"])[2]

# NA 31

all_dest[259702, "name_address"] <- "204 hampton drive, los angeles, ca, 90291"
all_dest[259702, "full_address"] <- "kitchen 1 at street joseph center culinary training program 204 hampton drive, los angeles, ca, 90291"
all_dest[259702, "longitude"] <- geocodeSL(all_dest[259702, "name_address"])[1] 
all_dest[259702, "latitude"] <- geocodeSL(all_dest[259702, "name_address"])[2]

# NA 32

all_dest[260571, "name_address"] <- "interstate 15, san diego, ca, 92126"
all_dest[260571, "full_address"] <- "mira mesa boulevard park and ride north west of interstate 15 off ramp, interstate 15, san diego, ca, 92126"
all_dest[260571, "longitude"] <- geocodeSL(all_dest[260571, "name_address"])[1] 
all_dest[260571, "latitude"] <- geocodeSL(all_dest[260571, "name_address"])[2]

# NA 33

all_dest[260678, "name_address"] <- "101 south brand boulevard, glendale, ca, 91210"
all_dest[260678, "full_address"] <- "glendale galleria entrance located on the corner of brand and broadway, 101 south brand boulevard, glendale, ca, 91210"
all_dest[260678, "longitude"] <- geocodeSL(all_dest[260678, "name_address"])[1] 
all_dest[260678, "latitude"] <- geocodeSL(all_dest[260678, "name_address"])[2]

# NA 34

all_dest[276975, "name_address"] <- "84245 indio springs drive, indio, ca, 92203"
all_dest[276975, "full_address"] <- "84245 indio springs parkway fantasy springs resort casino, 84245 indio springs drive, indio, ca, 92203"
all_dest[276975, "longitude"] <- geocodeSL(all_dest[276975, "name_address"])[1] 
all_dest[276975, "latitude"] <- geocodeSL(all_dest[276975, "name_address"])[2]

# NA 35

all_dest[297306, "name_address"] <- "3243 south la cienega boulevard, los angeles, ca, 90016"
all_dest[297306, "full_address"] <- "Blackwelder, 3243 south la cienega boulevard, los angeles, ca, 90016"
all_dest[297306, "longitude"] <- geocodeSL(all_dest[297306, "name_address"])[1] 
all_dest[297306, "latitude"] <- geocodeSL(all_dest[297306, "name_address"])[2]

# NA 36

all_dest[304685, "name_address"] <- "2198 filbert street, san francisco, ca, 94123"
all_dest[304685, "full_address"] <- "Rapha, 2198 filbert street, san francisco, ca, 94123"
all_dest[304685, "longitude"] <- geocodeSL(all_dest[304685, "name_address"])[1] 
all_dest[304685, "latitude"] <- geocodeSL(all_dest[304685, "name_address"])[2]

# NA 37

all_dest[310950, "name_address"] <- "10152 rancho carmel drive, san diego, ca, 92128"
all_dest[310950, "full_address"] <- "carmel mountain ranch sabre springs recreation center, 10152 rancho carmel drive, san diego, ca, 92128"
all_dest[310950, "longitude"] <- geocodeSL(all_dest[310950, "name_address"])[1] 
all_dest[310950, "latitude"] <- geocodeSL(all_dest[310950, "name_address"])[2]

# NA 38

all_dest[328877, "name_address"] <- "701 east el camino real north wing second floor, mountain view, ca, 94040"
all_dest[328877, "full_address"] <- "palo alto medical foundation north wing second floor, 701 east el camino real north wing second floor, mountain view, ca, 94040"
all_dest[328877, "longitude"] <- geocodeSL(all_dest[328877, "name_address"])[1] 
all_dest[328877, "latitude"] <- geocodeSL(all_dest[328877, "name_address"])[2]

# NA 39

all_dest[331796, "name_address"] <- "6382 east pacific coast highway, long beach, ca, 90803"
all_dest[331796, "full_address"] <- "marina pacifica shopping center, 6382 east pacific coast highway, long beach, ca, 90803"
all_dest[331796, "longitude"] <- geocodeSL(all_dest[331796, "name_address"])[1] 
all_dest[331796, "latitude"] <- geocodeSL(all_dest[331796, "name_address"])[2]

# NA 40

all_dest[339936, "name_address"] <- "280 charles east young drive, los angeles, ca, 90095"
all_dest[339936, "full_address"] <- "north charles east young research library 1 floor, 280 charles east young drive, los angeles, ca, 90095"
all_dest[339936, "longitude"] <- geocodeSL(all_dest[339936, "name_address"])[1] 
all_dest[339936, "latitude"] <- geocodeSL(all_dest[339936, "name_address"])[2]

# NA 41

all_dest[341723, "name_address"] <- "5453 b hollywood boulevard, los angeles, ca, 90027"
all_dest[341723, "full_address"] <- "corner of hollywood boulevard and western avenue in hollywest promenade, 5453 b hollywood boulevard, los angeles, ca, 90027"
all_dest[341723, "longitude"] <- geocodeSL(all_dest[341723, "name_address"])[1] 
all_dest[341723, "latitude"] <- geocodeSL(all_dest[341723, "name_address"])[2]

# NA 42

all_dest[350985, "name_address"] <- "701 east el camino real, mountain view, ca, 94040"
all_dest[350985, "full_address"] <- "palo alto medical foundation south wing third floor, 701 east el camino real, mountain view, ca, 94040"
all_dest[350985, "longitude"] <- geocodeSL(all_dest[350985, "name_address"])[1] 
all_dest[350985, "latitude"] <- geocodeSL(all_dest[350985, "name_address"])[2]

# NA 43

all_dest[369936, "name_address"] <- "4770 natomas boulevard, sacramento, ca, 95835"
all_dest[369936, "full_address"] <- "corner of natomas boulevard and truxel drive park place, 4770 natomas boulevard, sacramento, ca, 95835"
all_dest[369936, "longitude"] <- geocodeSL(all_dest[369936, "name_address"])[1] 
all_dest[369936, "latitude"] <- geocodeSL(all_dest[369936, "name_address"])[2]

# NA 44

all_dest[370061, "name_address"] <- "6600 topanga boulevard, los angeles, ca, 91303"
all_dest[370061, "full_address"] <- "karina smirnoff dance studio the village westfield topanga, 6600 topanga boulevard, los angeles, ca, 91303"
all_dest[370061, "longitude"] <- geocodeSL(all_dest[370061, "name_address"])[1] 
all_dest[370061, "latitude"] <- geocodeSL(all_dest[370061, "name_address"])[2]

# NA 45

all_dest[385319, "name_address"] <- "11601 main street, sunol, ca, 94586"
all_dest[385319, "full_address"] <- "sunol glen elementary school, 11601 main street, sunol, ca, 94586"
all_dest[385319, "longitude"] <- geocodeSL(all_dest[385319, "name_address"])[1] 
all_dest[385319, "latitude"] <- geocodeSL(all_dest[385319, "name_address"])[2]

# NA 46

all_dest[400767, "name_address"] <- "790 inland center drive, san bernardino, ca, 92404"
all_dest[400767, "full_address"] <- "across from inland center mall interstate 215 and inland c, 790 inland center drive, san bernardino, ca, 92404"
all_dest[400767, "longitude"] <- geocodeSL(all_dest[400767, "name_address"])[1] 
all_dest[400767, "latitude"] <- geocodeSL(all_dest[400767, "name_address"])[2]

# NA 47

all_dest[405244, "name_address"] <- "5151 west channel islands boulevard, port hueneme, ca, 93041"
all_dest[405244, "full_address"] <- "suite 605, 5151 west channel islands boulevard, port hueneme, ca, 93041"
all_dest[405244, "longitude"] <- geocodeSL(all_dest[405244, "name_address"])[1] 
all_dest[405244, "latitude"] <- geocodeSL(all_dest[405244, "name_address"])[2]

# NA 48

all_dest[408271, "name_address"] <- "1675 owens street, san francisco, ca, 94158"
all_dest[408271, "full_address"] <- "ucsf mission bay campus william j ritter community center, 1675 owens street, san francisco, ca, 94158"
all_dest[408271, "longitude"] <- geocodeSL(all_dest[408271, "name_address"])[1] 
all_dest[408271, "latitude"] <- geocodeSL(all_dest[408271, "name_address"])[2]

# NA 49

all_dest[429857, "name_address"] <- "carretera poza rica cazones km 50 4706 col, bakersfield, ca, 93306"
all_dest[429857, "full_address"] <- "la rueda entre esquina con otono cp 93306, carretera poza rica cazones km 50 4706 col, bakersfield, ca, 93306"
all_dest[429857, "longitude"] <- geocodeSL(all_dest[429857, "name_address"])[1] 
all_dest[429857, "latitude"] <- geocodeSL(all_dest[429857, "name_address"])[2]

# NA 50

all_dest[431969, "name_address"] <- "juan de la luz enriquez 707 col, lookout, ca, 96054"
all_dest[431969, "full_address"] <- "barrio la palma entre ramon corona y 20 de noviembre cp 96054, juan de la luz enriquez 707 col, lookout, ca, 96054"
all_dest[431969, "longitude"] <- geocodeSL(all_dest[431969, "name_address"])[1] 
all_dest[431969, "latitude"] <- geocodeSL(all_dest[431969, "name_address"])[2]

# NA 51

all_dest[440631, "name_address"] <- "street cyril of jerusalem school, 4548 haskell avenue, encino, ca, 91436"
all_dest[440631, "full_address"] <- "4548 haskell avenue, encino, ca, 91436"
all_dest[440631, "longitude"] <- geocodeSL(all_dest[440631, "name_address"])[1] 
all_dest[440631, "latitude"] <- geocodeSL(all_dest[440631, "name_address"])[2]

# NA 52

all_dest[458309, "name_address"] <- "39000 bob hope drive, rancho mirage, ca, 92270"
all_dest[458309, "full_address"] <- "harry and diane rinker building desert orthopedic center, 39000 bob hope drive, rancho mirage, ca, 92270"
all_dest[458309, "longitude"] <- geocodeSL(all_dest[458309, "name_address"])[1] 
all_dest[458309, "latitude"] <- geocodeSL(all_dest[458309, "name_address"])[2]

# NA 53

all_dest[459768, "name_address"] <- "701 east el camino real, mountain view, ca, 94040"
all_dest[459768, "full_address"] <- "palo alto medical foundation south wing third floor, 701 east el camino real, mountain view, ca, 94040"
all_dest[459768, "longitude"] <- geocodeSL(all_dest[459768, "name_address"])[1] 
all_dest[459768, "latitude"] <- geocodeSL(all_dest[459768, "name_address"])[2]

# NA 54

all_dest[459804, "name_address"] <- "701 east el camino real, mountain view, ca, 94040"
all_dest[459804, "full_address"] <- "palo alto medical foundation south wing second floor, 701 east el camino real, mountain view, ca, 94040"
all_dest[459804, "longitude"] <- geocodeSL(all_dest[459804, "name_address"])[1] 
all_dest[459804, "latitude"] <- geocodeSL(all_dest[459804, "name_address"])[2]

#####

### The following was used for debugging. Ignore.

osrmTable_Stockton_VMT_v2 <- NULL

for (counterTable in 34596:nrow(all_dest)){
  
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  osrmTable_Stockton_VMT_v2 <- rbind(osrmTable_Stockton_VMT_v2, safegraph_osrm)

  print(counterTable)

}

osrmTable_Stockton_VMT_v2 <- do.call(rbind, lapply( 1:nrow(all_dest), function(counterTable){
  # counterTable
  safegraph_osrm <- data.frame( osrmTable(src = sboi_centroid[, c("GEOID", "geometry")], dst = all_dest[counterTable, c("full_address", "longitude", "latitude")]) )
  # counterTable
  safegraph_osrm <- data.frame( cbind(sboi_centroid$GEOID, all_dest[counterTable, c("safegraph_place_id", "full_address")], safegraph_osrm) )
  colnames(safegraph_osrm) <- c("source_GEOID", "safegraph_place_id", "full_address", "time_minutes", "sources.lon", "sources.lat", "destination.lon", "destination.lat")
  
  return(safegraph_osrm)
  
}))

save(osrmTable_Stockton_VMT_v2, file = "C:/Users/Derek/Desktop/osrmTable_Stockton_VMT_v2.RData")

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
