### Install Libraries (do once)

install.packages("readxl")
install.packages("dplyr")
install.packages("reshape2")
install.packages("fitdistrplus")
install.packages("stringr")

### Use of installed packages

library(readxl)
library(dplyr)
library(reshape2)
library(fitdistrplus)
library(stringr)

##########

### Read Excel Sheets

Travel_Decay_Analysis <- read_excel("/Users/gregoryforbes/Desktop/Stanford\ Career/Q7\ -\ Spring\ Quarter/CEE\ 224Z\ -\ Sustainable\ Urban\ Systems\ Project/Subjective_Weighting/Travel_Decay_Analysis.xlsx", sheet = 1, col_names = TRUE)
Trip_Pub_Raw <- read_excel("/Users/gregoryforbes/Desktop/Stanford\ Career/Q7\ -\ Spring\ Quarter/CEE\ 224Z\ -\ Sustainable\ Urban\ Systems\ Project/Subjective_Weighting/Trip_Pub_Raw.xlsx", sheet = 1, col_names = TRUE)
Per_Pub_Raw <- read_excel("/Users/gregoryforbes/Desktop/Stanford\ Career/Q7\ -\ Spring\ Quarter/CEE\ 224Z\ -\ Sustainable\ Urban\ Systems\ Project/Subjective_Weighting/Per_Pub_Raw.xlsx", sheet = 1, col_names = TRUE)
HH_Pub_Raw <- read_excel("/Users/gregoryforbes/Desktop/Stanford\ Career/Q7\ -\ Spring\ Quarter/CEE\ 224Z\ -\ Sustainable\ Urban\ Systems\ Project/Subjective_Weighting/HH_Pub_Raw.xlsx", sheet = 1, col_names = TRUE)

##########

### Functions to clean the excel sheets by deleting the proper columns. However, first check the data that you need.

filterData <- function(){  
  
  ##########
  
  df_TDA <- Travel_Decay_Analysis %>%
    dplyr::select(tdcaseid, houseid, personid, cntyfips, city, o_locno)
  
  df_TPR <- Trip_Pub_Raw %>%
    dplyr::select(HOUSEID, PERSONID, TRVLCMIN, TRPTRANS, TDCASEID, WHYTO, WHYFROM, R_AGE, TRPMILES, TDAYDATE, WTTRDFIN)
  names(df_TPR) <- c("houseid", "personid", "trvlcmin", "trptrans", "tdcaseid", "whyto", "whyfrom", "r_age", "trpmiles", "tdaydate", "wttrdfin")
  
  df_PPR <- Per_Pub_Raw %>%
    dplyr::select(HOUSEID, PERSONID, EDUC, BIKE_DFR, BIKE_GKP, CONDNIGH, CONDPUB, CONDRIVE, CONDSPEC, CONDTAX, CONDTRAV, SCHTRN1, SCHTRN2, SCHTYP, WALK_DEF, WALK_GKQ, WORKER, OCCAT)
  names(df_PPR) <- c("houseid", "personid", "education", "bike_dfr", "bike_gkp", "condnigh", "condpub", "condrive", "condspec", "condtax", "condtrav", "schtrn1", "schtrn2", "schtyp", "walk_def", "walk_gkq", "worker", "occupation")
  
  df_HPR <- HH_Pub_Raw %>%
    dplyr::select("HOUSEID", "TRAVDAY", "PLACE", "HHFAMINC")
  names(df_HPR) <- c("houseid", "travday", "place", "household_income")
  
  ##########
  
  ### Next Step: Merge the resulting excel sheets.
  
  ##########
  
  df_TDA_TPR <- merge(df_TDA, df_TPR, by = "tdcaseid")
  df_TDA_TPR <- dplyr::select(df_TDA_TPR, -houseid.x)
  df_TDA_TPR <- dplyr::select(df_TDA_TPR, -personid.x)
  names(df_TDA_TPR) <-c("tdcaseid", "cntyfips", "city", "o_locno", "houseid", "personid", "trvlcmin", "trptrans", "whyto", "whyfrom", "r_age", "trpmiles", "tdaydate", "wttrdfin")
  
  df_TDA_TPR_PPR <- merge(df_TDA_TPR, df_PPR, by = c("houseid","personid"))
  
  df_final_all <- merge(df_TDA_TPR_PPR, df_HPR, by = "houseid")
  
  ##########
  
  return(df_final_all)
  
}

filterVar <- function(df_final){  
  
  ### Creation of "trptransfilt" filtered columns.
  
  ##########
  
  NTHS_num <- c(-9, -8, -7, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 97)
  
  NHTS_transMode <- c("Not ascertained", "I don't know", "I prefer not to answer", "Appropriate skip", "Walk",
                      "Bicycle", "Car", "SUV", "Van", "Pickup Truck", "Golf cart / Segway", "Motorcycle / Moped",
                      "RV (motor home, ATV, snowmobile)", "School bus", "Public or commuter bus",
                      "Paratransit / Dial-a-ride", "Private / Charter / Tour / Shuttle bus",
                      "City-to-city bus (Greyhound, Megabus)", "Amtrak / Commuter Rail",
                      "Subway / elevated / light rail / street car", "Taxi / limo (including Uber / Lyft)",
                      "Rental car (including Zipcar / Car2Go)", "Airplane", "Boat / ferry / water taxi",
                      "Something Else")
  
  CCF_num_trans <- c(99, 99, 99, 99, 1, 2, 4, 4, 4, 4, 99, 99, 99, 3, 3, 99, 99, 99, 99, 3, 4, 99, 99, 99, 99)
  
  trptransfiltTable <- data.frame(NTHS_num, NHTS_transMode, CCF_num_trans)
  
  trptransfiltration <- trptransfiltTable$CCF_num_trans
  names(trptransfiltration) <- trptransfiltTable$NTHS_num
  
  trptransfilt <- data.frame(matrix(NA, nrow = length(df_final$trptrans), ncol = 2))
  names(trptransfilt) <-c("tdcaseid","trptransfilt")
  
  trptransfilt[ , 2] <- unlist(lapply(df_final$trptrans, function(x) return(trptransfiltration[as.character(x)][[1]])))
  trptransfilt[ , 1] <- df_final$tdcaseid
  
  df_final <- merge(df_final, trptransfilt, by = "tdcaseid")
  
  ##########
  
  ### Creation of "Agefilt" Filtered Columns.
  
  ##########
  
  Age_num <- c(-10, 0, 4, 17, 24, 40, 65)
  NHTS_Age <- c("Other", "00 to 04 years old", "05 to 17 years old", "18 to 24 years old", "25 to 40 years old", "41 to 65 years old", "65+ years old")
  CCF_num_Age <- c(0, 1, 2, 3, 4, 5, 6)
  
  AgefiltTable <- data.frame(Age_num, NHTS_Age, CCF_num_Age)
  
  Agefiltration <- AgefiltTable$Age_num
  names(Agefiltration) <- AgefiltTable$CCF_num_Age
  
  Agefilt <- data.frame(matrix(NA, nrow = length(df_final$r_age), ncol = 2))
  names(Agefilt) <-c("tdcaseid","Agefilt")
  
  Agefilt[ , 2] <- unlist(lapply(df_final$r_age, function(x) return(max(CCF_num_Age[Age_num <= x]))))
  Agefilt[ , 1] <- df_final$tdcaseid
  
  Agefilt[Agefilt[, 2]==0, 2] = 99
  
  df_final <- merge(df_final, Agefilt, by = "tdcaseid")
  
  ##########
  
  ### Creation of "HHincomefilt" Filtered Columns.
  
  ##########
  
  HHincome_num <- c(-9, -8, -7, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  NHTS_HHincome <- c("Not ascertained", "I don't know", "I prefer not to answer", "Less than $10,000", "$10,000 to $14,999",
                     "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999",
                     "$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $199,999", "$200,000 or more")
  CCF_num_HHincome <- c(99, 99, 99, 1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4)
  
  HHincomefiltTable <- data.frame(HHincome_num, NHTS_HHincome, CCF_num_HHincome)
  
  HHincomefiltration <- HHincomefiltTable$HHincome_num
  names(HHincomefiltration) <- HHincomefiltTable$CCF_num_HHincome
  
  HHincomefilt<- data.frame(matrix(NA, nrow = length(df_final$household_income), ncol = 2))
  names(HHincomefilt) <-c("tdcaseid","HHincomefilt")
  
  HHincomefilt[ , 2] <- unlist(lapply(df_final$household_income, function(x) return(CCF_num_HHincome[HHincome_num == x])))
  HHincomefilt[ , 1] <- df_final$tdcaseid
  
  df_final <- merge(df_final, HHincomefilt, by = "tdcaseid")
  
  ##########
  
  ### Creation of "Occupfilt" Filtered Columns.
  
  ##########
  
  ### Most of the following values are -1 (an appropriate skip), thus it is probably best to avoid using factor.
  
  Occup_num <- c(-9, -8, -7, -1, 1, 2, 3, 4, 97)
  NHTS_Occup <- c("Not ascertained", "I don't know", "I prefer not to answer", "Appropriate skip",
                  "Sales or service", "Clerical or administrative support",
                  "Manufacturing, construction, maintenance, or farming",
                  "Professional, managerial or technical", "Something else")
  CCF_num_Occup <- c(99, 99, 99, 99, 1, 2, 3, 4, 99)
  
  OccupfiltTable <- data.frame(Occup_num, NHTS_Occup, CCF_num_Occup)
  
  Occupfiltration <- OccupfiltTable$Occup_num
  names(Occupfiltration) <- OccupfiltTable$CCF_num_Occup
  
  Occupfilt <- data.frame(matrix(NA, nrow = length(df_final$occupation), ncol = 2))
  names(Occupfilt) <-c("tdcaseid","Occupfilt")
  
  Occupfilt[ , 2] <- unlist(lapply(df_final$occupation, function(x) return(CCF_num_Occup[Occup_num == x])))
  Occupfilt[ , 1] <- df_final$tdcaseid
  
  df_final <- merge(df_final, Occupfilt, by = "tdcaseid")
  
  ##########
  
  ### Creation of "whytofilt" Filtered Columns.
  
  ##########
  
  whyto_num <- c(-9, -8, -7, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 97)
  NHTS_whytoReason <- c("Not ascertained", "I don't know", "I prefer not to answer", "Regular home activities (chores, sleep)", "Work from home (paid)",
                        "Work", "(01) Work-related meeting / trip", "Volunteer activities (not paid)", "(02) Drop off / pick up somone", "(03) Change type of transportation",
                        "(04) Attend school as a student", "Attend child care", "Attend adult care", "(05) Buy goods (groceries, clothes, appliances, gas)",
                        "(06) Buy services (dry cleaners, banking, service a car, pet care)", "(07) Buy meals (go out for a meal, snack, carry-out)",
                        "(08) Other general errands (post office, library)", "(09) Recreational activities (visit parks, movies, bars, museums)",
                        "Exercise (go for a jog, walk, walk the dog, go to the gym)", "Visit friends or relatives", "(10) Health care visit (medical, dental, therapy)",
                        "(11) Religious or other community activities", "Something else")
  CCF_num_whyto <- c(99, 99, 99, 99, 99, 99, 1, 99, 2, 3, 4, 99, 99, 5, 6, 7, 8, 9, 99, 99, 10, 11, 99)
  
  whytofiltTable <- data.frame(whyto_num, NHTS_whytoReason, CCF_num_whyto)
  
  whytofiltration <- whytofiltTable$whyto_num
  names(whytofiltration) <- whytofiltTable$CCF_num_whyto
  
  whytofilt <- data.frame(matrix(NA, nrow = length(df_final$whyto), ncol = 2))
  names(whytofilt) <-c("tdcaseid","whytofilt")
  
  whytofilt[ , 2] <- unlist(lapply(df_final$whyto, function(x) return(CCF_num_whyto[whyto_num == x])))
  whytofilt[ , 1] <- df_final$tdcaseid
  
  df_final <- merge(df_final, whytofilt, by = "tdcaseid")
  
  ##########
  
  ### Creation of "educfilt" Filtered Columns.
  
  ##########
  
  educ_num <- c(-8, -7, -1, 1, 2, 3, 4, 5)
  NHTS_educAttain <- c("I don't know", "I prefer not to answer", "Appropriate skip", "Less than a high school graduate",
                       "High school graduate or GED", "Some college or associate's degree", "Bachelor's degree",
                       "Graduate degree or professional degree")
  CCF_num_educ <- c(99, 99, 99, 1, 2, 3, 4, 5)
  
  educfiltTable <- data.frame(educ_num, NHTS_educAttain, CCF_num_educ)
  
  educfiltration <- educfiltTable$educ_num
  names(educfiltration) <- educfiltTable$CCF_num_educ
  
  educfilt <- data.frame(matrix(NA, nrow = length(df_final$education), ncol = 2))
  names(educfilt) <-c("tdcaseid","educfilt")
  
  educfilt[ , 2] <- unlist(lapply(df_final$education, function(x) return(CCF_num_educ[educ_num == x])))
  educfilt[ , 1] <- df_final$tdcaseid
  
  df_final <- merge(df_final, educfilt, by = "tdcaseid")
  
  ##########
  
  ### Creation of "workerfilt" Filtered Columns.
  
  ##########
  
  worker_num <- c(-9, -1, 1, 2)
  NHTS_workerAttain <- c("Not ascertained", "I don't know", "Yes - Worker", "No - Unemployed")
  CCF_num_worker <- c(99, 99, 1, 2)
  
  workerfiltTable <- data.frame(worker_num, NHTS_workerAttain, CCF_num_worker)
  
  workerfiltration <- workerfiltTable$worker_num
  names(workerfiltration) <- workerfiltTable$CCF_num_worker
  
  workerfilt <- data.frame(matrix(NA, nrow = length(df_final$worker), ncol = 2))
  names(workerfilt) <-c("tdcaseid","workerfilt")
  
  workerfilt[ , 2] <- unlist(lapply(df_final$worker, function(x) return(CCF_num_worker[worker_num == x])))
  workerfilt[ , 1] <- df_final$tdcaseid
  
  df_final <- merge(df_final, workerfilt, by = "tdcaseid")
  
  ##########
  
  ### Creation of "schtypfilt" Filtered Columns.
  
  ##########
  
  schtyp_num <- c(-9, -7, -1, 1, 2, 3)
  NHTS_schtyp <- c("Not ascertained", "I don't know", "Appropriate skip", "Public or private school", "Home schooled", "Not in school")
  CCF_num_schtyp <- c(99, 99, 99, 1, 2, 3)
  
  schtypfiltTable <- data.frame(schtyp_num, NHTS_schtyp, CCF_num_schtyp)
  
  schtypfiltration <- schtypfiltTable$schtyp_num
  names(schtypfiltration) <- schtypfiltTable$CCF_num_schtyp
  
  schtypfilt <- data.frame(matrix(NA, nrow = length(df_final$schtyp), ncol = 2))
  names(schtypfilt) <-c("tdcaseid","schtypfilt")
  
  schtypfilt[ , 2] <- unlist(lapply(df_final$schtyp, function(x) return(CCF_num_schtyp[schtyp_num == x])))
  schtypfilt[ , 1] <- df_final$tdcaseid
  
  df_final <- merge(df_final, schtypfilt, by = "tdcaseid")
  
  ##########
  
  ### Creation of "schtrnfilt" Filtered Columns.
  
  ##########
  
  schtrn_num <- c(-9, -8, -7, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 97)
  NHTS_schtrn <- c("Not ascertained", "I don't know", "I prefer not to answer", "Appropriate skip", "Walk",
                   "Bicycle", "Car", "SUV", "Van", "Pickup truck", "Golf cart / Segway", "Motorcyle / Moped",
                   "RV (motor home, ATV, snowmobile)", "School bus", "Public or Commuter bus", "Paratransit / Dial-a-ride",
                   "Private / Charter / Tour / Shuttle bus", "City-to-city bus (Greyhound, Megabus)",
                   "Amtrak / Commuter rail", "Subway / Elevated / Light rail / Street car",
                   "Taxi / Limo / (including Uber / Lyft)", "Rental car (including Zipcar / Car2Go)",
                   "Airplane", "Boat / Ferry / Water taxi", "Something Else")
  CCF_num_schtrn <- c(99, 99, 99, 99, 1, 2, 4, 4, 4, 4, 99, 99, 99, 3, 3, 99, 99, 99, 99, 3, 4, 99, 99, 99, 99)
  
  schtrnfiltTable <- data.frame(schtrn_num, NHTS_schtrn, CCF_num_schtrn)
  
  schtrnfiltration <- schtrnfiltTable$schtrn_num
  names(schtrnfiltration) <- schtrnfiltTable$CCF_num_schtrn
  
  schtrn1filt <- data.frame(matrix(NA, nrow = length(df_final$schtrn1), ncol = 2))
  names(schtrn1filt) <-c("tdcaseid","schtrn1filt")
  
  schtrn1filt[ , 2] <- unlist(lapply(df_final$schtrn1, function(x) return(CCF_num_schtrn[schtrn_num == x])))
  schtrn1filt[ , 1] <- df_final$tdcaseid
  
  schtrn2filt <- data.frame(matrix(NA, nrow = length(df_final$schtrn2), ncol = 2))
  names(schtrn2filt) <-c("tdcaseid","schtrn2filt")
  
  schtrn2filt[ , 2] <- unlist(lapply(df_final$schtrn2, function(x) return(CCF_num_schtrn[schtrn_num == x])))
  schtrn2filt[ , 1] <- df_final$tdcaseid
  
  df_final <- merge(df_final, schtrn1filt, by = "tdcaseid")
  df_final <- merge(df_final, schtrn2filt, by = "tdcaseid")
  
  ##########
  
  return(df_final)
  
}

##########

### Operations to filter by location.

df_final_all <- filterData()

df_final <- df_final_all[df_final_all$city == "STOCKTON", ]
# df_final <- df_final_all[df_final_all$cntyfips == 77 , ]
# df_final <- df_final_all

df_final <- filterVar(df_final)
View(df_final)

write.csv(df_final, file = "/Users/gregoryforbes/Desktop/df_final.csv")