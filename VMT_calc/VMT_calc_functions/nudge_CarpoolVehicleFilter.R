### Nudge: Peforming the carpooling and vehicle vs. other transit mode nudge.

NHTS_carpoolVehicleFilter <- function(NHTS_df_final){
  
  # The feature to use includes the following: "numontrp".
  
  # All "trpmiles"
  
  ratio_cVF <- sum(NHTS_df_final[NHTS_df_final$trptransfilt == 4, "wttrdfin"] * NHTS_df_final[NHTS_df_final$trptransfilt == 4, "trpmiles"]) / 
               sum(NHTS_df_final[, "wttrdfin"] * NHTS_df_final[, "numontrp"] * NHTS_df_final[, "trpmiles"])
  
  ### This is just a test to see the average amount of people per ride: test <- sum(NHTS_df_final[, "numontrp"]) / nrow(NHTS_df_final)
  
  # "trpmiles" < 2 miles
  
  ratio_cVF_less2 <- sum(NHTS_df_final[NHTS_df_final$trptransfilt == 4 & NHTS_df_final$trpmiles < 2, "wttrdfin"] *
                         NHTS_df_final[NHTS_df_final$trptransfilt == 4 & NHTS_df_final$trpmiles < 2, "trpmiles"]) / 
                     sum(NHTS_df_final[NHTS_df_final$trpmiles < 2, "wttrdfin"] *
                         NHTS_df_final[NHTS_df_final$trpmiles < 2, "numontrp"] *
                         NHTS_df_final[NHTS_df_final$trpmiles < 2, "trpmiles"])

  # "trpmiles" > 2 miles & < 5 miles
  
  ratio_cVF_2to5 <- sum(NHTS_df_final[NHTS_df_final$trptransfilt == 4 & NHTS_df_final$trpmiles >= 2 & NHTS_df_final$trpmiles < 5, "wttrdfin"] *
                        NHTS_df_final[NHTS_df_final$trptransfilt == 4 & NHTS_df_final$trpmiles >= 2 & NHTS_df_final$trpmiles < 5, "trpmiles"]) / 
                    sum(NHTS_df_final[NHTS_df_final$trpmiles >= 2 & NHTS_df_final$trpmiles < 5, "wttrdfin"] *
                        NHTS_df_final[NHTS_df_final$trpmiles >= 2 & NHTS_df_final$trpmiles < 5, "numontrp"] *
                        NHTS_df_final[NHTS_df_final$trpmiles >= 2 & NHTS_df_final$trpmiles < 5, "trpmiles"])
  
  # "trpmiles" > 5 miles & < 10 miles

  ratio_cVF_5to10 <- sum(NHTS_df_final[NHTS_df_final$trptransfilt == 4 & NHTS_df_final$trpmiles >= 5 & NHTS_df_final$trpmiles < 10, "wttrdfin"] *
                         NHTS_df_final[NHTS_df_final$trptransfilt == 4 & NHTS_df_final$trpmiles >= 5 & NHTS_df_final$trpmiles < 10, "trpmiles"]) / 
                     sum(NHTS_df_final[NHTS_df_final$trpmiles >= 5 & NHTS_df_final$trpmiles < 10, "wttrdfin"] *
                         NHTS_df_final[NHTS_df_final$trpmiles >= 5 & NHTS_df_final$trpmiles < 10, "numontrp"] *
                         NHTS_df_final[NHTS_df_final$trpmiles >= 5 & NHTS_df_final$trpmiles < 10, "trpmiles"])
    
  # "trpmiles" > 10 miles & < 50 miles
  
  ratio_cVF_10to50 <- sum(NHTS_df_final[NHTS_df_final$trptransfilt == 4 & NHTS_df_final$trpmiles >= 10 & NHTS_df_final$trpmiles < 50, "wttrdfin"] *
                          NHTS_df_final[NHTS_df_final$trptransfilt == 4 & NHTS_df_final$trpmiles >= 10 & NHTS_df_final$trpmiles < 50, "trpmiles"]) / 
                      sum(NHTS_df_final[NHTS_df_final$trpmiles >= 10 & NHTS_df_final$trpmiles < 50, "wttrdfin"] *
                          NHTS_df_final[NHTS_df_final$trpmiles >= 10 & NHTS_df_final$trpmiles < 50, "numontrp"] *
                          NHTS_df_final[NHTS_df_final$trpmiles >= 10 & NHTS_df_final$trpmiles < 50, "trpmiles"])
  
  # "trpmiles" > 50 miles
  
  ratio_cVF_50plus <- sum(NHTS_df_final[NHTS_df_final$trptransfilt == 4 & NHTS_df_final$trpmiles > 50, "wttrdfin"] *
                          NHTS_df_final[NHTS_df_final$trptransfilt == 4 & NHTS_df_final$trpmiles > 50, "trpmiles"]) / 
                      sum(NHTS_df_final[NHTS_df_final$trpmiles > 50, "wttrdfin"] *
                          NHTS_df_final[NHTS_df_final$trpmiles > 50, "numontrp"] *
                          NHTS_df_final[NHTS_df_final$trpmiles > 50, "trpmiles"])
  
  return(ratio_cVF)
  
}