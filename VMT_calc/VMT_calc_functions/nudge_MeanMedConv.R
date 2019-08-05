### Nudge: Peforming the median to mean converison.

nudge_MeanMedConv <- function(NHTS_df_final){

  # NHTS_df_final <- NHTS_df_final[NHTS_df_final$trptransfilt == 3 && NHTS_df_final$trptransfilt == 4, ]

  NHTS_wghtAvg <- sum(NHTS_df_final$trpmiles * NHTS_df_final$wttrdfin) / sum(NHTS_df_final$wttrdfin)
  NHTS_medVal <- as.numeric(median(rep(NHTS_df_final$trpmiles, round(NHTS_df_final$wttrdfin))))
  NHTS_MeanMedConv <- NHTS_wghtAvg / NHTS_medVal # Need to filter by "trptransmode" and use of variables for the network distance (e.g., TRPMILES or VMT_MILES).
  
  return(NHTS_MeanMedConv)
  
}