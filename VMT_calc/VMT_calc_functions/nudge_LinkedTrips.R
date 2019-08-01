### Nudge: Performing the linked trips conversion factor.

nudge_LinkedTrips <- function(NHTS_df_final){

  # NHTS_df_linkedTrips <- NHTS_df_final[NHTS_df_final$trptransfilt == 3 && NHTS_df_final$trptransfilt == 4, ]
  NHTS_df_linkedTrips <- NHTS_df_final
  
  ##########
  
  # Manual removal of outlier: 304216040202 (TDCASEID) & 322.673 (TRPMILES)
  
  NHTS_df_linkedTrips <- subset(NHTS_df_linkedTrips, tdcaseid != 304216040202)
  
  ##########
  
  key_num <- c(1, 2, 3, 99)
  key_name <- c("Home", "Work", "SG_Dest", "Other")
  key_matrix <- data.frame(key_num, key_name)

  from = length( unique(NHTS_df_linkedTrips$whytofilt) )
  to = length( unique(NHTS_df_linkedTrips$whyfromfilt) )

  matrix_tripCount <- data.frame(matrix(data = NA, nrow = to, ncol = from))
  colnames(matrix_tripCount) <- key_name
  rownames(matrix_tripCount) <- key_name

  matrix_tripVMT <- data.frame(matrix(data = NA, nrow = to, ncol = from))
  colnames(matrix_tripVMT) <- key_name
  rownames(matrix_tripVMT) <- key_name

  for(linkedTo in sort(unique(NHTS_df_linkedTrips$whytofilt))){
  
    for(linkedFrom in sort(unique(NHTS_df_linkedTrips$whyfromfilt))){
    
      row_name = as.character(key_matrix[key_num == linkedTo, 2])
      col_name = as.character(key_matrix[key_num == linkedFrom, 2])
      matrix_tripCount[row_name, col_name] <- nrow(NHTS_df_linkedTrips[NHTS_df_linkedTrips$whytofilt == linkedTo & NHTS_df_linkedTrips$whyfromfilt == linkedFrom, ])
      matrix_tripVMT[row_name, col_name] <- sum(NHTS_df_linkedTrips[NHTS_df_linkedTrips$whytofilt == linkedTo & NHTS_df_linkedTrips$whyfromfilt == linkedFrom, ]$trpmiles)
      
    }
    
  }
  
  VMT_HomeAmenity_Avg <- 2 * (matrix_tripVMT["Home" ,"SG_Dest"] + matrix_tripVMT["SG_Dest", "Home"]) / (matrix_tripCount["Home" ,"SG_Dest"] + matrix_tripCount["SG_Dest", "Home"])
  model_SGmodel <-  2 * matrix_tripVMT["SG_Dest", "Home"] + (matrix_tripCount["SG_Dest", "SG_Dest"] * VMT_HomeAmenity_Avg)
  # model_SGmodel <- matrix_tripVMT["Home" ,"SG_Dest"] + matrix_tripVMT["SG_Dest", "Home"] + (matrix_tripCount["SG_Dest", "SG_Dest"] * VMT_HomeAmenity_Avg)
  model_realEst <- matrix_tripVMT["Home" ,"SG_Dest"] + matrix_tripVMT["SG_Dest", "Home"] + matrix_tripVMT["SG_Dest", "SG_Dest"]
  NHTS_LinkedTripsConv <- model_realEst / model_SGmodel
  
  return(NHTS_LinkedTripsConv)
  
}