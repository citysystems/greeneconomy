# safegraphplaces_ca_edit

safegraphplaces_ca <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/safegraphplaces_ca.csv", header=TRUE, stringsAsFactors = FALSE)

X_col = data.frame(matrix(0, nrow = nrow(safegraphplaces_ca), ncol = 1))

start_val <- 17245
X_col[,1] <- as.numeric(rownames(X_col)) + start_val

sub_category_col = data.frame(matrix(NA, nrow = nrow(safegraphplaces_ca), ncol = 1))
naics_code_col = data.frame(matrix(NA, nrow = nrow(safegraphplaces_ca), ncol = 1))
open_hours_col = data.frame(matrix(NA, nrow = nrow(safegraphplaces_ca), ncol = 1))
polygon_wkt_col = data.frame(matrix(NA, nrow = nrow(safegraphplaces_ca), ncol = 1))
polygon_class_col = data.frame(matrix(NA, nrow = nrow(safegraphplaces_ca), ncol = 1))
phone_number_col = data.frame(matrix(NA, nrow = nrow(safegraphplaces_ca), ncol = 1))

safegraphplaces_ca_edit <- cbind(X_col, safegraphplaces_ca[, c("sgpid", "psgpid", "sgbid", "poiName", "brands", "topCat")], sub_category_col,
                                 naics_code_col, safegraphplaces_ca[, c("latitude", "longitude", "address", "city", "state", "zip_code")], open_hours_col, polygon_wkt_col, polygon_class_col, phone_number_col)

colnames(safegraphplaces_ca_edit) <- c("X", "safegraph_place_id", "parent_safegraph_place_id", "safegraph_brand_ids",
                "location_name", "brands", "top_category", "sub_category", "naics_code", "latitude",
                "longitude", "street_address", "city", "state", "zip_code", "open_hours", "polygon_wkt",
                "polygon_class", "phone_number")

write.csv(safegraphplaces_ca_edit, file = "C:/Users/Derek/Desktop/safegraphplaces/safegraphplaces_ca_edit.csv", row.names = FALSE)
