# Function to apply mpspline to a single soil property and depth range
apply_mpspline <- function(df, property, depth_range) {
  # Prepare input data for mpspline function
  input_data <- df[, c("ProfID", "top", "bottom", property)]
  colnames(input_data) <- c("SID", "UD", "LD", "VAL")
  
  # Apply mpspline function
  mpspline_result <- mpspline(obj = input_data, 
                              var_name = "VAL", 
                              d = depth_range,
                              vlow = min(input_data$VAL), 
                              vhigh = max(input_data$VAL))
  
  # Extract predicted values for the depth range
  predicted_values <- t(unique(as.data.frame(sapply(mpspline_result, function(x) x$est_dcm))))
  predicted_values <- as.data.frame(predicted_values)
  predicted_values$ProfID <- sub(x = rownames(predicted_values), pattern = "X", replacement = "")
  class(predicted_values$ProfID) <- class(df$ProfID)
  
  # Create a new dataframe with the interpolated values
  result_df <- unique(data.frame(ProfID = df$ProfID))
  result_df <- left_join(result_df, predicted_values)
  n <- length(depth_range)
  coln <- NULL
  for(layer in 2:n){
    coln <- append(coln, paste0(property,"_", depth_range[layer-1], "_", depth_range[layer]))
  }
  names(result_df)[-1] <- coln
  return(result_df)
}

# Function to apply mpspline to all soil properties and depth ranges
apply_mpspline_all <- function(df, properties, depth_range) {
  final_df <- data.frame(ProfID = df$ProfID)
  for (property in properties) {
    final_df <- left_join(final_df, apply_mpspline(df, property, depth_range))
  }
  final_df <- unique(final_df)
  return(final_df)
}
