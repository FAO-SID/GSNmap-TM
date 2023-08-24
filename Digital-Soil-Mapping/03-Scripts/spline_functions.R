# Function to apply mpspline to a single soil property and depth range
library(purrr)
apply_mpspline <- function(df, property, depth_range) {
  # Prepare input data for mpspline function
  input_data <- df[, c("ProfID", "top", "bottom", property)]
  input_data <- na.omit(input_data)
  colnames(input_data) <- c("SID", "UD", "LD", "VAL")
  
  # Apply mpspline function
  mpspline_result <- mpspline(obj = input_data, 
                              var_name = "VAL", 
                              d = depth_range,
                              vlow = min(input_data$VAL), 
                              vhigh = max(input_data$VAL))
  temp <- NULL
  for (i in names(mpspline_result)){
    t2 <- cbind(mpspline_result[[i]]$SID,mpspline_result[[i]]$est_dcm[1:(length(depth_range)-1)])
    temp <- rbind(temp,t2)
  }
  temp <- as.data.frame(temp)
  x <- str_split(row.names(temp), "\\.", simplify = TRUE)[,1]
  temp$name <- x
  names(temp) <- c("ProfID", "value", "name")
  temp$value <- as.numeric(temp$value)
  temp <- group_by(temp, ProfID)
  
  n <- length(depth_range)
  coln <- NULL
  for(layer in 2:n){
    coln <- append(coln, paste0(property,"_", depth_range[layer-1], "_", depth_range[layer]))
  }
  temp <- pivot_wider(temp, id_cols = "ProfID", values_from = "value", names_from = name)
  names(temp) <- c("ProfID", coln)
  return(temp)
}

# Function to apply mpspline to all soil properties and depth ranges
apply_mpspline_all <- function(df, properties, depth_range) {
  final_df <- data.frame(ProfID = df$ProfID)
  for (property in properties) {
    if(any(!is.na(df[,property]))){
    x <- apply_mpspline(df, property, depth_range)
    final_df <- left_join(final_df, x)}
  }
  final_df <- unique(final_df)
  return(final_df)
}
