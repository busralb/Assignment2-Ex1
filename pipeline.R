data <- read.csv("Wholesale_customers_data.csv")

calculate_z_scores <- function(data, outlier_threshold = NULL) {
  # Calculate the mean and standard deviation for each column
  column_stats <- apply(data[, 3:8], 2, function(x) {
    mean_val <- mean(x, na.rm = TRUE)
    sd_val <- sd(x, na.rm = TRUE)
    if (sd_val == 0) {
      # Avoid division by zero when standard deviation is 0
      z_score <- 0
    } else {
      z_score <- (x - mean_val) / sd_val
    }
    return(z_score)
  })
  
  if (!is.null(outlier_threshold)) {
    # Identify and replace values exceeding the threshold with NA
    outliers <- abs(column_stats) > outlier_threshold
    column_stats[outliers] <- NA
  }
  
  return(column_stats)
}

# Call the function with an outlier threshold
z_scores <- calculate_z_scores(data, outlier_threshold = 2)
print(round(z_scores, 4))

