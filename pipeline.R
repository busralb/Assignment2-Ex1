# Load file
data <- read.csv("Wholesale_customers_data.csv")


calculate_z_scores <- function(data) {
  # Split the data by region
  data_by_region <- split(data, data$Region)
  
  # Calculate the mean and standard deviation for each region
  region_stats <- lapply(data_by_region, function(region_data) {
    col_stats <- apply(region_data[, 3:8], 2, function(x) {
      list(mean = mean(x), sd = sd(x))
    })
    return(col_stats)
  })
  
  # Calculate Z-scores for each customer
  z_scores <- lapply(data_by_region, function(region_data) {
    region_stats <- apply(region_data[, 3:8], 2, function(x) {
      list(mean = mean(x), sd = sd(x))
    })
    customer_z_scores <- sweep(region_data[, 3:8], 2, unlist(region_stats), "-")
    customer_z_scores <- sweep(customer_z_scores, 2, unlist(region_stats), "/")
    return(customer_z_scores)
  })
  
  # Combine Z-scores for all customers
  all_z_scores <- do.call(rbind, z_scores)
  
  return(all_z_scores)
}

z_scores <- calculate_z_scores(data)
print(round(z_scores, 4))
