library(readxl)
library("dplyr")

companies_df <- read_excel("companies.xlsx")

unformat_numeric <- function(input) {
  
  # Empty vector to store formatted values 
  # formatted_input <- c()
  formatted_input <- numeric(length(input))
  
  # To loop through every element in the column
  for (i in seq_along(input)){
    
    # Setting the current element using the index i
    element <- input[i]
    
    # Checking if the values in the columns if Null
    if (element == "--"){
      formatted_input[i] <- NA
      # Checking if the values in the contains 'k'
    } else if (grepl("k", element, fixed = TRUE)) {
      # Remove 'k' and multiply by 1000
      unformat <- gsub("k", "", element)
      formatted_input[i] <- as.numeric(unformat) * 1000
    } else {
      # If 'k' is not present, just convert to numeric
      formatted_input[i] <- as.numeric(element)
    }
  }
  return (formatted_input)
}

unformatted_companies_df <- companies_df %>%
  mutate(Avg_salary = unformat_numeric(Avg_salary),
         Interviews_taken = unformat_numeric(Interviews_taken),
         Total_jobs_available = unformat_numeric(Total_jobs_available),
         Total_reviews = unformat_numeric(Total_reviews),
         Total_benefits = unformat_numeric(Total_benefits)
  )

print(unformatted_companies_df)
str(unformatted_companies_df)

calculate_stats <- function(element) {
  # Calculate the mean, variance and sd of the elements, excluding NA values
  element_mean <- mean(element, na.rm = TRUE)
  element_var <- var(element, na.rm = TRUE)
  element_sd <- sd(element, na.rm = TRUE)
  
  return(list(mean = element_mean, variance = element_var, standard_deviation = element_sd))
}

mean_salary <- calculate_stats(unformatted_companies_df$Avg_salary)
st_ratings <- calculate_stats(unformatted_companies_df$Ratings)
print(paste("Mean salary: ", round(mean_salary$mean, 2)))
print(paste("Standard deviation of Rating:", round(st_ratings$standard_deviation, 2)))
