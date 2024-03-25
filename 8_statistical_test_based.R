library(readxl)
library("dplyr")

companies_df <- read_excel("companies.xlsx")


print(unformatted_companies_df)
str(unformatted_companies_df)

calculate_stats <- function(element) {
  # Calculate the mean, variance and sd of the elements, excluding NA values
  element_mean <- mean(element, na.rm = TRUE)
  element_var <- var(element, na.rm = TRUE)
  element_sd <- sd(element, na.rm = TRUE)
  
  return(list(mean = element_mean, variance = element_var, standard_deviation = element_sd))
}

# Setting the variables 
mean_salary <- calculate_stats(unformatted_companies_df$Avg_salary)
st_ratings <- calculate_stats(unformatted_companies_df$Ratings)

# Printing out the results
print(paste("Mean salary: ", round(mean_salary$mean, 2)))
print(paste("Standard deviation of Rating:", round(st_ratings$standard_deviation, 2)))
