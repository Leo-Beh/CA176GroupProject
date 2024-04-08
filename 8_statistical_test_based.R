library(readxl)
library(dplyr)


# Load data ------------------------------------
companies_df <- read_excel("companies.xlsx")


# Creating function ----------------------------
calculate_stats <- function(element) {
  # Calculate the mean, variance and sd of the elements, excluding NA values
  element_mean <- mean(element, na.rm = TRUE)
  element_var <- var(element, na.rm = TRUE)
  element_sd <- sd(element, na.rm = TRUE)
  
  return(list(mean = element_mean, variance = element_var, standard_deviation = element_sd))
}


# Running the function -------------------------

# Saving the calculated results into variables
mean_salary <- calculate_stats(underscore_unformatted_companies_df$Avg_salary_)
st_ratings <- calculate_stats(underscore_unformatted_companies_df$Ratings)

# Printing out the results rounded to two decimal places
print(paste("Mean salary: ", round(mean_salary$mean, 2)))
print(paste("Standard deviation of Rating:", round(st_ratings$standard_deviation, 2)))

