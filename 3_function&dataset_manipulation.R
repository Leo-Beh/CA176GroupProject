#reading the file
library(readxl)
library(dplyr)


unformat_numeric_df <- function(df, column_names) {
  # Loop through every given column
  for (col_name in column_names) {
    # Empty vector to store formatted value
    formatted_input <- numeric(nrow(df))
    
    # Loop through every element in the column
    for (i in seq_along(df[[col_name]])) {
      
      # Setting the current element using the index i
      element <- df[[col_name]][i]
      
      # Checking and unformatting the value k
      if (element == "--") {
        formatted_input[i] <- NA
      } else if (grepl("k", element, fixed = TRUE)) {
        unformat <- gsub("k", "", element)
        formatted_input[i] <- as.numeric(unformat) * 1000
      } else {
        formatted_input[i] <- as.numeric(element)
      }
    }
    
    # Adding unformatted values into dataframe as a new column
    new_col_name <- paste0(col_name, "_")
    df[[new_col_name]] <- formatted_input
  }
  
  return(df)
}

underscore_unformatted_companies_df <- unformat_numeric_df(companies_df, c("Avg_salary", "Interviews_taken", "Total_jobs_available", "Total_reviews", "Total_benefits"))

View(underscore_unformatted_companies_df)

first.word <- function(input){
  parts <- strsplit(input, "|", fixed = TRUE)[[1]] #splitting on the bars and taking the first value
  category <- trimws(parts[1]) #trimws <- trims leading whitespace
  return(category)
}


underscore_unformatted_companies_df$Industry <- sapply(underscore_unformatted_companies_df$Description, first.word)
View(underscore_unformatted_companies_df)
#saving the file as binary data
save(list = ls(all=T), file = "Q3_test.R")