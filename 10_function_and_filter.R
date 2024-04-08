library(readxl)
library(dplyr)


# Load data ---------------------------
companies_df <- read_excel("companies.xlsx")


# Creating function -------------------
top_companies <- function(dataframe) {
  # filter, order and limiting the companies
  
  filtered_companies_df <- dataframe %>%
    filter(Highly_rated_for == "Job Security")%>%                               # Filter the data base uniquely on "Job security"
    arrange(desc(Ratings)) %>%                                                  # Arranging the "Ratings" column from highest to lowest
    slice(1:5)                                                                  # Selecting only the top 5 results
  
  return (filtered_companies_df)                  
}


# View filtered data -----------------
View(top_companies(companies_df))
