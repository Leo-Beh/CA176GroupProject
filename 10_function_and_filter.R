library(readxl)
library(dplyr)


# Load data ---------------------------
companies_df <- read_excel("companies.xlsx")


# Creating function -------------------
top_companies <- function(dataframe) {
  
  # filter, order and limiting the companies
  filtered_companies_df <- dataframe %>%
    filter(Highly_rated_for == "Job Security")%>%
    arrange(desc(Ratings)) %>%
    slice(1:5)
  
  return (filtered_companies_df)
}


# View filtered data -----------------
View(top_companies(companies_df))
