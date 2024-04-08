library(readxl)
library(dplyr)

companies_df <- read_excel("companies.xlsx")

top_companies <- function(dataframe) {
  
  # filter, order and limiting the companies
  filtered_companies_df <- dataframe %>%
   filter(grepl("Job Security", Highly_rated_for) & grepl("Work Life Balance", Highly_rated_for)) %>%
#   filter(Highly_rated_for == "Job Security, Work Life Balance") %>%
    arrange(desc(Ratings)) %>%
    slice(1:5)
  
  return (filtered_companies_df)
}

View(top_companies(companies_df))
