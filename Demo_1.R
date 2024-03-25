library(readxl)
library("dplyr")

attributes_split <- strsplit(companies_df$Critically_rated_for, ", ")
attributes_vector <- unlist(attributes_split)

critically_rated_freq_count <- table(attributes_vector)

class(critically_rated_freq_count)

names(critically_rated_freq_count)[names(critically_rated_freq_count) == "--"] <- "NA"

critically_rated_freq_count

total_observations <- sum(critically_rated_freq_count)

proportions <- critically_rated_freq_count / total_observations

result_df <- data.frame(
  Critically_Rated_Aspects = names(proportions),
  Critically_Rated_Proportions = as.numeric(proportions))

result_df

proportions
