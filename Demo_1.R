library(readxl)
library(dplyr)


# Separating the multivalued attributes uniquely by comma and returning a list
attributes_split <- strsplit(companies_df$Critically_rated_for, ", ")


# Unlist the attributes, into simple atomic vector
attributes_vector <- unlist(attributes_split)


# Creating a table of the unlisted attributes
critically_rated_freq_count <- table(attributes_vector)


# converting the empty attributes "__" into "NA"
names(critically_rated_freq_count)[names(critically_rated_freq_count) == "--"] <- "NA"


# Calculating the proportions of each unique attributes
total_observations <- sum(critically_rated_freq_count)
proportions <- critically_rated_freq_count / total_observations


# Converting the proportions table into a data.frame
result_df <- data.frame(
  Critically_Rated_Aspects = names(proportions),
  Critically_Rated_Proportions = as.numeric(proportions))

aspect_df <- data.frame(
  critically_rated_freq_count
)

colnames(aspect_df) <- c('Aspect', 'Frequency')
aspect_df

View(proportions)