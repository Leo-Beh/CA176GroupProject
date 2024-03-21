#reading the excel file
library(readxl)
path <- "/Users/samfitz/Desktop/Copy of companies.xlsx"
data <- read_excel(path)
str(data)
head(data)

#creating a dataframe with the specified columns
myDF <- data.frame(
  total_reviews = data[,c("Total_reviews")],
  avg_salary = data[c("Avg_salary")],
  interviews_taken = data[c("Interviews_taken")],
  total_jobs_available = data[c("Total_jobs_available")],
  total_benifits = data[c("Total_benefits")]
)

#checking for the letter k. Removes it if 'k' is present and multiplies it by-
#-one thousand, stays the same otherwise
data$Total_reviews <- ifelse(grepl("k", data$Total_reviews),
                             as.numeric(sub("k", "", data$Total_reviews)) * 1000,
                             as.numeric(data$Total_reviews))

data$Avg_salary <- ifelse(grepl("k", data$Avg_salary),
                          as.numeric(sub("k", "", data$Avg_salary)) * 1000,
                          as.numeric(data$Avg_salary))

data$Interviews_taken <- ifelse(grepl("k", data$Interviews_taken),
                                as.numeric(sub("k", "", data$Interviews_taken)) * 1000,
                                as.numeric(data$Interviews_taken))

data$Total_jobs_available <- ifelse(grepl("k", data$Total_jobs_available),
                                    as.numeric(sub("k", "", data$Total_jobs_available)) * 1000,
                                    as.numeric(data$Total_jobs_available))

data$Total_benefits <- ifelse(grepl("k", data$Total_benefits),
                              as.numeric(sub("k", "", data$Total_benefits)) * 1000,
                              as.numeric(data$Total_benefits))

#specifying the columns in the dataframe for the function
columns <- c("Total_reviews", "Avg_salary", "Interviews_taken", "Total_jobs_available", "Total_benefits")

#unformat values function
unformatted.value <- function(input_value){
  numeric_value <- gsub('[[:punct:] ]+',' ',input_value)
}

#new columns function
new_cols <-function(data, columns){
  new_data <- data
  for(columns in columns){
    new_col_name <- paste0(columns, "_")
    ###
  }
  return(new_data)
}

#running the function
new_data <- new_cols(myDF, columns)

first.word <- function(input){
  parts <- strsplit(input, "|", fixed = TRUE)[[1]] #splitting on the bars and taking the first value
  category <- trimws(parts[1]) #trimws <- trims leading whitespace
  return(category)
}

data$Industry <- sapply(data$Description, first.word)
# Test

