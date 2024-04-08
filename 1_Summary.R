library(readxl)
library(dplyr)


# Creating function --------------------------
frequency_count <- function(column){
  # Calculates the frequency count of the elements in a column
  
  attributes_split <- strsplit(column, ", ")                                    # Separating the multivalued attributes uniquely by comma and returning a list
  attributes_vector <- unlist(attributes_split)                                 # Unlist the attributes, into simple atomic vector
  freq_count <- table(attributes_vector)                                        # Creating a table of the unlisted attributes
  names(freq_count)[names(freq_count) == "--"] <- "NA"                          # Converting the empty attributes "__" into "NA"
  
  aspect_df <- data.frame(                                                      # Converting the tabel into a dataframe                                                     
    critically_rated_freq_count
  )
  
  colnames(aspect_df) <- c('Aspect', 'Frequency')
  
  return(aspect_df)  
}


# Running the function -----------------------
aspect_and_frquency <- frequency_count(underscore_unformatted_companies_df$Critically_rated_for)
aspect_and_frquency


# Saving the dataframe -----------------------
write.csv(aspect_and_frquency)


# Part 2: Calculate proportions:
# Creating function --------------------------
proportion_cal <- function(aspect_and_frquency){
  # Input a dataframe of aspect and frequency and will calculate the proportions
  
  total_frequency <- sum(aspect_and_frquency$Frequency)                         # Calculate the total frequency 
  
  proportions <- c()                                                            # Create empty vector to store the proportions
  for (i in 1:8){                                                               # Calculate and appending the proportions into a vector
    proportions <- append(proportions, aspect_and_frquency$Frequency[i] / total_frequency)
  }
  
  aspect_and_frquency$Proportions <- proportions                                # Add the proportions into the dataframe as a new column
  
  return(aspect_and_frquency)
}

# Running the function -----------------------
proportion_cal(aspect_and_frquency)