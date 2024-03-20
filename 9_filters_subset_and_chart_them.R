library(readxl)
library("dplyr")
library(ggplot2)


# Reading the raw company data
companies_df <- read_excel("companies.xlsx")


# filtering and limiting the company df
filtered_companies_df <- companies_df %>%
  filter(Highly_rated_for == "Job Security")%>%  
  mutate(Avg_salary = as.numeric(gsub("k", "", Avg_salary)) * 1000)%>%  # Convert the k into 1000
  slice(1:100)


# Creating a scatter plot to visualize the relationship between company rating and average salary
ggplot(data = filtered_companies_df, aes(x = Avg_salary, y = Ratings)) +
  geom_point(aes(colour = Ratings), shape = 16, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_color_gradient(low = "blue", high = "green") +
  theme_bw() +
  
  labs(title = "Scatter Plot of Avg Salary vs Ratings",
       x = "Average Salary",
       y = "Ratings") +
  
  scale_x_log10(labels = scales::comma) +  # Scale the x axis 
  theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
        legend.position = "right",  # Adjust legend position
        axis.text = element_text(size = 12),  # Adjust text size
        axis.title = element_text(size = 13, face = "bold"))  # Adjust title size and font 

# Conclusion we see a very slight negative correlation between a company's rating and the average salary 