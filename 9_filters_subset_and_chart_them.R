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
  geom_smooth(method = "gam", color = "black", se = TRUE, size = 0.7) +
  scale_color_gradient(low = "blue", high = "green") +
  theme_bw() +
  
  labs(title = "Scatter Plot of Avg Salary vs Ratings",
       subtitle = "For comapnies only rated for Job Security",
       x = "Average Salary",
       y = "Ratings",
       caption = "A very weak correlation between average salary \n and rating for companies who are only highly rated for Job security") +
  
#  scale_x_log10(labels = scales::comma) + # Scale the x axis
  scale_x_continuous(labels = scales::comma,
                     # breaks = scales::pretty_breaks(n = ),
                     # breaks = function(x) 10^pretty(log10(range(x, finite = TRUE))),
                     trans = "log10") +
  scale_y_continuous(limits = c(2.5,4.5),
                     breaks = 2.5:4.5,
                     labels = c("2 Bad", "3 Fair", "4 Good")) +
  
  theme(plot.title = element_text(hjust = 0.5, face = "bold.italic"),# Center the plot title
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        legend.position = "right",  # Adjust legend position
        axis.text = element_text(size = 10, face = "italic"),  # Adjust text size
        axis.title = element_text(size = 12, face = "bold.italic"))  # Adjust title size and font 

# Conclusion we see a very slight negative correlation between a company's rating and the average salary
# Since most points falls outside the confidence interval(grey area), we can also conclude that linear model might not be a good fit