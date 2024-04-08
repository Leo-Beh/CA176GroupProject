library(readxl)
library(dplyr)
library(ggplot2)


# Load data ---------------------------
companies_df <- read_excel("companies.xlsx")


# Filter data -------------------------
filtered_companies_df <- companies_df %>%
  filter(Highly_rated_for == "Job Security")%>%  
  mutate(Avg_salary = as.numeric(gsub("k", "", Avg_salary)) * 1000)%>%
  slice(1:100)


# Load graph ---------------------------
ggplot(data = filtered_companies_df, aes(x = Avg_salary, y = Ratings)) +
  geom_point(aes(colour = Ratings), shape =16 , alpha = 0.7) +
  geom_smooth(method = "gam", color = "black", size = 0.7, se = TRUE) +
  scale_color_gradient(low = "yellow", high = "#D22B2B") +
  theme_bw() +
  labs(
    title = "Scatter Plot of Avg Salary vs Ratings",
       subtitle = "For comapnies only rated for Job Security",
       x = "Average Salary",
       y = "Ratings",
       caption = "A very weak correlation between average salary \nas we see most points lies outside the confidence interval") +
  
  # Scale x and y axis
  scale_x_continuous(labels = scales::comma,
                     trans = "log10") +
  scale_y_continuous(
                     limits = c(3,4.5),
                     breaks = seq(3, 4.5, 0.5),
                     labels = c("3 Fair", "3.5", "4 Good", "4.5")
                     ) +
  
  # Setting up the aesthetic of the graph 
  theme(plot.title = element_text(hjust = 0.5, face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        legend.position = "right",
        axis.text = element_text(size = 10, face = "italic"),
        axis.title = element_text(size = 12, face = "bold.italic"))