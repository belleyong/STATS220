library(tidyverse)

learning_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQqfYLoyc7Y3hqTaeWIJ4_9Xyzstg3HQSHYV0Wwhol_SJ87G-phfDV0LyD2K2gafpxQ0G2AWCLWtVOL/pub?output=csv") %>% 
  rename(age = 2,
         social_media_app = 3,
         social_media_usage = 4,
         social_media_check_frequency = 5,
         addiction_to_social_media = 6,
         social_media_purposes = 7,
         social_media_effect = 8)

# First Bar Chart - Checking 
learning_data %>%
  ggplot() + 
  geom_bar(aes(x = social_media_check_frequency, fill = social_media_effect)) + 
  # adding more information to the bar chart so it shows the relationship of the plot clearly 
  labs(title = "Does Social Media usage affects Individuals' wellbeing?", # Stating the purpose of the plot 
       subtitle = "Comparing the frequency of using social media and it's effect on individuals",
       x = "Usage Frequency", # name x-axis as Usage Frequency 
       y = "Number of Individuals") # name y-axis as the amount of individuals that has answered the form to have either negative, positive or no effect after using social media

# Second Bar Chart 
```{r}
learning_data_longer <- learning_data %>%
  separate_rows(social_media_purposes, sep=", ") %>% 
  length(social_media_purposes)
```

# First summary value
# Show the longest social media usage with the shortest social media usage 
paste("The largest time an individual uses social media is", max(learning_data$social_media_usage), "hours and the shortest time an individual uses social media is", min(learning_data$social_media_usage),"hours.")

length(learning_data$age)
# Second summary value 
average_age <- mean(learning_data$age) %>% round(1)
paste("The average age of individuals that took my social media usage survey are the age of", average_age)