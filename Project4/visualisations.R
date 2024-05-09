library(tidyverse)

youtube_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRj2lK3toZAqsQE9iccztMJFeWGN0mR0aLQKYK59IbtwDKnJm9VN6CEuPH7AHCh0XU9snOdrrb2jeEj/pub?output=csv")

# Define custom color palette
my_custom_palette <- c("#ca0020", "#f4a582", "#92c5de", "#0571b0")

my_theme <- theme_minimal() + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.background = element_rect(fill = "#f7f7f7"),
        panel.background = element_rect(fill = "#f7f7f7"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        strip.text = element_text(size = 12, color = "black", face = "italic"), # Adjust size of facet strip text
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12))

#create a line plot of year released and mean view counts 
#get substring from datePublished so we can get the year 
filtered_data <- youtube_data %>%
  mutate(year_released = datePublished %>% str_sub(1, 4) %>% parse_number())

#calculate the mean views and make the value smaller by dividing it by 1000
mean_views <- filtered_data %>%
  group_by(channelName, year_released) %>%
  summarise(mean_views_thousands = round(mean(viewCount, na.rm = TRUE)/1000))

plot1 <- mean_views %>%
  ggplot() + 
  geom_line(aes(x = year_released, 
                y = mean_views_thousands,
                colour = channelName)) +
  facet_wrap(vars(channelName)) + #show two different plot for each channel
  scale_colour_manual(values = my_custom_palette) +
  labs(title = "Is the channel growing over the years?",
       x = "Year released",
       y = "Mean View Count (Thousands)") + #specify that the values on the plot need to be multiplied by 1000
  
  my_theme
#save the plot and specify the size of the plot as an image
ggsave("plot1.png", plot = plot1, width = 1200, height = 400, units = "px", dpi = 130)

#plot2
channelPopularity <- youtube_data %>%
  mutate(popularity = ifelse(likeCount >  5000, 
                             "popular", #if like count greater than 5000 categorise as popular
                             "unpopular"))

# Modify the plot code to include custom colors
plot2 <- channelPopularity %>%
  ggplot() +
  geom_density(aes(x = duration/60,
                   fill = popularity), #so duration will be in minutes
               alpha = 0.5,
               adjust = 2) +
  scale_fill_manual(values = my_custom_palette) +  # Specify custom colors
  labs(title = "Does the duration of the video affect the popularity?",
       x = "Duration of Video (Minutes)",
       y = "Density") + 
  my_theme
  #my_theme #added my theme that I've created at the start

ggsave("plot2.png", plot = plot2, width = 1200, height = 400, units = "px", dpi = 120)

#create mean view counts and mean like counts 
#make the values smaller by dividing by 1000
mean_data <- youtube_data %>% 
  mutate(mean_views_count_thousands = round(mean(viewCount, na.rm = TRUE)/1000)) %>%
  mutate(mean_like_thousands = round(mean(likeCount, na.rm = TRUE)/1000))

# Create a scatter plot of viewCount vs likeCount colored by channelName
plot3 <- mean_data %>%
  ggplot(aes(x = viewCount/1000, y = likeCount/1000, colour = channelName)) +
  geom_point() +  # Removed extra parenthesis and used the custom palette for channelName
  labs(title = "Does more views result in more likes?",
       x = "View Count (Thousands)",
       y = "Like Count (Thousands)",
       colour = "Channel Name") + # Add axis labels and legend title
  facet_wrap(vars(channelName)) +
  scale_colour_manual(values = my_custom_palette) +
  my_theme

ggsave("plot3.png", plot = plot3, width = 1200, height = 400, units = "px", dpi = 130)

# Bar plot 4 (Count the word occurence in the title column)
title_word_occurrence <- youtube_data %>%
  select(title) %>%
  separate_rows(title, sep = " ") %>%
  mutate(stripped_word = str_to_lower(title) %>%
           str_remove_all("[[:punct:]]")) %>%
  filter(!stripped_word == "") %>%
  group_by(stripped_word) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ungroup()

plot4 <- title_word_occurrence %>%
  ggplot(aes(x = reorder(stripped_word, n),
             y = n,
             fill = stripped_word)) +
  
  geom_col(fill = my_custom_palette[1]) +
  
  geom_text(aes(label = stripped_word),
            color = my_custom_palette[4],
            size = 4,
            position = position_nudge(y = 4)) +
  
  geom_text(aes(label = n),
            color = "white",
            position = position_nudge(y = -4),
            size = 3) +
  
  my_theme +
  
  labs(x = "Word",
       y = "Frequency ",
       title="Most common words used in Titles") 

ggsave("plot4.png", plot = plot4, width = 1200, height = 400, units = "px", dpi = 130)

plot4