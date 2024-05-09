library(magick)
library(tidyverse)
library(jsonlite)
json_data <- fromJSON("pixabay_data.json")
pixabay_photo_data <- json_data$hits 

selected_photos <- pixabay_photo_data %>%
  select(previewURL, pageURL, likes, views, downloads, comments) %>%
  mutate(popularity = ifelse(views > 5000 & likes > 30,
                                               "More Popular Images",
                                               "Less Popular Images")) %>% #New Variable 1 = popularity
  mutate(total_downloads_count = ifelse(downloads < 5000,
                                        "Low Downloads Count",
                                        "High Downloads Count")) %>% #New Variable 2 = total_downloads_count
  mutate(image_filter = ifelse(str_detect(str_to_lower(pageURL), #New Variable 3 = image_filter to check if the url has key words
                                            "dog"),
                                 "sleepy dog",
                                 "sleepy animal that is not a dog")) %>%
  filter(views > 4000) #filter to get a data set of only 50 photos

write_csv(selected_photos, "selected_photos.csv") # save data frame as csv file 

# summary value 1 
median_views <- selected_photos$views %>% median(na.rm = TRUE)
# summary value 2
median_downloads <- selected_photos$downloads %>% median(na.rm = TRUE) %>% round()
# summary value 3
total_likes <- selected_photos$likes %>% sum()

summarised_data <- selected_photos %>%
  group_by(popularity) %>% 
  summarise(median_downloads = median(downloads)) #count the median of popularity 

# make gif 
# frame 1
sleepy1 <- image_read(selected_photos$previewURL[1]) %>%
  image_scale(450)

daily_mood <- image_blank(width = 450, height = 70, color = "#000000") %>%
  image_annotate(text = "MY DAILY MOOD", color = "#FFFFFF", font = "IMPACT", size = 40, gravity = "center")

frame1 <- c(daily_mood, sleepy1) %>%
  image_append(stack = TRUE)

# frame 2
sleepy2 <- image_read(selected_photos$previewURL[2]) %>%
  image_scale(450)

word <- image_blank(width = 450, height = 70, color = "#000000") %>% 
  image_annotate(text = "I'M ENJOYING MY NAP", color = "#FFFFFF", font = "IMPACT", size = 40, gravity = "center")

frame2 <- c(sleepy2, word) %>% 
  image_append(stack=TRUE)

# frame 3
sleepy3 <- image_read(selected_photos$previewURL[3]) %>%
  image_scale(450)

word2 <- image_blank(width = 450, height = 30, color = "#FFFFFF") %>%
  image_annotate(text = "SHHHH", font = "IMPACT", size = 30, gravity = "center")

frame3 <- c(word2, sleepy3) %>% 
  image_append(stack=TRUE)

# frame 4
sleepy4 <- image_read(selected_photos$previewURL[4]) %>%
  image_scale(450)

word3 <- image_blank(width = 450, height = 40, color = "#FFFFFF") %>% 
  image_annotate(text = "I'M CUTE WHEN IM ASLEEP", font = "IMPACT", size = 30, gravity = "center")

frame4 <- c(sleepy4, word3) %>%
  image_append(stack=TRUE)

# animated gif 
animated_gif <- c(frame1, frame2, frame3, frame4) %>%
  image_animate(fps = 1)

# write the animated_gif to my_photo.gif
image_write(animated_gif, "my_photos.gif")

# creativity part making a plot 
selected_photos %>%
  ggplot() +
  geom_bar(aes(x = popularity, fill = image_filter)) + 
  labs(title = "Does a sleepy dog image have a higher popularity?", # The plot has a title so conveys the purpose of the plot clearer
       x = "Popularity", # name x-axis as Popularity 
       y = "Number of Images") # name y-axis as Number of Image