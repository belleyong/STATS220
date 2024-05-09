library(magick)
library(tidyverse)
my_custom_palette <- c("#ca0020", "#f4a582", "#92c5de", "#0571b0")

titleSlide <- image_blank(color = "#92c5de", width = 1200, height = 400) %>% 
  image_annotate(text = "Comparing YouTube data of two food channels: \n@Marionskitchen & @NinosHome.",
                 color = "#f7f7f7",
                 font = "IMPACT",
                 size = 60,
                 gravity = "center")

plot1_text <- ("I've used geom_line() to show the trend of the channels' growth depending on its mean views. I've set y = Mean View Count (Thousands) to indicate that these values have been divided by 1000, so it will not show an abnormally large value on the plot.") %>%
  str_wrap(130)
  
plot1 <- image_read("plot1.png") %>% 
  image_annotate(text = plot1_text ,
                 size = 20,
                 gravity = "south")

plot2_text = ("I've observed that videos that are around\n5-6 mins are more popular compared to other video duration. I've used geom_density() to show the distribution of\npopular and unpopular video according to their like count. I made sure my title is concise!") %>%
  str_wrap(65)

plot2 <- image_read("plot2.png") %>% 
  image_annotate(text = plot2_text,
                 size = 15,
                 gravity = "center")

plot3_text <- ("I've observed that the more views a video has the more likes it receives. I've made a new theme to my plot using my colour palette. I've made sure my title is clear and informative!") %>%
  str_wrap(150)
  
plot3 <- image_read("plot3.png") %>% 
  image_annotate(text = plot3_text,
                 size = 17,
                 gravity = "south")

plot4_text <- ("The channel Marion's Kitchen includes the channel name in the title for most of the videos.By looking at the top 10 most common words used in titles, it is not obvious that these are data from food channels.
  This indicates that viewers are likely drawn to the videos based on the thumbnails rather than the titles alone. I've used geom_text() to add text onto the columns showing the word and frequency of each column and set the colours of the geom_col and geom_text to my colour palette so it fits my theme.") %>%
  str_wrap(190)

plot4 <- image_read("plot4.png") %>%
  image_annotate(text = plot4_text,
                 size = 15,
                 gravity = "south") 
  
conclusion_text <- "Overall, I learnt that: Both channels had declining views after the year 2020.
This might be due to the pandamic where everyone was required to stay home.
Videos around length 5-6 mins are the most popular indicating that the audience prefers short and engaging content.
Lastly I've learnt that exploring the youtube data from my favorite food channels using geoms are so fun and engaging!" %>%
  str_wrap(80)

conclusion <- image_blank(color = my_custom_palette[2], width = 1200, height = 400) %>%
  image_annotate(text = conclusion_text,
                 color = "#f7f7f7",
                 font = "IMPACT",
                 size = 35, gravity = "center")

#creativity 
ninoshome <- image_read("https://res.cloudinary.com/jerrick/image/upload/d_642250b563292b35f27461a7.png,f_jpg,fl_progressive,q_auto,w_1024/5f5da3ad6532ec001db67f10.jpg") %>%
  image_scale("600 x 300!")

top_text <- image_blank(width = 600, height = 100, color = "#f7f7f7") %>%
  image_annotate(text = "Let's take a break from plots and look at me and my cat!
                 I'm Nino's home!",
                 size = 20,
                 font = "IMPACT",
                 gravity = "center")

nino <- c(top_text, ninoshome) %>% 
  image_append(stack=TRUE)

marionskitchen <- image_read("https://i.ytimg.com/vi/4FNQGT0kT1c/hq720.jpg") %>% 
  image_scale("600 x 300!")

text <- image_blank(width = 600, height = 100, color = "#f7f7f7") %>%
  image_annotate(text = "I love sharing recipes and watch me enjoy my ribs!
                 I'm Marion's Kitchen!",
                 size = 20,
                 font = "IMPACT",
                 gravity = "center")

marion <- c(text, marionskitchen) %>% 
  image_append(stack=TRUE)

creativity <- c(nino, marion) %>% 
  image_append()

#combine frames
combined_gif <- c(rep(titleSlide, 5), rep(plot1, 5), rep(plot2, 5), rep(creativity, 5), rep(plot3,5), rep(plot4,5), rep(conclusion, 5))

# animation
animated_gif <- combined_gif %>% 
  image_animate(fps=1)

# write the animated_gif to data_story.gif
image_write(animated_gif, "data_story.gif")

creativity