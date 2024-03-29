library(magick)

# inspiration meme link : https://www.care.com/c/getting-older-memes/

# store url 
meme_url <- "https://www.care.com/c/wp-content/uploads/sites/2/2021/04/maressab-202115020615567399.jpg.optimal.jpg"

meme <- image_read(meme_url) %>% 
  image_scale(600)

image_write(meme, "inspo_meme.png")

# static memes creation (5 Frames)
# my meme frame 1 
hungry_text <- image_blank(width = 300, height = 100, color = "#FFFFFF") %>% 
  image_annotate(text = "Me  WHEN  I'M  HUNGRY", color = "#000000", size = 30, font = "IMPACT", gravity = "center")

food_text <- image_blank(width = 300, height = 100, color = "#FFFFFF") %>% 
  image_annotate(text = "ME  WHEN I  HAVE  FOOD", color = "#000000", size = 30, font = "IMPACT", gravity = "center")

# read/get the image and change the size of the image
crying_cat <- image_read("https://i.pinimg.com/736x/9d/70/b8/9d70b860a93cc01702926970874cdcf2.jpg") %>% 
  image_scale(305)

happy_star_cat <- image_read("https://preview.redd.it/cute-cats-3-i-have-no-clue-what-to-post-so-yeah-and-you-can-v0-kwevwk6tpzba1.jpg?width=640&crop=smart&auto=webp&s=51b27e2a51f087d7d56a6efcc5ceef4b0e34208e") %>% 
  image_scale(300)

# make introduction text 
introduction <- image_blank(width = 600, height = 90, color = "#FFFFFF") %>%
  image_annotate(text = "A LITTLE ABOUT ME", color = "#000000", font = "IMPACT", size = 35, gravity = "center")

# combine text for top row 
words <- c(hungry_text, food_text)
words_row <- image_append(words)

#combine pictures for bottom row 
cats <- c(crying_cat, happy_star_cat)
cats_row <- image_append(cats) #append

# create a vector by combining all the text and image
my_meme <- c(words_row, cats_row, introduction) %>% 
  image_append(stack = TRUE)

# create my_meme.png
image_write(my_meme, "my_meme.png")

# my meme frame 2 

sad_cat <- image_read("https://pbs.twimg.com/profile_images/1070028306790141952/P3tkvBDi_400x400.jpg") %>% 
  image_scale(255)
    
happy_cat <- image_read("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcS_HITWQZZ5zgTHFM3PSvUjzqwls1LiYiOzwcQXalM69w&s") %>% 
  image_scale(255)
    
rain_text <- image_blank(width = 300, height = 200, color = "#FFFFFF") %>%
  image_annotate(text = "Me when it RAINS", color = "#000000", size = 35, font = "IMPACT", gravity = "center")

sunny_text <- image_blank(width = 300, height = 200, color = "#FFFFFF") %>%
  image_annotate(text = "Me when it's SUNNY", color = "#000000", size = 35, font = "IMPACT", gravity = "center")

introduction <- image_blank(width = 600, height = 50, color = "#FFFFFF") %>%
  image_annotate(text = "A LITTLE ABOUT ME", color = "#000000", font = "IMPACT", size = 35, gravity = "center")

# combine text and picture for top row 
top_row <- image_append(c(rain_text, sad_cat))

# combine text and picture for bottom row 
bottom_row <- image_append(c(sunny_text, happy_cat))

my_meme2 <- c(introduction, top_row, bottom_row) %>%
  image_append(stack = TRUE)

# my meme frame 3 
stats_text <- image_blank(width = 600, height = 100, color = "#FFFFFF") %>%
  image_annotate(text = "ME WHEN SOMEONE STARTS TALKING\nABOUT STATS 220", color = "#000000", size = 25, font = "IMPACT", gravity = "north")

stats_cat <- image_read("https://m.media-amazon.com/images/I/51ZjBEW+qNL._AC_UF894,1000_QL80_.jpg") %>%
  image_scale(600)

introduction <- image_blank(width = 600, height = 50, color = "#FFFFFF") %>%
  image_annotate(text = "A LITTLE ABOUT ME", color = "#000000", font = "IMPACT", size = 35, gravity = "center")

my_meme3 <- c(introduction, stats_cat, stats_text) %>%
  image_append(stack = TRUE)

# my meme frame 4 
confused_cat <- image_read("https://i.pinimg.com/564x/33/29/04/332904f53cd846c45fcf0f2ce682272d.jpg") %>%
  image_scale(450)

cereal_text <- image_blank(width = 450, height = 35, color = "#000000") %>%
  image_annotate(text = "ME WHEN SOMEONE PUTS MILK BEFORE CEREAL", color = "#FF0000", size = 20, font = "IMPACT", gravity = "center")

introduction <- image_blank(width = 450, height = 35, color = "#000000") %>%
  image_annotate(text = "A LITTLE ABOUT ME", color = "#FFFFFF", font = "IMPACT", size = 20, gravity = "center")

my_meme4 <- c(introduction, confused_cat, cereal_text) %>%
  image_append(stack = TRUE)

# my meme frame 5 
# create the top meme text 
money_emoji <- image_blank(width = 600, height = 100, color = "#FFFFFF") %>%
  image_annotate(text = "A LITTLE ABOUT ME ^$_$^", color = "#000000", font = "IMPACT", size = 20, gravity = "center")
# read the cat image 
money_cat <- image_read("https://thinksaveretire.com/wp-content/uploads/2019/01/money-memes-12.jpg") %>% 
  image_scale(600)
# create the bottom meme text 
money_text <- image_blank(width = 600, height = 100, color = "#FFFFFF") %>%
  image_annotate(text = "ME WITH MY DREAMS", color = "#000000", font = "IMPACT", size = 25, gravity = "center")
# create a vector that combines all text and image
my_meme5 <- c(money_emoji, money_cat, money_text) %>%
  image_append(stack = TRUE )

# combine all 4 frames together 
combined_meme <- c(meme, my_meme, my_meme2, my_meme3, my_meme4, my_meme5)

# animation
animated_gif <- combined_meme %>% 
  image_animate(fps = 0.5)

# create my_animation gif 
image_write(animated_gif, "my_animation.gif")

animated_gif