## Digital Humanities Librarianship Tech Tools Exploration
## Exploring data storage in GitHub using Spiderman Into the Spider-Verse as our data.

# Loading libraries
library(dplyr)
library(tidyverse)
library(tidytext)
library(stopwords)
library(ggplot2)
library(RColorBrewer)

# Reading script txt file:

screenplay <- readLines("~/Desktop/529/Tech Tools Exploration/SV_screenplay.txt")

# Creating dataframe from this file:

screenplay_df <- data.frame(line = 1: length(screenplay), text = screenplay)

# tokenizing screenplay:

tidy_screenplay <- screenplay_df %>% 
  unnest_tokens(word, text)

# creating dataframe for stopwords:
en_stopwords <- data.frame(word = stopwords())
en_stopwords <- en_stopwords %>% 
  #I am not a good enough coder to do this an easier way, clearly. this was tedious:
  add_row(word = c("v.o", "32","3200","3250", "33","34","3454543","35","3.58765E+13","36","37","38","39","4","40","41","42","43","44","45","46","47","48","49","5","50","51","52","53","54","55","56","57","58","59","6","60","61","616peter’s","62","63","64","65","66","67","68","69","7","70","71","72","73","74","75","76","77","78","79","8","80","81","82","83","84","85","86","87","88","89","9","90","91","92","93","94","95","96","97","98","99","1210","122","1220","123","124","1240","125","1250","126","127","128","129","13","130","131","1330","14","1400","1450", "15","16","1690","17","1700","1725","1750","18","19","1933","1967","1968","2","20","2018","2090","21","2100", "2120","2140","22","23","2300","2350","24","2400","25","2500","2545","2550","26","27","2700","28","2850","29","2900","2910","2d","2920","3","30","3000","3010","3050","3075","3090","31","3100","3145","3150","3175","0","1","0100","0150","0200","0425","0500","0520","0700","0730","0790","10","100","1010","102","103","104","105","106","107","108","109","11","110","1100","111","112","113","114","115","1150","116","117","118","119","12","120","1200","121","2800","35876534545435","101","int","cont'd","i'm","it's","can"))

#removing stopwords from screenplay
tidy_screenplay <- tidy_screenplay %>% 
  anti_join(en_stopwords)

#countng words:
count_screenplay <- tidy_screenplay %>% 
  count(word, sort = TRUE) 

#even though I put con't and i'm and it's in the stopwords, they still showed up. I got rid of them and some other commons words to get a better data visualization.
count_screenplay <- count_screenplay[-c(7,10,11,9,13,14,15,16,17),]

#slicing the top 10 most used words:
top_words <- count_screenplay %>% 
  slice_max(n = 10, order_by = n)

#making a graph!
ggplot(data = top_words) +
  geom_col(mapping = aes(x = n, 
                         y = reorder(word, n), fill = word)) +
  labs(title = "Most Common Words in the Spider-Verse",
       x = "Frequency",
       y = "Word")
