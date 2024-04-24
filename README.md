












SW
Im started from last sections
Use this dataset for Caravan - Q9
https://liacs.leidenuniv.nl/~puttenpwhvander/library/cc2000/data.html


Neel




HS
#Question 1 
data(starwars)  #Loading dataset
mh <- median(starwars$height, na.rm = TRUE) #calculating median height
mm<- median(starwars$mass, na.rm = TRUE)  #calculating median mass
filtered_characters <- starwars %>%
  filter(height > mh & mass > mm)
num_characters_above_median <- nrow(filtered_characters)
num_characters_above_median



Question 7 and 9 ?????????//






V

