












SW
#
Use this dataset for Caravan - Q9
https://liacs.leidenuniv.nl/~puttenpwhvander/library/cc2000/data.html

Q7 - USE DIFFERENT DATASET AND COLOURS
my_function <- function(dataframe, col_name) {
  dataframe %>%
    summarize(
      normalize = log({{col_name}}))%>%
    ggplot(aes(x = {{col_name}}), na.rm = TRUE)+
    geom_density(fill = "#69b3a2", color="#e9ecef", alpha=0.8)+
    theme_minimal()+
    scale_x_continuous(limits = c(0, 2000))
}
#Sample Data
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)
my_function(data, data$price)
USE DIFFERENT DATASET AND COLOURS

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

