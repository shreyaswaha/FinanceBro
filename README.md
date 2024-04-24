












SW
#

LOGISTIC - USE DIFFERENT DATASET AND COLOURS
ISLR::Caravan
plot(Caravan$Purchase)
?Caravan

#1
logit_model <- glm(Purchase ~ MINKGEM + MZPART + APERSAUT, data = Caravan, family = binomial)
summary(logit_model)
USE DIFFERENT DATASET AND COLOURS
#2
cor(Caravan[, c('MINKGEM', 'MZPART', 'APERSAUT')])
#No, the predictors are not correlated.

#3
## Split data into training and testing sets
set.seed(123) # for reproducibility
train_indices <- sample(1:nrow(Caravan), 1000)
train_data <- Caravan[train_indices, ]
test_data <- Caravan[-train_indices, ]
## Predict on test set
predicted <- predict(logit_model, newdata = test_data, type = "response")
## Create truth table
threshold <- 0.5 # threshold for classifying as positive
predicted_class <- ifelse(predicted > threshold, 1, 0)
actual_class <- test_data$Purchase
truth_table <- table(predicted_class, actual_class)
## Calculate Type 1 and Type 2 error rates
type1_error <- truth_table[2, 1] / sum(truth_table[, 1]) # Type 1 error rate
type2_error <- truth_table[1, 2] / sum(truth_table[, 2]) # Type 2 error rate
## Print truth table and error rates
print(truth_table)
cat("\nType 1 error rate:", type1_error, "\n")
cat("Type 2 error rate:", type2_error, "\n")
USE DIFFERENT DATASET AND COLOURS
#4
accuracy <- sum(diag(truth_table)) / sum(truth_table)
accuracy

#5
ggplot(Caravan, aes(x = MINKGEM, fill = Purchase)) +
  geom_bar(position = "fill") +
  labs(x = "MINKGEM", y = "Proportion", fill = "Purchase") +
  theme_minimal()

ggplot(Caravan, aes(x = MZPART, fill = Purchase)) +
  geom_bar(position = "fill") +
  labs(x = "MZPART", fill = "Purchase") +
  theme_minimal()

ggplot(Caravan, aes(x = APERSAUT, fill = Purchase)) +
  geom_bar(position = "fill") +
  labs(x = "APERSAUT", fill = "Purchase") +
  theme_minimal()
USE DIFFERENT DATASET AND COLOURS
#6
PseudoR2(logit_model)
2nd Graph
ggplot(Auto, aes(x = weight, y = acceleration, color = factor(origin))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = origin), 
              formula = y ~ x, 
              method.args = list(), 
              show.legend = FALSE) +
  labs(x = "Weight in pounds (lbs.)", y = "Acceleration", 
       title = "Weight vs Acceleration by Origin") +
  theme_minimal()

#Question 1 
data(starwars)  #Loading dataset
mh <- median(starwars$height, na.rm = TRUE) #calculating median height
mm<- median(starwars$mass, na.rm = TRUE)  #calculating median mass
filtered_characters <- starwars %>%
  filter(height > mh & mass > mm)
num_characters_above_median <- nrow(filtered_characters)
num_characters_above_median



humans <- starwars %>%
  filter(species == "Human" & !is.na(height))    #Filter for Humans

droids <- starwars %>%
  filter(species == "Droid" & !is.na(height))   #Filter for droids


mhh <- mean(humans$height)    #Calculation of mean heights
mhd <- mean(droids$height)    

if (mhh > mhd) {
  height_difference <- mean_height_humans - mean_height_droids
  cat("Humans are taller than droids on average by", height_difference, "units.\n")
} else {
  cat("Droids are taller than humans on average.\n")
}

# Print the mean heights for humans and droids
cat("Mean height for humans:", mhh, "\n")
cat("Mean height for droids:", mhd, "\n")



#Question 3


# Filter out rows where height or mass is missing
filtered_data <- starwars %>%
  filter(!is.na(height) & !is.na(mass))

# Calculate density 
filtered_data <- filtered_data %>%
  mutate(density = height / mass)

# Arrange the data in descending order of density 
top_dense <- filtered_data %>%
  arrange(desc(density)) %>%
  head(10)

# Arrange the data in ascending order of density 
bottom_dense <- filtered_data %>%
  arrange(density) %>%
  head(10)

# Print the top 10 most dense characters
cat("Top 10 Most Dense Characters:\n")
print(top_dense[, c("name", "density")])

# Print the bottom 10 least dense characters
cat("\nBottom 10 Least Dense Characters:\n")
print(bottom_dense[, c("name", "density")])



#Question 4

# Filter out rows where cylinders or displacement is missing
filtered_data <- Auto[complete.cases(Auto$cylinders, Auto$displacement), ]

# Calculate the average displacement for each cylinder count
avg_displacement_by_cylinders <- filtered_data %>%
  group_by(cylinders) %>%
  summarise(avg_displacement = mean(displacement))

# Print the average displacements for each cylinder count
print(avg_displacement_by_cylinders)

# Perform a statistical test to determine if there is a significant difference in average displacements across cylinder counts
anova_result <- aov(displacement ~ cylinders, data = filtered_data)
summary(anova_result)


#Since p<0.05, there is a significant difference in average displacements across different cylinder counts.


#Question 5
# Load the ISLR2 package
library(ISLR2)

# Load the Auto dataset
data(Auto)

# Filter out rows where acceleration or weight is missing
filtered_data <- Auto[complete.cases(Auto$acceleration, Auto$weight), ]

# Calculate acceleration-to-weight ratio
filtered_data <- filtered_data %>%
  mutate(acc_to_weight = acceleration / weight)

# Find the car with the highest acceleration-to-weight ratio
highest_ratio_car <- filtered_data %>%
  filter(acc_to_weight == max(acc_to_weight)) %>%
  

# Print the car with the highest acceleration-to-weight ratio
print(highest_ratio_car)


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
Also, one thing is missing from answer, will try at the end

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



Question 9 ?????????//






V

