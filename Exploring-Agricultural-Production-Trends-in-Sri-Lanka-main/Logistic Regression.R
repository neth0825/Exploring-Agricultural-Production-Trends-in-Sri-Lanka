#Setup and Loading Required Libraries 
library(caret) 
library(dplyr) 
library(readr) 
library(ggplot2) 

#Data Loading and Initial Cleaning 
raw_data <- read.csv("C:/Users/User/Downloads/FAOSTATCrops and livestock 
products.csv", header = FALSE) 
colnames(raw_data) <- as.character(unlist(raw_data[1, ])) 
FAOSTAT <- raw_data[-1, ] 
rownames(FAOSTAT) <- NULL 

#Data Type Conversion 
FAOSTAT$Value <- as.numeric(FAOSTAT$Value) 
FAOSTAT$Year <- as.integer(FAOSTAT$Year) 
FAOSTAT$Area <- as.factor(FAOSTAT$Area) 
FAOSTAT$Item <- as.factor(FAOSTAT$Item) 
FAOSTAT$Element <- as.factor(FAOSTAT$Element)

#Preparing Data for Logistic Regression 
logit_data <- FAOSTAT %>% 
  filter(Element == "Production", !is.na(Value)) %>% 
  select(Area, Item, Year, Value) 

median_value <- median(logit_data$Value, na.rm = TRUE) 
logit_data$HighProduction <- ifelse(logit_data$Value > median_value, 1, 0) 

#Splitting Data and Training the Model 
set.seed(123) 
train_index <- createDataPartition(logit_data$HighProduction, p = 0.7, list = FALSE) 
train_data <- logit_data[train_index, ] 
test_data <- logit_data[-train_index, ] 

logit_model <- glm(HighProduction ~ Area + Item + Year, data = train_data, family = 
                     binomial) 
summary(logit_model) 

# Prediction and Accuracy Evaluation 
test_data$predicted_prob <- predict(logit_model, newdata = test_data, type = "response") 
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0) 

conf_matrix <- table(Predicted = test_data$predicted_class, Actual = 
                       test_data$HighProduction) 
accuracy <- mean(test_data$predicted_class == test_data$HighProduction) 

#Confusion Matrix Visualization 
conf_df <- as.data.frame(conf_matrix) 
colnames(conf_df) <- c("Predicted", "Actual", "Freq") 

ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) + 
  geom_tile() + 
  geom_text(aes(label = Freq), color = "black", size = 6) + 
  scale_fill_gradient(low = "lightblue", high = "lightyellow") + 
  labs(title = "Confusion Matrix: High Production", x = "Actual", y = "Predicted") + 
  theme_minimal()

#. Distribution of Predicted Probabilities 
ggplot(test_data, aes(x = predicted_prob)) + 
  geom_histogram(bins = 30, fill = "lightgreen", color = "darkgreen") + 
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") + 
  labs(title = "Predicted Probabilities", x = "Probability of High Production", y = "Count") 
+ 
  theme_minimal() 

#Predicting Wheat Production Specifically 
FAOSTAT$Wheat_Produced <- ifelse(FAOSTAT$Item == "Wheat", 1, 0) 

wheat_data <- FAOSTAT %>% 
  filter(!is.na(Value)) %>% 
  select(Area, Element, Year, Value, Wheat_Produced) 

wheat_model <- glm(Wheat_Produced ~ Area + Element + Year + Value, data = 
                     wheat_data, family = binomial) 
wheat_data$predicted <- predict(wheat_model, type = "response") 
wheat_data$predicted_class <- ifelse(wheat_data$predicted > 0.5, 1, 0) 

wheat_conf <- table(Predicted = wheat_data$predicted_class, Actual = 
                      wheat_data$Wheat_Produced) 
wheat_accuracy <- mean(wheat_data$predicted_class == wheat_data$Wheat_Produced) 
print(paste("Wheat Accuracy:", round(wheat_accuracy, 4)))

