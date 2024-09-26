# data_preparation.R
# This script loads, cleans, and prepares  dataset for analysis.

# Load necessary libraries
install.packages("dplyr")
install.packages("caTools")
install.packages("ggplot2")
install.packages("caret")
library(dplyr)
library(caTools)

# Load the dataset
churn_data <- read.csv("https://raw.githubusercontent.com/Heydeaddad/testtest/refs/heads/main/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# View the structure of the dataset
str(churn_data)

# Data Cleaning: Removing rows with missing values
churn_data <- na.omit(churn_data)

# Encoding categorical variables
churn_data$Churn <- ifelse(churn_data$Churn == "Yes", 1, 0)
churn_data$SeniorCitizen <- as.factor(churn_data$SeniorCitizen)
churn_data$Partner <- as.factor(churn_data$Partner)
churn_data$Dependents <- as.factor(churn_data$Dependents)

# Splitting the dataset into training and test sets
set.seed(123)
split <- sample.split(churn_data$Churn, SplitRatio = 0.7)
training_set <- subset(churn_data, split == TRUE)
test_set <- subset(churn_data, split == FALSE)

# Save the cleaned and prepared data
save(training_set, file = "training_set.RData")
save(test_set, file = "test_set.RData")



# exploratory_data_analysis.R
# This script performs exploratory data analysis on the Telco Customer Churn dataset.

# Load necessary libraries
library(ggplot2)

# Load the prepared data
load("training_set.RData")

# Summary statistics
summary(training_set)

# Visualizations

# Histogram: Customer Tenure vs. Churn
ggplot(training_set, aes(x = tenure, fill = as.factor(Churn))) + 
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Customer Tenure vs Churn", x = "Tenure", y = "Count")

# Save plot to file
ggsave("tenure_vs_churn.png")




# logistic_regression_model.R
# This script builds and fits a logistic regression model to predict customer churn.

# Load necessary libraries
library(caret)

# Load the prepared data
load("training_set.RData")

# Building the logistic regression model
logistic_model <- glm(Churn ~ tenure + MonthlyCharges + Contract + SeniorCitizen + Partner, 
                      data = training_set, family = binomial)

# Display model summary
summary(logistic_model)

# Save the model
save(logistic_model, file = "logistic_model.RData")




# model_evaluation.R
# This script evaluates the performance of the logistic regression model.

# Load necessary libraries
library(caret)

# Load the prepared data and model
load("test_set.RData")
load("logistic_model.RData")

# Predicting on the test set
predicted_probabilities <- predict(logistic_model, newdata = test_set, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Confusion Matrix
confusion_matrix <- table(predicted_classes, test_set$Churn)
print(confusion_matrix)

# Model Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Model Accuracy:", accuracy))

# Save confusion matrix and accuracy
write.csv(confusion_matrix, "confusion_matrix.csv")
write(paste("Model Accuracy:", accuracy), file = "model_accuracy.txt")