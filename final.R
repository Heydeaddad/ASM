# data_preparation.R
# This script loads, cleans, and prepares  dataset for analysis.

# Load necessary libraries
install.packages("dplyr")
install.packages("caTools")
install.packages("ggplot2")
install.packages("caret")
install.packages("corrplot")
install.packages("reshape2")
install.packages("car")
library(dplyr)
library(caTools)

# Load the dataset
churn_data <- read.csv("https://raw.githubusercontent.com/Heydeaddad/testtest/refs/heads/main/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# View the structure of the dataset
str(churn_data)
summary(churn_data)
# Calculate the counts of each contract type
contract_counts <- table(churn_data$Contract)

# Calculate the proportion of each contract type
contract_proportions <- prop.table(contract_counts) * 100

# Print the results
print(contract_proportions)
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
library(ggplot2)

# Histogram of Tenure
tenure_histogram <- ggplot(churn_data, aes(x = tenure)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Tenure", x = "Tenure (Months)", y = "Count") +
  theme_minimal()

# Save plot to file
ggsave("tenure_histogram.png", plot = tenure_histogram, width = 8, height = 6)

# Boxplot of Monthly Charges by Churn
monthly_charges_boxplot <- ggplot(churn_data, aes(x = factor(Churn), y = MonthlyCharges, fill = factor(Churn))) +
  geom_boxplot() +
  labs(title = "Monthly Charges by Churn", x = "Churn", y = "Monthly Charges") +
  scale_fill_manual(values = c("green", "red"), name = "Churn", labels = c("No", "Yes")) +
  theme_minimal()

# Save plot to file
ggsave("monthly_charges_boxplot.png", plot = monthly_charges_boxplot, width = 8, height = 6)

# Bar Chart of Contract Type vs. Churn
contract_type_bar_chart <- ggplot(churn_data, aes(x = Contract, fill = factor(Churn))) +
  geom_bar(position = "fill") +
  labs(title = "Contract Type vs Churn", x = "Contract Type", y = "Proportion") +
  scale_fill_manual(values = c("green", "red"), name = "Churn", labels = c("No", "Yes")) +
  theme_minimal()

# Save plot to file
ggsave("contract_type_bar_chart.png", plot = contract_type_bar_chart, width = 8, height = 6)

library(corrplot)
library(tidyr)
library(ggplot2)
# Calculate correlation matrix for numeric columns
numeric_features <- churn_data[, sapply(churn_data, is.numeric)]
correlation_matrix <- cor(numeric_features, use = "complete.obs")

# Convert the correlation matrix to a long format
correlation_melted <- as.data.frame(as.table(correlation_matrix))

# Create heatmap with numbers
correlation_heatmap <- ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 3) +  # Add numbers to the cells
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "Features", y = "Features")

# Save the heatmap 
ggsave("correlation_heatmap_with_numbers.png", plot = correlation_heatmap, width = 10, height = 8)

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


# Fit the logistic regression model
logistic_model <- glm(Churn ~ tenure + MonthlyCharges + Contract, 
                      data = training_set, 
                      family = binomial)

# Extract p-values
p_values <- summary(logistic_model)$coefficients[, 4]  # 4th column contains p-values
print(p_values)

# model_evaluation.R
# This script evaluates the performance of the logistic regression model.



# Logistic Regression Model Performance
# Load the prepared data and model
load("test_set.RData")
load("logistic_model.RData")

# Predicting on the test set
predicted_probabilities <- predict(logistic_model, newdata = test_set, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Confusion Matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_set$Churn)
print(confusion_matrix)

# Model Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Model Accuracy:", accuracy))

# Precision, Recall, and F1 Score
library(caret)
confusion <- confusionMatrix(as.factor(predicted_classes), as.factor(test_set$Churn))
precision <- confusion$byClass["Pos Pred Value"]
recall <- confusion$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))
# ROC Curve and AUC
library(pROC)
roc_curve <- roc(as.numeric(test_set$Churn), predicted_probabilities)
plot(roc_curve, main = "ROC Curve", col = "blue")
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Save all metrics to a single text file
metrics <- list(
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1_Score = f1_score,
  AUC = auc_value
)

# Save all metrics to a single file
metrics_file <- "model_metrics.txt"
writeLines(paste(names(metrics), metrics, sep = ": "), con = metrics_file)

# Save confusion matrix separately
write.csv(confusion_matrix, "confusion_matrix.csv")
