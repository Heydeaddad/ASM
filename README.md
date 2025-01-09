# **Customer Churn Analysis**

A predictive analysis project to identify and mitigate customer churn in the telecommunications industry.

---

## **Project Overview**
This project leverages a logistic regression model to predict customer churn and provides actionable insights for business strategies.

### **Key Results**
- **Accuracy:** 78.25%
- **Precision:** 82.95%
- **Recall:** 88.57%
- **F1 Score:** 85.67%
- **AUC:** 0.8167

---

## **Dataset**
- **Source:** [Telco Customer Churn Dataset](https://raw.githubusercontent.com/Heydeaddad/testtest/refs/heads/main/WA_Fn-UseC_-Telco-Customer-Churn.csv)
- **Size:** 7,043 customer records with demographic, service, and billing details.

---

## **Steps Performed**
1. **Data Preparation:**
   - Handled missing values and encoded categorical variables.
   - Split data into training and test sets.
2. **Exploratory Data Analysis (EDA):**
   - Visualizations (e.g., histograms, boxplots) and summary statistics.
3. **Model Training and Evaluation:**
   - Built a logistic regression model.
   - Evaluated using metrics: Accuracy, Precision, Recall, F1 Score, and AUC.
4. **Recommendations:**
   - Business strategies to retain high-risk customers.

---

## **Usage**
1. **Setup:**
   - Install required R packages:
     - `dplyr`
     - `ggplot2`
     - `caret`
     - `pROC`
     - `corrplot`

2. **Run the Scripts:**
   - `data_preparation.R`: Prepares the dataset.
   - `exploratory_data_analysis.R`: Performs EDA and saves visualizations.
   - `logistic_regression_model.R`: Fits the logistic regression model.
   - `model_evaluation.R`: Evaluates model performance and saves metrics.

3. **Outputs:**

### 3.1 **Data Visualizations:**
- `tenure_histogram.png`: Histogram showing the distribution of customer tenure.
- `monthly_charges_boxplot.png`: Boxplot illustrating monthly charges by churn status.
- `contract_type_bar_chart.png`: Bar chart displaying the proportion of churn across different contract types.
- `correlation_heatmap_with_numbers.png`: Heatmap showcasing correlations among numerical features.

### 3.2 **Performance Metrics:**
- `model_metrics.txt`: Includes Accuracy, Precision, Recall, F1 Score, and AUC for the logistic regression model.

### 3.3 **Confusion Matrix:**
- `confusion_matrix.csv`: Tabular representation of predicted versus actual churn classifications.

### 3.4 **Saved Data and Models:**
- `training_set.RData`: The training dataset after preprocessing.
- `test_set.RData`: The test dataset after preprocessing.
- `logistic_model.RData`: The trained logistic regression model for churn prediction.

## **Future Work**
- Implement advanced models like Random Forest or Gradient Boosting.
- Explore additional customer metrics and non-linear relationships.

---

### **Author**
Developed by Oleg Rublevskii. For questions, contact: olegr50000@gmail.com.
