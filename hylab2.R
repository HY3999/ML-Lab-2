# Load necessary libraries
library(dplyr)
library(tidyr)
library(caret)

# Load student data
students_data <- read.csv("oulad-students.csv")

# Load assessment data
assessments_data <- read.csv("oulad-assessments.csv")

# Calculate mean weighted score for each student
weighted_scores <- assessments_data %>%
  group_by(id_student) %>%
  summarize(mean_weighted_score = sum(score * weight) / sum(weight))

# Join the mean weighted score with student data
students_data <- left_join(students_data, weighted_scores, by = "id_student")

# Convert final_result to binary (Fail or Not Fail)
students_data$final_result_binary <- ifelse(students_data$final_result == "Fail", 1, 0)

# Select relevant features for analysis
selected_features <- students_data %>%
  select(disability, date_registration, gender, code_module, mean_weighted_score, final_result_binary)

# Convert gender and code_module to factors
features$gender <- as.factor(features$gender)
features$code_module <- as.factor(features$code_module)

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(selected_features$final_result_binary, p = 0.8, list = FALSE)
train_data <- selected_features[train_index, ]
test_data <- selected_features[-train_index, ]

# Build logistic regression model
logistic_model <- glm(final_result_binary ~ ., data = train_data, family = "binomial")

# Make predictions using the logistic regression model
predictions <- predict(logistic_model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the performance of the model
confusion_matrix <- confusionMatrix(factor(predicted_classes), factor(test_data$final_result_binary))
f1_score <- (2 * precision * recall) / (precision + recall)
recall <- confusion_matrix$byClass["Sensitivity"]
precision <- confusion_matrix$byClass["Pos Pred Value"]
accuracy <- confusion_matrix$overall["Accuracy"]

# Print the confusion matrix and evaluation metrics
print(confusion_matrix)
print(paste("F1 Score:", f1_score))
print(paste("Recall:", recall))
print(paste("Precision:", precision))
print(paste("Accuracy:", accuracy))
