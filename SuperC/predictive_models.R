##########################################################
# Joan Puigdomenech i Joel Romia
##########################################################
library(caret)
library(e1071)
library(randomForest)
library(nnet)

# Step 1: Analyze the problem and inspect the data
train_data <- read.csv("train.csv")
test_data <- read.csv("unique_m.csv")

# Step 2: Preprocessing and resampling methodology
# Splitting the data into features and target variable
X_train <- train_data[, -81]  # Exclude the "critical_temp" column
y_train <- train_data$critical_temp

# Perform standardization on the features
preproc <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preproc, newdata = X_train)

# Convert the scaled data back to a data frame
X_train_scaled <- as.data.frame(X_train_scaled)
X_train_scaled$critical_temp <- y_train

# Update column names of test data
colnames(test_data) <- colnames(X_train_scaled)[-ncol(X_train_scaled)]

# Step 3: Create predictive models and find the best hyperparameters
# Support Vector Machines (SVM)
svm_model <- svm(critical_temp ~ ., data = X_train_scaled, epsilon=0.01,gamma=10)
svm_predictions <- predict(svm_model, newdata = test_data)

# Random Forest (RF)
rf_model <- randomForest(critical_temp ~ ., data = X_train_scaled, ntree=100, proximity=FALSE)
rf_predictions <- predict(rf_model, newdata = test_data)

# Neural Networks (NN)
nn_model <- nnet(critical_temp ~ ., data = X_train_scaled, size = 10)
nn_predictions <- predict(nn_model, newdata = test_data)

# Print the results
cat("Support Vector Machines (SVM):\n")
cat("Predicted critical_temp:", svm_predictions, "\n\n")

cat("Random Forest (RF):\n")
cat("Predicted critical_temp:", rf_predictions, "\n\n")

cat("Neural Networks (NN):\n")
cat("Predicted critical_temp:", nn_predictions, "\n")
