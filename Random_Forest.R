
#+eval=FALSE
#_-----------------#_-----------------#_-----------------#_-----------------#_-----------------
#_-----------------  Random Forest - Regression Tree Ensemble Model ------------##_-----------------
#_-----------------#_-----------------#_-----------------#_-----------------#_-----------------
install.packages("randomForest")
library(randomForest)
library(caret)
library(pROC)
library(gmodels)
library(caret)
library(tidyverse)
library(ROCR)
wbcd_RF <- wbcd
wbcd_RF <- subset(wbcd_RF, select = c("success","hasVideo","rating","teamSize",
                                    "hasGithub","hasReddit","capital_Raised",
                                    "minInvestment","distributedPercentage",
                                    "is_top_25","days_duration","is_Ethereum"))
head(wbcd_RF)
#wbcd_RF <- wbcd_cleaned
wbcd_RF$success <- factor(wbcd_RF$success)

RF_train_index <- createDataPartition(wbcd_RF$success, p = 0.8, list = FALSE)
RF_train_data <- wbcd_RF[RF_train_index, ]
RF_test_data <- wbcd_RF[-RF_train_index, ]
RF_train_data_labels <- wbcd_RF[RF_train_index, 1]
RF_test_data_labels <- wbcd_RF[-RF_train_index, "success"]

set.seed(123) # for reproducibility
rf_model <- randomForest(success ~ ., data = RF_train_data, ntree = 500, mtry = 4)
rf_predict <- predict(rf_model, RF_test_data)
rf_predict1 <- predict(rf_model, RF_test_data, type = "prob" )
summary(rf_predict)
tree_details <- getTree(rf_model, labelVar = TRUE)
tree_details
plot(rf_model)
importance(rf_model)
print(summary(rf_model))
#plot(rf_model, main="Error Rate by Class", col=c("blue", "red"))
library(gmodels)
CrossTable(x = RF_test_data_labels, y = rf_predict, prop.chisq=FALSE)
summary(rf_predict)
confusionMatrix_RF <- confusionMatrix(rf_predict , RF_test_data_labels, positive = "Y", mode = "everything")
confusionMatrix_RF
#Accuracy : 0.6861  

accuracy_RF <- confusionMatrix_RF$overall['Accuracy']
sensitivity_RF <- confusionMatrix_RF$byClass['Sensitivity'] # is also called recall
specificity_RF <- confusionMatrix_RF$byClass['Specificity']
precision_RF <- confusionMatrix_RF$byClass['Precision'] # Precision is also called Positive Predictive Value (PPV)
recall_RF <- confusionMatrix_RF$byClass['Recall']
f1_RF <- confusionMatrix_RF$byClass['F1']

RF_test_prob <- predict(rf_model, RF_test_data, type = "prob")
RF_results <- data.frame(actual_type = RF_test_data_labels,
                          predict_type = rf_predict,
                          prob_Y = round(RF_test_prob[ , 2], 5),
                          prob_N = round(RF_test_prob[ , 1], 5))
sensitivity(RF_results$predict_type, RF_results$actual_type, positive = "Y")
specificity(RF_results$predict_type, RF_results$actual_type, negative = "N")
posPredValue(RF_results$predict_type, RF_results$actual_type, positive = "Y") # this is precision
sensitivity(RF_results$predict_type, RF_results$actual_type, positive = "Y") # this is recall


#------------ ROC ---------
predict_object_RF <- prediction(rf_predict1[,2],RF_test_data_labels)
roc_RF <- performance(predict_object_RF, measure = "tpr", x.measure = "fpr")
plot(roc_RF, main = "ROC curve for Random Forest Model", col = "blue", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_RF <- performance(predict_object_RF, measure = "auc")
auc_object_RF
auc_object_RF@y.values[[1]]







wbcd_train[, -1]

#----------- K fold Validation on RF --------------------#

library(caret)
num_folds <- 10
folds <- createFolds(wbcd_RF$success, k = num_folds)
accuracy_RF <- numeric(num_folds)
sensitivity_RF <- numeric(num_folds)
specificity_RF <- numeric(num_folds)
precision_RF <- numeric(num_folds)
recall_RF <- numeric(num_folds)
f1_RF <- numeric(num_folds)

# Iterate over each fold
for (i in 1:num_folds) {
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  RF_train_data <- wbcd_RF[test_indices, ]
  RF_test_data <- wbcd_RF[-test_indices, ]
  RF_train_data_labels <- wbcd_RF[test_indices, 1]
  RF_test_data_labels <- wbcd_RF[-test_indices, "success"]
  RF_model <- randomForest(success ~ ., data = RF_train_data, ntree = 500)
  RF_predictions <- predict(RF_model, newdata = RF_test_data)
  confusion_matrix_rf <- confusionMatrix(data = factor(RF_predictions, levels = levels(factor(RF_test_data_labels))),
                                        reference = factor(RF_test_data_labels), positive = "Y")
  
  accuracy_RF[i] <- confusion_matrix_rf$overall['Accuracy']
  sensitivity_RF[i] <- confusion_matrix_rf$byClass['Sensitivity']
  specificity_RF[i] <- confusion_matrix_rf$byClass['Specificity']
  precision_RF[i] <- confusion_matrix_rf$byClass['Precision']
  recall_RF[i] <- confusion_matrix_rf$byClass['Recall']
  f1_RF[i] <- confusion_matrix_rf$byClass['F1']
}

avg_accuracy <- mean(accuracy_RF)
avg_sensitivity <- mean(sensitivity_RF)
avg_specificity <- mean(specificity_RF)
avg_precision <- mean(precision_RF)
avg_recall <- mean(recall_RF)
avg_f1 <- mean(f1_RF)

