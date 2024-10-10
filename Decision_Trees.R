#+eval=FALSE

#_-----------------#_-----------------#_-----------------#_-----------------#_-----------------
#_-----------------  Decision Trees ----------------------------------------------##_-----------------
#_-----------------#_-----------------#_-----------------#_-----------------#_-----------------
wbcd_DT <- wbcd
nrow(wbcd_DT)
wbcd_DT <- subset(wbcd_DT, select = c("success","hasVideo","rating","teamSize",
                                      "hasGithub","hasReddit","capital_Raised",
                                      "minInvestment","distributedPercentage",
                                      "is_top_25","days_duration","is_Ethereum"))
head(wbcd_DT)
table(wbcd$success)
wbcd_DT$success <- factor(wbcd_DT$success)
smp_size <- floor(0.8 * nrow(wbcd_DT))
set.seed(12345)
sample(10, 5)

set.seed(12345)
train_ind <- sample(nrow(wbcd_DT), smp_size)
DT_train <- wbcd_DT[train_ind, ]
DT_test <- wbcd_DT[-train_ind, ]
DT_wbcd_train_labels <- wbcd_DT[train_ind, 1]
DT_wbcd_test_labels <- wbcd_DT[-train_ind, "success"]
prop.table(table(DT_train$success)) 
install.packages("C50")
library(C50)
library(tidyverse)
DT_model <- C5.0(success ~., DT_train)
DT_model
summary(DT_model)
print(summary(DT_model))
plot(DT_model)
DT_pred <- predict(DT_model, DT_test)
DT_predict1 <- predict(DT_model, DT_test, type = "prob" )
library(gmodels)
CrossTable(DT_pred, DT_test$success, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
confusionMatrix_DT <- confusionMatrix(DT_pred , DT_wbcd_test_labels, positive = "Y", mode = "everything")
confusionMatrix_DT
library(ROCR)
predict_object_DT <- prediction(DT_predict1[,2],DT_wbcd_test_labels)
roc_DT <- performance(predict_object_DT, measure = "tpr", x.measure = "fpr")
plot(roc_DT, main = "ROC curve for DT", col = "blue", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_DT <- performance(predict_object_DT, measure = "auc")
auc_object_DT
auc_object_DT@y.values[[1]]
#Accuracy : 0.6642

# ----------------------K-fold Validation -------------------------------------------#

library(caret)
library(C50)
# Set seed for reproducibility
set.seed(12345)
num_folds <- 10
folds <- createFolds(DT_wbcd_train_labels, k = num_folds)
accuracy_DT <- numeric(num_folds)
sensitivity_DT <- numeric(num_folds)
specificity_DT <- numeric(num_folds)
precision_DT <- numeric(num_folds)
recall_DT <- numeric(num_folds)
f1_DT <- numeric(num_folds)

for (i in 1:num_folds) {
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  fold_train_data <- wbcd_DT[train_indices, ]
  fold_test_data <- wbcd_DT[test_indices, ]
  DT_model <- C5.0(success ~ ., data = fold_train_data)
  DT_predictions <- predict(DT_model, newdata = fold_test_data)
  confusion_matrix_dt <- confusionMatrix(data = factor(DT_predictions, levels = levels(factor(fold_test_data$success))),
                                         reference = factor(fold_test_data$success), positive = "Y")
  
  # Store the evaluation metrics for the current fold
  accuracy_DT[i] <- confusion_matrix_dt$overall['Accuracy']
  sensitivity_DT[i] <- confusion_matrix_dt$byClass['Sensitivity']
  specificity_DT[i] <- confusion_matrix_dt$byClass['Specificity']
  precision_DT[i] <- confusion_matrix_dt$byClass['Precision']
  recall_DT[i] <- confusion_matrix_dt$byClass['Recall']
  f1_DT[i] <- confusion_matrix_dt$byClass['F1']
}
avg_accuracy <- mean(accuracy_DT)
avg_sensitivity <- mean(sensitivity_DT)
avg_specificity <- mean(specificity_DT)
avg_precision <- mean(precision_DT)
avg_recall <- mean(recall_DT)
avg_f1 <- mean(f1_DT)

# ----------------------------------------------------------------#

#Adaptive Boosting
DT_boost10 <- C5.0(select(DT_train, -success), DT_train$success, trials = 10)
DT_boost10
summary(DT_boost10)
view(DT_train)
DT_pred_boost10 <- predict(DT_boost10, DT_test)
DT_pred_boost10_prob <- predict(DT_boost10, DT_test, type = "prob")
CrossTable(DT_pred_boost10, DT_test$success, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('predicted default', 'actual default'))
adaboost_matrix <- confusionMatrix(as.factor(DT_pred_boost10), as.factor(DT_wbcd_test_labels), positive = "Y", mode = "everything")
adaboost_matrix

library(ROCR)
predict_object_DTBoost <- prediction(DT_pred_boost10_prob[,2],DT_wbcd_test_labels)
roc_DTBoost <- performance(predict_object_DTBoost, measure = "tpr", x.measure = "fpr")
plot(roc_DTBoost, main = "ROC curve for AdaBoost", col = "brown", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_DTBoost <- performance(predict_object_DTBoost, measure = "auc")
auc_object_DTBoost
auc_object_DTBoost@y.values[[1]]
#Accuracy : 0.6836



# ----------------------K-fold Validation for AdaBoost -------------------------------------------#
library(caret)
library(C50)
# Set seed for reproducibility
set.seed(12345)
num_folds <- 10
folds <- createFolds(DT_wbcd_train_labels, k = num_folds)
accuracy_DTBoost <- numeric(num_folds)
sensitivity_DTBoost <- numeric(num_folds)
specificity_DTBoost <- numeric(num_folds)
precision_DTBoost <- numeric(num_folds)
recall_DTBoost <- numeric(num_folds)
f1_DTBoost <- numeric(num_folds)

for (i in 1:num_folds) {
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  fold_train_data <- wbcd_DT[train_indices, ]
  fold_test_data <- wbcd_DT[test_indices, ]
  DT_model <- C5.0(success ~ ., data = fold_train_data)
  DT_predictions <- predict(DT_boost10, newdata = fold_test_data)
  confusion_matrix_dt <- confusionMatrix(data = factor(DT_predictions, levels = levels(factor(fold_test_data$success))),
                                         reference = factor(fold_test_data$success), positive = "Y")
  
  # Store the evaluation metrics for the current fold
  accuracy_DTBoost[i] <- confusion_matrix_dt$overall['Accuracy']
  sensitivity_DTBoost[i] <- confusion_matrix_dt$byClass['Sensitivity']
  specificity_DTBoost[i] <- confusion_matrix_dt$byClass['Specificity']
  precision_DTBoost[i] <- confusion_matrix_dt$byClass['Precision']
  recall_DTBoost[i] <- confusion_matrix_dt$byClass['Recall']
  f1_DTBoost[i] <- confusion_matrix_dt$byClass['F1']
}
avg_accuracy <- mean(accuracy_DTBoost)
avg_sensitivity <- mean(sensitivity_DTBoost)
avg_specificity <- mean(specificity_DTBoost)
avg_precision <- mean(precision_DTBoost)
avg_recall <- mean(recall_DTBoost)
avg_f1 <- mean(f1_DTBoost)