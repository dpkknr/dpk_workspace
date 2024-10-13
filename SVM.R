#+eval=FALSE
#_-----------------#_-----------------#_-----------------#_-----------------#_-----------------
#_-----------------  SVM ------------##_-----------------
#_-----------------#_-----------------#_-----------------#_-----------------#_-----------------
library(gmodels)
library(caret)
library(tidyverse)
library(ROCR)
wbcd_svm <- wbcd
wbcd_svm <- subset(wbcd_svm, select = c("success","hasVideo","rating","teamSize",
                                        "hasGithub","hasReddit","capital_Raised",
                                        "minInvestment","distributedPercentage",
                                        "is_top_25","days_duration","is_Ethereum"))

wbcd_svm$success <- factor(wbcd_svm$success)

smp_size <- floor(0.8 * nrow(wbcd_svm))
# Set Seed so that same sample can be reproduced in future
set.seed(123)
SVM_train_ind <- sample(nrow(wbcd_svm), smp_size)
SVM_train <- wbcd_svm[SVM_train_ind, ]
SVM_test <- wbcd_svm[-SVM_train_ind, ]
SVM_train_labels <- wbcd_svm[SVM_train_ind, 1]
SVM_test_labels <- wbcd_svm[-SVM_train_ind, "success"]
#simple linear SVM
install.packages("kernlab")
library(kernlab)
svm_classifier <- ksvm(success ~ ., data = SVM_train,
                       kernel = "vanilladot", prob.model = TRUE)


#---------------------------- Vanillabot SVM ---------------------------#
svm_pred <- predict(svm_classifier, select(SVM_test, -success))
SVM_predict_prob <- predict(svm_classifier, select(SVM_test, -success), type = "probabilities")
svm_results <- data.frame(actual_type = SVM_test_labels,
                          predict_type = svm_pred,
                          prob_Y = round(SVM_predict_prob[ , 2], 5),
                          prob_N = round(SVM_predict_prob[ , 1], 5))
CrossTable(svm_results$actual_type, svm_results$predict_type, 
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)
confusionMatrix(svm_results$actual_type, svm_results$predict_type, positive = "Y", mode ="everything")


pred_object_svm <- prediction(svm_results$prob_Y, svm_results$actual_type)
roc_svm <- performance(pred_object_svm, measure = "tpr", x.measure = "fpr")
plot(roc_svm, main = "ROC curve for SVM - Vanilladot ", col = "red", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_SVM <- performance(pred_object_svm, measure = "auc")
auc_SVM <- auc_object_SVM@y.values[[1]]
auc_SVM
agreement <- svm_pred == SVM_test$success
table(agreement)
prop.table(table(agreement)) 
#Accuracy : 0.6557 

# ----------------------K-fold Validation for Vanillabot -------------------------------------------#
library(caret)
library(C50)
# Set seed for reproducibility
set.seed(12345)
num_folds <- 10
folds <- createFolds(SVM_train_labels, k = num_folds)
accuracy_SVM <- numeric(num_folds)
sensitivity_SVM <- numeric(num_folds)
specificity_SVM <- numeric(num_folds)
precision_SVM <- numeric(num_folds)
recall_SVM <- numeric(num_folds)
f1_SVM <- numeric(num_folds)

for (i in 1:num_folds) {
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  fold_train_data <- wbcd_svm[train_indices, ]
  fold_test_data <- wbcd_svm[test_indices, ]
  fold_train_labels <- SVM_train_labels[train_indices]
  fold_test_labels <- SVM_train_labels[test_indices]
  SVM_model <- ksvm(success ~ ., data = fold_train_data, kernel = "vanilladot")
  SVM_predictions <- predict(SVM_model, newdata = fold_test_data)
  confusion_matrix_SVM <- confusionMatrix(data = factor(SVM_predictions, levels = levels(factor(fold_test_labels))),
                                          reference = factor(fold_test_labels), positive = "Y")
  
  # Store the evaluation metrics for the current fold
  accuracy_SVM[i] <- confusion_matrix_dt$overall['Accuracy']
  sensitivity_SVM[i] <- confusion_matrix_dt$byClass['Sensitivity']
  specificity_SVM[i] <- confusion_matrix_dt$byClass['Specificity']
  precision_SVM[i] <- confusion_matrix_dt$byClass['Precision']
  recall_SVM[i] <- confusion_matrix_dt$byClass['Recall']
  f1_SVM[i] <- confusion_matrix_dt$byClass['F1']
}

avg_accuracy <- mean(accuracy_SVM)
avg_sensitivity <- mean(sensitivity_SVM)
avg_specificity <- mean(specificity_SVM)
avg_precision <- mean(precision_SVM)
avg_recall <- mean(recall_SVM)
avg_f1 <- mean(f1_SVM)


#---------------------------- rbfdot SVM -----------------------------------------------#

set.seed(123)
svm_classifier_rbf <- ksvm(success ~ ., data = SVM_train, kernel = "rbfdot", prob.model=TRUE)
svm_predictions_rbf <- predict(svm_classifier_rbf, select(SVM_test, -success))
svm_predictions_rbf_prob <- predict(svm_classifier_rbf, select(SVM_test, -success), type = "probabilities")
svm_results_rbf <- data.frame(actual_type = SVM_test_labels,
                          predict_type = svm_predictions_rbf,
                          prob_Y = round(svm_predictions_rbf_prob[ , 2], 5),
                          prob_N = round(svm_predictions_rbf_prob[ , 1], 5))
CrossTable(svm_results_rbf$actual_type, svm_results_rbf$predict_type, dnn=c('Y', 'N'),
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)
confusionMatrix(svm_results_rbf$actual_type, svm_results_rbf$predict_type, positive = "Y",mode = "everything")
pred_object_svm_rbf <- prediction(svm_results_rbf$prob_Y, svm_results_rbf$actual_type)
roc_svm_rbf <- performance(pred_object_svm_rbf, measure = "tpr", x.measure = "fpr")
plot(roc_svm_rbf, main = "ROC curve for SVM - rbfdot", col = "green", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_SVM_rbf <- performance(pred_object_svm_rbf, measure = "auc")
auc_SVM_rbf <- auc_object_SVM@y.values[[1]]
auc_SVM_rbf
agreement_rbf <- svm_predictions_rbf == SVM_test$success
table(agreement_rbf)
prop.table(table(agreement_rbf))
#Accuracy : 0.6503  

# ----------------------K-fold Validation for Rbfdot -------------------------------------------#
library(caret)
library(C50)
# Set seed for reproducibility
set.seed(333345)
num_folds <- 10
folds <- createFolds(SVM_train_labels, k = num_folds)
accuracy_SVM_rbf <- numeric(num_folds)
sensitivity_SVM_rbf <- numeric(num_folds)
specificity_SVM_rbf <- numeric(num_folds)
precision_SVM_rbf <- numeric(num_folds)
recall_SVM_rbf <- numeric(num_folds)
f1_SVM_rbf <- numeric(num_folds)

for (i in 1:num_folds) {
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  fold_train_data <- wbcd_svm[train_indices, ]
  fold_test_data <- wbcd_svm[test_indices, ]
  fold_train_labels <- SVM_train_labels[train_indices]
  fold_test_labels <- SVM_train_labels[test_indices]
  SVM_model_rbf <- ksvm(success ~ ., data = fold_train_data, kernel = "rbfdot")
  SVM_predictions_rbf <- predict(SVM_model_rbf, newdata = fold_test_data)
  confusion_matrix_SVM_rbf <- confusionMatrix(data = factor(SVM_predictions_rbf, levels = levels(factor(fold_test_labels))),
                                          reference = factor(fold_test_labels), positive = "Y")
  
  # Store the evaluation metrics for the current fold
  accuracy_SVM_rbf[i] <- confusion_matrix_SVM_rbf$overall['Accuracy']
  sensitivity_SVM_rbf[i] <- confusion_matrix_SVM_rbf$byClass['Sensitivity']
  specificity_SVM_rbf[i] <- confusion_matrix_SVM_rbf$byClass['Specificity']
  precision_SVM_rbf[i] <- confusion_matrix_SVM_rbf$byClass['Precision']
  recall_SVM_rbf[i] <- confusion_matrix_SVM_rbf$byClass['Recall']
  f1_SVM_rbf[i] <- confusion_matrix_SVM_rbf$byClass['F1']
}

avg_accuracy <- mean(accuracy_SVM_rbf)
avg_sensitivity <- mean(sensitivity_SVM_rbf)
avg_specificity <- mean(specificity_SVM_rbf)
avg_precision <- mean(precision_SVM_rbf)
avg_recall <- mean(recall_SVM_rbf)
avg_f1 <- mean(f1_SVM_rbf)

#---------------------------- polydot SVM -----------------------------------------------#
#+eval=FALSE
set.seed(123)
svm_classifier_pd <- ksvm(success ~ ., data = SVM_train, kernel = "polydot", prob.model =TRUE)
svm_predictions_pd <- predict(svm_classifier_pd, select(SVM_test, -success))
svm_predictions_pd_prob <- predict(svm_classifier_pd, select(SVM_test, -success), type = "probabilities")
svm_results_pd <- data.frame(actual_type = SVM_test_labels,
                              predict_type = svm_predictions_pd,
                              prob_Y = round(svm_predictions_pd_prob[ , 2], 5),
                              prob_N = round(svm_predictions_pd_prob[ , 1], 5))
CrossTable(svm_results_pd$actual_type, svm_results_pd$predict_type, dnn=c('Y', 'N'),
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)
confusionMatrix(svm_results_pd$actual_type, svm_results_pd$predict_type, positive = "Y",mode = "everything")
pred_object_svm_pd <- prediction(svm_results_pd$prob_Y, svm_results_pd$actual_type)
roc_svm_pd <- performance(pred_object_svm_pd, measure = "tpr", x.measure = "fpr")
plot(roc_svm_pd, main = "ROC curve for SVM - polydot", col = "violet", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_SVM_pd <- performance(pred_object_svm_pd, measure = "auc")
auc_SVM_pd <- auc_object_SVM_pd@y.values[[1]]
auc_SVM_pd
agreement_pd <- svm_predictions_pd == SVM_test$success
table(agreement_pd)
prop.table(table(agreement_pd))

# ----------------------K-fold Validation for polydot -------------------------------------------#
library(caret)
library(C50)
# Set seed for reproducibility
set.seed(333345)
num_folds <- 10
folds <- createFolds(SVM_train_labels, k = num_folds)
accuracy_SVM_pd <- numeric(num_folds)
sensitivity_SVM_pd <- numeric(num_folds)
specificity_SVM_pd <- numeric(num_folds)
precision_SVM_pd <- numeric(num_folds)
recall_SVM_pd <- numeric(num_folds)
f1_SVM_pd <- numeric(num_folds)

for (i in 1:num_folds) {
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  fold_train_data <- wbcd_svm[train_indices, ]
  fold_test_data <- wbcd_svm[test_indices, ]
  fold_train_labels <- SVM_train_labels[train_indices]
  fold_test_labels <- SVM_train_labels[test_indices]
  SVM_model_pd <- ksvm(success ~ ., data = fold_train_data, kernel = "polydot")
  SVM_predictions_pd <- predict(SVM_model_pd, newdata = fold_test_data)
  confusion_matrix_SVM_pd <- confusionMatrix(data = factor(SVM_predictions_pd, levels = levels(factor(fold_test_labels))),
                                              reference = factor(fold_test_labels), positive = "Y")
  
  # Store the evaluation metrics for the current fold
  accuracy_SVM_pd[i] <- confusion_matrix_SVM_pd$overall['Accuracy']
  sensitivity_SVM_pd[i] <- confusion_matrix_SVM_pd$byClass['Sensitivity']
  specificity_SVM_pd[i] <- confusion_matrix_SVM_pd$byClass['Specificity']
  precision_SVM_pd[i] <- confusion_matrix_SVM_pd$byClass['Precision']
  recall_SVM_pd[i] <- confusion_matrix_SVM_pd$byClass['Recall']
  f1_SVM_pd[i] <- confusion_matrix_SVM_pd$byClass['F1']
}

avg_accuracy <- mean(accuracy_SVM_pd)
avg_sensitivity <- mean(sensitivity_SVM_pd)
avg_specificity <- mean(specificity_SVM_pd)
avg_precision <- mean(precision_SVM_pd)
avg_recall <- mean(recall_SVM_pd)
avg_f1 <- mean(f1_SVM_pd)



#---------------------------- tanhdot SVM -----------------------------------------------#

set.seed(123)
svm_classifier_th <- ksvm(success ~ ., data = SVM_train, kernel = "tanhdot", prob.model =TRUE)
svm_predictions_th <- predict(svm_classifier_th, select(SVM_test, -success))
svm_predictions_th_prob <- predict(svm_classifier_th, select(SVM_test, -success), type = "probabilities")
svm_results_th <- data.frame(actual_type = SVM_test_labels,
                             predict_type = svm_predictions_th,
                             prob_Y = round(svm_predictions_th_prob[ , 2], 5),
                             prob_N = round(svm_predictions_th_prob[ , 1], 5))
CrossTable(svm_results_th$actual_type, svm_results_th$predict_type, dnn=c('Y', 'N'),
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)
th_confusion_matrix <- confusionMatrix(svm_results_th$actual_type, svm_results_th$predict_type, positive = "Y", mode = "everything")
th_confusion_matrix
th_confusion_matrix$overall['Accuracy']

pred_object_svm_th <- prediction(svm_results_th$prob_Y, svm_results_th$actual_type)
roc_svm_th <- performance(pred_object_svm_th, measure = "tpr", x.measure = "fpr")
plot(roc_svm_th, main = "ROC curve for SVM - tanhdot", col = "orange", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_SVM_th <- performance(pred_object_svm_th, measure = "auc")
auc_SVM_th <- auc_object_SVM_th@y.values[[1]]
auc_SVM_th
agreement_th <- svm_predictions_th == SVM_test$success
table(agreement_th)
prop.table(table(agreement_th))

# ----------------------K-fold Validation for tanhdot -------------------------------------------#
library(caret)
library(C50)
# Set seed for reproducibility
set.seed(333345)
num_folds <- 10
folds <- createFolds(SVM_train_labels, k = num_folds)
accuracy_SVM_th <- numeric(num_folds)
sensitivity_SVM_th <- numeric(num_folds)
specificity_SVM_th<- numeric(num_folds)
precision_SVM_th <- numeric(num_folds)
recall_SVM_th <- numeric(num_folds)
f1_SVM_th <- numeric(num_folds)

for (i in 1:num_folds) {
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  fold_train_data <- wbcd_svm[train_indices, ]
  fold_test_data <- wbcd_svm[test_indices, ]
  fold_train_labels <- SVM_train_labels[train_indices]
  fold_test_labels <- SVM_train_labels[test_indices]
  SVM_model_th <- ksvm(success ~ ., data = fold_train_data, kernel = "tanhdot")
  SVM_predictions_th<- predict(SVM_model_th, newdata = fold_test_data)
  confusion_matrix_SVM_th <- confusionMatrix(data = factor(SVM_predictions_th, levels = levels(factor(fold_test_labels))),
                                             reference = factor(fold_test_labels), positive = "Y")
  
  # Store the evaluation metrics for the current fold
  accuracy_SVM_th[i] <- confusion_matrix_SVM_th$overall['Accuracy']
  sensitivity_SVM_th[i] <- confusion_matrix_SVM_th$byClass['Sensitivity']
  specificity_SVM_th[i] <- confusion_matrix_SVM_th$byClass['Specificity']
  precision_SVM_th[i] <- confusion_matrix_SVM_th$byClass['Precision']
  recall_SVM_th[i] <- confusion_matrix_SVM_th$byClass['Recall']
  f1_SVM_th[i] <- confusion_matrix_SVM_th$byClass['F1']
}

avg_accuracy <- mean(accuracy_SVM_th)
avg_sensitivity <- mean(sensitivity_SVM_th)
avg_specificity <- mean(specificity_SVM_th)
avg_precision <- mean(precision_SVM_th)
avg_recall <- mean(recall_SVM_th)
avg_f1 <- mean(f1_SVM_th)

