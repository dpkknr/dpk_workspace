
#+eval=FALSE
#_-----------------#_-----------------#_-----------------#_-----------------#_-----------------
#_-----------------KNN------------------------------------------------##_-----------------
#_-----------------#_-----------------#_-----------------#_-----------------#_-----------------
nrow(wbcd)

# Model Preparation with Clean Data 

wbcd_knn <- wbcd
head(wbcd_knn)
nrow(wbcd_knn)
wbcd_knn <- wbcd_knn[-1]

#We apply normalization to rescale the features to a standard range of values.
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

head(wbcd_knn)
wbcd_knn <- subset(wbcd_knn, select = c("success","hasVideo","rating","teamSize",
                                        "hasGithub","hasReddit","capital_Raised",
                                        "minInvestment","distributedPercentage",
                                        "is_top_25","days_duration","is_Ethereum"))

wbcd_DT$success <- factor(wbcd_DT$success)

head(wbcd_knn)

str(wbcd_knn)

head(wbcd_knn)
wbcd_knn_n <- as.data.frame(lapply(wbcd_knn[2:11], normalize))
nrow(wbcd_knn_n) #2743

set.seed(12345)
smp_size <- floor(0.8 * nrow(wbcd_knn))
train_ind <- sample(nrow(wbcd_knn), smp_size)
wbcd_train <- wbcd_knn_n[train_ind, ]
wbcd_test<- wbcd_knn_n[-train_ind, ]
nrow(wbcd_train)
nrow(wbcd_test)

wbcd_train_labels <- wbcd_knn[train_ind, 1]
wbcd_test_labels <- wbcd_knn[-train_ind, "success"]


table(wbcd_test_labels)
library(class)

K = 45 #training size = 2194 so we try its square root 46 as the value of k first
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=K)

library(gmodels)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
confusionMatrix_KNN <- confusionMatrix(data = factor(wbcd_test_pred, levels = levels(factor(wbcd_test_labels))),
                                       reference = factor(wbcd_test_labels),positive = "Y",
                                       mode = "everything")


confusionMatrix_KNN 


install.packages("ROCR")
library(ROCR)

accuracy_knn <- confusionMatrix_KNN$overall['Accuracy']
sensitivity_knn <- confusionMatrix_KNN$byClass['Sensitivity'] # is also called recall
specificity_knn <- confusionMatrix_KNN$byClass['Specificity']
precision_knn <- confusionMatrix_KNN$byClass['Precision'] # Precision is also called Positive Predictive Value (PPV)
recall_knn <- confusionMatrix_KNN$byClass['Recall']
f1_knn <- confusionMatrix_KNN$byClass['F1']

#_-----------------#_----------------- ROC Curve -----------------#_-----------------#_-----------------
wbcd_test_pred
wbcd_train_labels_knn <- factor(wbcd_train_labels)
wbcd_KNN_pred_prob <- predict(caret::knn3(wbcd_train, wbcd_train_labels_knn, k = K), wbcd_test)

wbcd_KNN_results <- data.frame(actual_type = wbcd_test_labels,
                               predict_type = wbcd_test_pred,
                               prob_Yes = round(wbcd_KNN_pred_prob[ , 2], 5),
                               prob_No = round(wbcd_KNN_pred_prob[ , 1], 5))
pred_object <- prediction(wbcd_KNN_results$prob_Yes, wbcd_KNN_results$actual_type)
roc_KNN <- performance(pred_object, measure = "tpr", x.measure = "fpr")
plot(roc_KNN, main = "ROC curve for KNN Model", col = "pink", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_KNN <- performance(pred_object, measure = "auc")
auc_object_KNN
auc_object_KNN@y.values[[1]]



#Choose the best k 
#10-fold cross-validation to find the optimal value of k that maximizes the model's performance on the training data
train_control <- trainControl(method = "cv", number = 10) 
knn_model_cv <- train(x = wbcd_train[, -1],
                      y = wbcd_train_labels,
                      method = "knn",
                      trControl = train_control,
                      tuneGrid = data.frame(k=seq(5,100,by=5)))
pred_knn_k <- predict(knn_model_cv,wbcd_test )
plot(knn_model_cv)
print(knn_model_cv)

K=65
wbcd_test_pred_65 <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=K)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_65, prop.chisq=FALSE)
confusionMatrix_KNN_65 <- confusionMatrix(data = factor(wbcd_test_pred_65, levels = levels(factor(wbcd_test_labels))),
                                          reference = factor(wbcd_test_labels),positive = "Y")

confusionMatrix_KNN_65 <- confusionMatrix(data = factor(wbcd_test_pred_65, levels = levels(factor(wbcd_test_labels))),
                                          reference = factor(wbcd_test_labels),positive = "Y",
                                          mode= "everything")
confusionMatrix_KNN_65

#0.6648  

#_-----------------#_----------------- ROC Curve for k= 65-----------------#_-----------------#_-----------------
wbcd_test_pred
wbcd_train_labels_knn <- factor(wbcd_train_labels)
wbcd_KNN_pred_prob_65 <- predict(caret::knn3(wbcd_train, wbcd_train_labels_knn, k = K), wbcd_test)

wbcd_KNN_results_65 <- data.frame(actual_type = wbcd_test_labels,
                               predict_type = wbcd_test_pred,
                               prob_Yes = round(wbcd_KNN_pred_prob_65[ , 2], 5),
                               prob_No = round(wbcd_KNN_pred_prob_65[ , 1], 5))
pred_object_65 <- prediction(wbcd_KNN_results_65$prob_Yes, wbcd_KNN_results_65$actual_type)
roc_KNN_65 <- performance(pred_object_65, measure = "tpr", x.measure = "fpr")
plot(roc_KNN_65, main = "ROC curve for KNN Model, K=65", col = "green", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_KNN_65 <- performance(pred_object_65, measure = "auc")
auc_object_KNN_65
auc_object_KNN_65@y.values[[1]]



#----------- K fold Validation on k  = 65 --------------------#

library(caret)
num_folds <- 10
folds <- createFolds(wbcd_knn$success, k = num_folds)
accuracy <- numeric(num_folds)
sensitivity <- numeric(num_folds)
specificity <- numeric(num_folds)
precision <- numeric(num_folds)
recall <- numeric(num_folds)
f1 <- numeric(num_folds)

# Iterate over each fold
for (i in 1:num_folds) {
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  wbcd_train <- wbcd_knn_n[train_indices, ]
  wbcd_test <- wbcd_knn_n[test_indices, ]
  wbcd_train_labels <- wbcd_knn[train_indices, "success"]
  wbcd_test_labels <- wbcd_knn[test_indices, "success"]

  knn_model <- knn(train = wbcd_train[, -1], test = wbcd_test[, -1], cl = wbcd_train_labels, k = 65)
   confusion_matrix <- confusionMatrix(data = factor(knn_model, levels = levels(factor(wbcd_test_labels))),
                                        reference = factor(wbcd_test_labels), positive = "Y")

   accuracy[i] <- confusion_matrix$overall['Accuracy']
   sensitivity[i] <- confusion_matrix$byClass['Sensitivity']
   specificity[i] <- confusion_matrix$byClass['Specificity']
   precision[i] <- confusion_matrix$byClass['Precision']
   recall[i] <- confusion_matrix$byClass['Recall']
   f1[i] <- confusion_matrix$byClass['F1']
}

avg_accuracy <- mean(accuracy)
avg_sensitivity <- mean(sensitivity)
avg_specificity <- mean(specificity)
avg_precision <- mean(precision)
avg_recall <- mean(recall)
avg_f1 <- mean(f1)



