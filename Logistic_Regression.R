#+eval=FALSE
#_-----------------Logistic Regression------------------------------------------------##_-----------------
#_-----------------#_-----------------#_-----------------#_-----------------#_-----------------
wbcd_LR <- wbcd

wbcd_LR <- subset(wbcd_LR, select = c("success","hasVideo","rating","teamSize",
                                    "hasGithub","hasReddit","capital_Raised",
                                    "minInvestment","distributedPercentage",
                                    "is_top_25","days_duration","is_Ethereum"))
table(wbcd_LR$success)
wbcd_LR$success <- ifelse(wbcd_LR$success == "Y", 1, 0)
set.seed(123) 
train_indices <- createDataPartition(wbcd_LR$success, p = 0.8, list = FALSE)  # 80% train, 20% test
LR_train_data <- wbcd_LR[train_indices, ]
LR_test_data <- wbcd_LR[-train_indices, ]
LR_test_data_labels <- wbcd_LR[-RF_train_index, 1]
logistic_model <- glm(success ~ hasVideo + rating + teamSize + hasGithub + 
                        hasReddit + capital_Raised + minInvestment + distributedPercentage + 
                        is_top_25 + days_duration + is_Ethereum,
                      data = LR_train_data, family = binomial)
table(LR_train_data$success)
summary(logistic_model)
par(pty = "s")
roc(LR_train_data$success, logistic_model$fitted.values, plot = TRUE,legacy.axes = TRUE, 
    xlab="False Positive Rate", ylab = "True Positive Rate", col="blue", main = "ROC Curve for Success")
summary(logistic_model)
lr_pred <- predict(logistic_model, newdata = LR_test_data, type = "response")
lr_pred_binary <- ifelse(lr_pred > 0.2, 1, 0)
cross_table <- table(Actual = LR_test_data$success, Predicted = lr_pred_binary)
print(cross_table)
confusion_matrix_LR <- confusionMatrix(as.factor(LR_test_data$success),as.factor(lr_pred_binary), mode ="everything", positive = "1")
confusion_matrix_LR


#######----------------- K-Fold Cross Validation for Logistic Regression --------

K <- 10
accuracy <- numeric(K)
sensitivity <- numeric(K)
specificity <- numeric(K)
precision <- numeric(K)
f1 <- numeric(K)
auc <- numeric(K)

for (i in 1:K) {
  validation_indices <- ((i - 1) * nrow(LR_train_data) / K + 1):(i * nrow(LR_train_data) / K)
  validation_set <- LR_train_data[validation_indices, ]
  training_set <- LR_train_data[-validation_indices, ]
  logistic_model <- glm(success ~ hasVideo + rating + teamSize + hasGithub + 
                          hasReddit + capital_Raised + minInvestment + distributedPercentage + 
                          is_top_25 + days_duration + is_Ethereum,
                        data = training_set, family = binomial)
  predicted_probs <- predict(logistic_model, newdata = validation_set, type = "response")
  predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
  confusion_matrix <- table(predicted_classes, validation_set$success)
  TP <- confusion_matrix[2, 2]
  TN <- confusion_matrix[1, 1]
  FP <- confusion_matrix[1, 2]
  FN <- confusion_matrix[2, 1]
  
  accuracy[i] <- (TP + TN) / sum(confusion_matrix)
  sensitivity[i] <- TP / (TP + FN)
  specificity[i] <- TN / (TN + FP)
  precision[i] <- TP / (TP + FP)
  f1[i] <- 2 * (precision[i] * sensitivity[i]) / (precision[i] + sensitivity[i])
  auc[i] <- roc(response = validation_set$success, predictor = predicted_probs)$auc
}
mean_accuracy <- mean(accuracy)
mean_sensitivity <- mean(sensitivity)
mean_specificity <- mean(specificity)
mean_precision <- mean(precision)
mean_f1 <- mean(f1)
mean_auc <- mean(auc)


