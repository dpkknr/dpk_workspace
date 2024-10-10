###### FORECASTING SEASONALLY ADJUSTED TIME SERIES ##################

install.packages("forecast")
install.packages("imputeTS")
install.packages("tidyverse")
library(forecast)
library(imputeTS)
library(tidyverse)
install.packages("VIM",dependencies = T)
library("VIM")

## Masking Personal Details in Directory Path
setwd("XXXXXXX/FABA") 

pce_data <- read.csv("PCE.csv")

head(pce_data)
tail(pce_data)
summary(pce_data)
nrow(pce_data)

# Visualization to understand the missing values.
aggr(pce_data, numbers=TRUE, prop=FALSE)

#Missing Data Percentage 
nrow(pce_data) #total rows
pce_row <- nrow(pce_data)
missing_values <- is.na(pce_data)
num_missing <- sum(missing_values)
print(num_missing)  # missing data 
percent_missing <- (num_missing / pce_row * 100)
print(percent_missing) #5.51% missing data %

#Convert into TS Object
pce_ipts <- ts(pce_data$PCE, start = c(1959, 1), end = c(2023, 11), frequency = 12)
PCE_TS <- pce_ipts

#Plot the Series
plot(pce_ipts,col = "blue", main ="US - PCE Time Series")
tsdisplay(PCE_TS)
checkresiduals(pce_ipts)

# Perform Imputation
library(tidyverse)
datasetComplete1<-na_interpolation(pce_data) 
datasetComplete2<-na_ma(pce_data, k=4, weighting = "exponential")
datasetComplete3<-na_kalman(pce_data)
datasetComplete4<-na_kalman(pce_data, model="auto.arima")

#Plot Series with all the imputations
install.packages("ggplot2")
library(ggplot2)
plot(pce_ipts, main = "Plot After Imputation", col = "red")
lines(pce_ipts1, col = "brown", lwd = 2)
lines(pce_ipts2, col = "green", lwd = 2)
lines(pce_ipts3, col = "orange", lwd = 2)
legend("bottomright", 
       legend = c("Na Interpolation", "Exponential","Na_Kalman","Na_Kalman Auto Arima"), 
       col = c("red","brown", "green","orange"), lty = 1, lwd = 2, cex= 0.6)



# Time Series sfter Interpolation, Exponential, Na_Kalmann Na_kalman Auto Arima.
pce_ipts <- ts(datasetComplete1$PCE, start = c(1959, 1), end = c(2023, 11), frequency = 12)
pce_ipts1 <- ts(datasetComplete2$PCE, start = c(1959, 1), end = c(2023, 11), frequency = 12)
pce_ipts2 <- ts(datasetComplete3$PCE, start = c(1959, 1), end = c(2023, 11), frequency = 12)
pce_ipts3 <- ts(datasetComplete4$PCE, start = c(1959, 1), end = c(2023, 11), frequency = 12)

plot(pce_ipts)
aggr(pce_ipts, numbers=TRUE, prop=FALSE) #No Missing Values

# Seasonally Adjusted but still it has very minimal seasonality
de <- decompose(pce_ipts, type="additive")
plot(de)

# Set Train and Test Data Set 80%-20%
train_Set_80 <- floor(length(pce_ipts)*0.8)
train_Set_80
Full_set <- nrow(pce_data)
Full_set
test_Set_20 <-  Full_set -train_Set_80
test_Set_20

train <- subset(pce_ipts,end =train_Set_80)
test <- tail(pce_ipts,Full_set-train_Set_80)


#----------------------------SIMPLE FORECASTING METHODS-------------------------------------------------------------
#Naive
fcnaive <- naive(train, h = test_Set_20)
autoplot(train) + autolayer(fcnaive)
autoplot(pce_ipts) + autolayer(fcnaive)

#seasonal Naive - No Seasonality in the TS, can be ignored
fcsnaive <- snaive(train, h = test_Set_20)
autoplot(train) + autolayer(fcsnaive)
autoplot(pce_ipts) + autolayer(fcsnaive)

#mean
fmean <- meanf(train, h= test_Set_20)
autoplot(train) + autolayer(fmean)
autoplot(pce_ipts) + autolayer(fmean)

#drift
fcdrift <- rwf(train, h = test_Set_20, drift= 'TRUE')
autoplot(train) + autolayer(fcdrift)
autoplot(pce_ipts) + autolayer(fcdrift)

Naive <- fcnaive$mean
Seasonal_Naive <- fcsnaive$mean
Mean <- fmean$mean
Drift <- fcdrift$mean

# Plot for Simple Forecasting Methods Comparison
autoplot(pce_ipts) + autolayer(Naive) + autolayer(Seasonal_Naive) + autolayer(Mean) + autolayer(Drift)+
  ggtitle("Simple Forecasting Methods Comparison")

# Accuracy of Each Simple Forecasting Methods
accuracy(fcdrift,test)
accuracy(fcnaive,test)
accuracy(fcsnaive,test)
accuracy(fmean,test)

#----------------------------_Exponential Smoothing ----------------------------------------------------------------------------#

#Simpler Exponential Smoothing are used only time series without trend and seasonality
fcses <- ses(train, h = test_Set_20)
autoplot(train) + autolayer(fcses)

#Holt Linear Method 
fcholt <- holt(train, h = 156 )
autoplot(train) + autolayer(fcholt)
accuracy(fcholt, test)
checkresiduals(fcholt)

#ETS Method
ifit_ets <- ets(train)
forecast_ets <- forecast(ifit_ets, h = test_Set_20)  
summary(forecast_ets)
accuracy(forecast_ets, test)
ETS <- forecast_ets$mean
Holt <-fcholt$mean
 
# Plot for Exponential Smoothing Comparison
autoplot(pce_ipts)+ autolayer(ETS) + autolayer(Holt)+
  ggtitle(" Exponential Smoothing Comparison")

#####------------Holt Winters Methods are used only with time series with trend and seasonality, just to check-----------
#Additive Holt Winters Method
fchw <- hw(train, h = 156)
autoplot(train) + autolayer(fchw)

#Multiplicative Holt-Winters Method
fchwm <- hw(train, h = 156, seasonal = "multiplicative")
autoplot(train) + autolayer(fchwm)
#####------------------------------------------------------------------------------------------------------------
print("Holt's Linear Exponential Model")
accuracy(fcholt, test)

print("ETS Model")
accuracy(forecast_ets, test)

#---------------------------- ARIMA Models ----------------------------------------------------------------------------#

#Auto Arima ARIMA(3,2,2)
fcauto <- auto.arima(train, seasonal = FALSE)
fcauto_arima <- forecast(fcauto, h = 156)
Arima <- fcauto_arima$mean
autoplot(pce_ipts) + autolayer (Arima) +
  ggtitle("Plot for Arima Model")

# ARIMA(3,2,3)
arima_stand <- arima(train,order=c(3,2,3))
fc_arima_stand <- forecast(arima_stand, h = 156)

# ARIMA(2,2,3)
arima_stand1 <- arima(train,order=c(2,2,3))
fc_arima_stand1 <- forecast(arima_stand1, h = 156)

#ARIMA(2,2,2)
arima_stand2 <- arima(train,order=c(2,2,2))
fc_arima_stand2 <- forecast(arima_stand2, h = 156)

accuracy(fcauto_arima,test)
accuracy(fc_arima_stand,test)
accuracy(fc_arima_stand1,test)
accuracy(fc_arima_stand2,test)
Arima_3_2_3 <- fc_arima_stand$mean
Auto_Arima <- fcauto_arima$mean
Arima_2_2_3 <-fc_arima_stand1$mean
Arima_2_2_2 <-fc_arima_stand2$mean
autoplot(pce_ipts) + autolayer (Auto_Arima)+autolayer (Arima_3_2_3)+ autolayer (Arima_2_2_3) + autolayer (Arima_2_2_2)+
  ggtitle("Arima Models Comparison")


#Comparison of Drift, Holt and Arima models.
autoplot(pce_ipts) + autolayer (Auto_Arima) + autolayer(Holt) + autolayer(Drift)+
  ggtitle("Best Models Comparison")

#Preferred Model is Exponential Smoothing - Holt's Linear Method
tail(pce_ipts)
predict_oct_24 <- holt(pce_ipts, h = 11 )
predict_oct_24
print("Estimation of the PCE expenditures for October 2024")
predict_oct_24$mean[11]
plot(predict_oct_24)
summary(predict_oct_24)
predicted_values <- predict_oct_24$mean
autoplot(pce_ipts) + autolayer (predict_oct_24$mean) + labs(color = "Predicted Values")


#---------------------------- One step Rolling Forecasting. ------------------------------------#

fcdrift <- rwf(train,drift=TRUE)		
refit_drift <- rwf(pce_ipts,model =fcdrift,drift=TRUE)
One_Step_Drift <- window(fitted(refit_drift), start=c(2010,12))
print("Drift Model")
accuracy(One_Step_Drift,test)


refit_holt <-  holt(pce_ipts, model = fcholt, use.initial.values=TRUE)
One_Step_Holt <- window(fitted(forecast(refit_holt)), start=c(2010,12))
print("Holt's Linear Exponential Model")
accuracy(One_Step_Holt,test)

fit_arima <-  auto.arima(train)
refit_arima <- Arima(pce_ipts, model=fit_arima)
One_Step_Arima <- window(fitted(refit_arima),start=c(2010,12))
print("Arima Model")
accuracy(One_Step_Arima,test)

autoplot(pce_ipts) + 
  autolayer(One_Step_Drift) + 
  autolayer(One_Step_Holt) + 
  autolayer(One_Step_Arima) +
  ggtitle(" One-Step Rolling Forecasting Comparison")