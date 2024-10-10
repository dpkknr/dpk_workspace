#+eval=FALSE
wbcd <- wbcd[-1]
head(wbcd)
skim(wbcd) #to get the complete summary of the dataset 

table(wbcd$success)
table(wbcd$platform)

library(dplyr)

wbcd <- read.csv('LUBS5990M_courseworkData_2324.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')
wbcd_raw <- read.csv('LUBS5990M_courseworkData_2324.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')


#that the target feature is coded as a factor
#wbcd$success <- factor(wbcd$success, levels = c("N", "Y"), 
#                       labels = c("No", "Yes"))

head(wbcd)

round(prop.table(table(wbcd$success)) * 100, digits = 1)
table(wbcd$success)
table(wbcd$countryRegion)
names(sort(table(wbcd$countryRegion), decreasing = TRUE))[1:5]

#------------------------------- Country ---------------------------------------------------------------------#
wbcd$countryRegion <- gsub("Curacao", "Curaçao", wbcd$countryRegion, ignore.case = TRUE)
wbcd$countryRegion <- gsub("SINGAPORE", "Singapore", wbcd$countryRegion, ignore.case = TRUE)
wbcd$countryRegion <- gsub("usa", "USA", wbcd$countryRegion, ignore.case = TRUE)
wbcd$countryRegion <- gsub("india", "India", wbcd$countryRegion, ignore.case = TRUE)

wbcd$is_top_25 <- ifelse(wbcd$countryRegion %in% c("USA", "UK", "Switzerland","Russia",
                                                   "Singapore","Lithuania","Australia",
                                                   "Gibraltar","Germany","Canada",
                                                  "Israel","Ukraine", "France","Spain",
                                                  "Poland","Liechtenstein","China",
                                                  "Luxembourg",
                                                  "Costa Rica",
                                                  "Argentina",
                                                  "Serbia",
                                                  "Slovakia",
                                                  "Slovenia",
                                                  "Sweden"
                                                  ), 1, 0)




#------------------------------- Days Duration ---------------------------------------------------------------------#
wbcd <- wbcd %>%
  mutate(days_duration = 
           as.numeric(difftime(as.Date(wbcd$endDate,format = "%d/%m/%Y"),
                               as.Date(wbcd$startDate, format = "%d/%m/%Y"), units = "days")))

summary(wbcd$days_duration)
table(wbcd$days_duration)

#------------------------------- Platform Cleansing ---------------------------------------------------------------------#

wbcd$platform <- gsub("Ethereum", "Ethereum", wbcd$platform, ignore.case = TRUE)
wbcd$platform <- gsub("Ethererum", "Ethereum", wbcd$platform, ignore.case = TRUE)
wbcd$platform <- gsub("Ethereum, Waves", "Ethereum", wbcd$platform, ignore.case = TRUE)
wbcd$platform <- gsub("Etherum", "Ethereum", wbcd$platform, ignore.case = TRUE)
wbcd$platform <- gsub("Ethereum   ", "Ethereum", wbcd$platform, ignore.case = TRUE)
wbcd$platform <- gsub("Ethereum ", "Ethereum", wbcd$platform, ignore.case = TRUE)
wbcd$platform <- gsub("Ethereum ", "Ethereum", wbcd$platform, ignore.case = TRUE)
wbcd$platform <- gsub("Ethereum ", "Ethereum", wbcd$platform, ignore.case = TRUE)
wbcd$platform <- gsub(" Ethereum", "Ethereum", wbcd$platform, ignore.case = TRUE)

# Made 1 as ethereum
head(wbcd)
sum(complete.cases(wbcd)) #2442
sum(!complete.cases(wbcd)) #325
#2767

wbcd$is_Ethereum <- 0
wbcd$is_Ethereum <- ifelse(grepl("Ethereum", gsub(" ", "", wbcd$platform)), 1, 0)
table(wbcd$platform, wbcd$is_Ethereum)
table(wbcd$is_Ethereum,wbcd$success)

ggplot(wbcd)+geom_histogram(aes(is_Ethereum))

#------------------------------- Days duration remove less than 0 ---------------------------------------------------------------------#
wbcd <- subset(wbcd, days_duration >=0) #removed 12 data less than 0
max(wbcd$days_duration, na.rm = TRUE)

#removed outlier
wbcd <- subset(wbcd, days_duration < 3722)

#------------------------------- Removed distributedPercentage > 1 ---------------------------------------------------------------------#
wbcd <- subset(wbcd, distributedPercentage <= 1) ##removed all data less than 1.00 %

#------------------------------- Removed priceUSD oulier ---------------------------------------------------------------------#

nrow(subset(wbcd, priceUSD != 39384 | is.na(priceUSD)))

wbcd_usd <- subset(wbcd, priceUSD == 0)
table(wbcd_usd$success,wbcd_usd$priceUSD)

wbcd <- subset(wbcd, priceUSD != 39384 | is.na(priceUSD)) #removed 1 more record with this extreme outlier)
nrow(wbcd) #2743

#------------------------------- Imputation- PriceUSD ---------------------------------------------------------------------#

mean_priceUSD <- mean(wbcd$priceUSD, na.rm = TRUE)
mean_priceUSD
wbcd$priceUSD <- ifelse(is.na(wbcd$priceUSD), mean_priceUSD, wbcd$priceUSD)


#Before Imputation
hist(wbcd_raw$priceUSD, main = "Histogram of PriceUSD",xlab = "Team Size", ylab = "Frequency")
hist(wbcd_raw$teamSize, main = "Histogram of TeamSize",xlab = "Team Size", ylab = "Frequency")

nrow(wbcd)
nrow(wbcd_raw)


#------------------------------- Imputation- TeamSize ---------------------------------------------------------------------#

mean(wbcd$teamSize, na.rm = TRUE)
median(wbcd$teamSize, na.rm = TRUE)
max(wbcd$teamSize, na.rm = TRUE)
min(wbcd$teamSize, na.rm = TRUE)

median_teamSize <- median(wbcd$teamSize, na.rm = TRUE)
wbcd$teamSize <- ifelse(is.na(wbcd$teamSize), median_teamSize, wbcd$teamSize)
  #Median imputed It preserves the overall central tendency of the data.

#After Imputation
hist(wbcd$priceUSD, main = "Histogram of PriceUSD after Imputation",xlab = "Team Size", ylab = "Frequency")
hist(wbcd$teamSize, main = "Histogram of TeamSize after Imputation",xlab = "Team Size", ylab = "Frequency")

#------------------------------- Clean coinNUM ---------------------------------------------------------------------#

wbcd <- wbcd[wbcd$coinNum != 22619078416760300, ] #2743 cleaned
#removed 1 extreme value from coinnum

#------------ Capital Raising ----------------- #

wbcd$capital_Raised <- wbcd$priceUSD * wbcd$coinNum
head(wbcd)

ggplot(wbcd)+geom_histogram(aes(is_top_25,fill=success))

pie(prop.table(table(wbcd$is_top_25)), 
    labels = paste(names(table(wbcd$is_top_25)), ": ", round(prop.table(table(wbcd$is_top_25)) * 100, 1), "% (Count:", table(wbcd$is_top_25), ")"), 
    main = "Pie Chart of Top 25 Countries", 
    col = rainbow(length(table(wbcd$is_top_25))),
    cex = 0.8) 

mean(wbcd$capital_Raised, na.rm = TRUE)
median(wbcd$capital_Raised, na.rm = TRUE)
max(wbcd$capital_Raised, na.rm = TRUE)
min(wbcd$capital_Raised, na.rm = TRUE)
sum(!complete.cases(wbcd$capital_Raised)) 



#remove a column
#wbcd <- wbcd[, !colnames(wbcd) %in% c("is_top_5")]




raw_data <- nrow(wbcd_raw)
cleaned_data <- nrow(wbcd)

#------------------------------- Percentage of Deleted Data ---------------------------------------------------------------------#

percent_deleted <- ((raw_data - cleaned_data)/raw_data)*100
percent_deleted #0.907% was cleaned to prepare data for ML processing 



#----------------- Part 2  -----------------------
install.packages("VIM",dependencies = T)
library("VIM")
aggr(wbcd_raw, numbers=TRUE, prop=FALSE)
aggr(wbcd, numbers=TRUE, prop=FALSE)


nrow(wbcd)
write.csv(wbcd, file = '/Users/deepu/Documents/ML/wbcd.csv', row.names = FALSE)
write.csv(wbcd, row.names = FALSE)

wbcd <-wbcd_cleaned
head(wbcd_cleaned)
head(wbcd)

wbcd_cleaned <- wbcd #Master Copy


wbcd_cleaned <- wbcd_cleaned[-1]
wbcd_cleaned$success <- ifelse(wbcd_cleaned$success == "Yes", 1, 0)

corrgram(wbcd_cleaned)


wbcd_model <- subset(wbcd, select = c("success","hasVideo","rating","teamSize",
                                      "hasGithub","hasReddit","capital_Raised",
                                      "minInvestment","distributedPercentage",
                                      "is_top_25","days_duration","is_Ethereum"))





library(Hmisc)
describe(wbcd_cleaned)
