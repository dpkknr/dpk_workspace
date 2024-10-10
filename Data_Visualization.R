#+eval=FALSE
install.packages("skimr")
library(skimr)
library(dplyr)
install.packages("caret")
library(caret)

library(dplyr)
library(tidyr)
library(ggplot2)

rm(list=ls())
setwd("/Users/deepu/Documents/ML")
wbcd <- read.csv('LUBS5990M_courseworkData_2324.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')
str(wbcd)
wbcd_raw <- wbcd #To store the raw data
head(wbcd)

install.packages("Hmisc")
library(Hmisc)
describe(wbcd)

#wbcd_raw$success <- ifelse(wbcd_raw$success == "Yes", 1, 0)
#wbcd$success <- ifelse(wbcd_raw$success == "Yes", 1, 0)

#------------------------------- success ---------------------------------------------------------------------#
pie(prop.table(table(wbcd$success)), 
    labels = paste(names(table(wbcd$success)), ": ", round(prop.table(table(wbcd$success)) * 100, 1), "% (Count:", table(wbcd$success), ")"), 
    main = "Pie Chart of Success", 
    col = rainbow(length(table(wbcd$success))),
    cex = 0.8) 



# Create a bar plot
barplot(prop.table(table(wbcd$success)), 
        main = "Bar Plot of Cleaned Data", 
        col = rainbow(length(table(wbcd$success))), 
        xlab = "Success", 
        ylab = "Proportion",
        ylim = c(0, 1))

# Add labels to the bars
text(x = 1:length(table(wbcd$success)), 
     y = prop.table(table(wbcd$success)), 
     labels = paste(names(table(wbcd$success)), ": ", 
                    round(prop.table(table(wbcd$success)) * 100, 1), 
                    "% (Count:", table(wbcd$success), ")"),
     cex = 0.8, 
     pos = 3)
#------------------------------- brandslogan ---------------------------------------------------------------------#
install.packages('topicmodels')
install.packages('wordcloud')
library(topicmodels)
library(wordcloud)
install.packages("tm")
install.packages("tokenizers")
library(tm)
library(tokenizers)
head(wbcd)
myCorpus <- Corpus(VectorSource(wbcd$brandSlogan))
dtmdocs <- DocumentTermMatrix(myCorpus,
                              control = list(lemma=TRUE,
                                             removePunctuation = TRUE,
                                             removeNumbers = TRUE,
                                             stopwords = TRUE,
                                             tolower = TRUE))
dtm.new <- as.matrix(dtmdocs)
frequency <- colSums(dtm.new)
frequency <- sort(frequency, decreasing=TRUE)
doc_length <- rowSums(dtm.new)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:25], rot.per=0.15, 
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))


#------------------------------- TeamSize ---------------------------------------------------------------------#

hist(wbcd$teamSize, main = "Histogram of Team Size",xlab = "Team Size", ylab = "Frequency")
median(wbcd$teamSize, na.rm = TRUE)
mean(wbcd$teamSize, na.rm = TRUE)
sum(!complete.cases(wbcd$teamSize)) 

#------------------------------- Rating ---------------------------------------------------------------------#
hist(wbcd$rating, main = "Histogram of Ratings",xlab = "Ratings", ylab = "Frequency")
sum(!complete.cases(wbcd$rating)) 
mean(wbcd$rating, na.rm = TRUE)


#------------------------------- CountryRegion ---------------------------------------------------------------------#
# Create a bar plot for the top 10 countryRegion
barplot(sort(table(wbcd$countryRegion), decreasing = TRUE)[1:10], 
        main = "Top 10 Country Regions", 
        xlab = "Country Region", 
        ylab = "Count",
        col = rainbow(length(sort(table(wbcd$countryRegion), decreasing = TRUE)[1:10])))  

sort(table(wbcd$countryRegion), decreasing = TRUE)[1:10]
table(wbcd$countryRegion)

#------------------------------- Platform ---------------------------------------------------------------------#
barplot(sort(table(wbcd$platform), decreasing = TRUE)[1:10], 
        main = "Top 10 Platforms", 
        xlab = "Platform", 
        ylab = "Count",
        col = rainbow(length(sort(table(wbcd$platform), decreasing = TRUE)[1:10])))  
table(wbcd$platform)

#------------------------------- priceUSD ---------------------------------------------------------------------#
boxplot(wbcd$priceUSD,
        main = "Boxplot of priceUSD",
        xlab = "priceUSD")
sum(!complete.cases(wbcd$priceUSD)) 
max(wbcd$priceUSD,na.rm = TRUE)
max(wbcd$priceUSD,na.rm = TRUE)


#------------------------------- minInvestment ---------------------------------------------------------------------#
ggplot(wbcd)+geom_histogram(aes(minInvestment,fill=success))
sum(!complete.cases(wbcd$minInvestment)) 
table(wbcd$minInvestment, wbcd$success)

#------------------------------- distributedPercentage ---------------------------------------------------------------------#
boxplot(wbcd$distributedPercentage,
        main = "Boxplot of distributedPercentage",
        xlab = "distributedPercentage")
subset(wbcd, distributedPercentage > 1)

#------------------------------- coinNum ---------------------------------------------------------------------#
boxplot(wbcd_raw$coinNum,
        main = "Boxplot of coinNum",
        xlab = "coinNum")
max(wbcd_raw$coinNum)
sum(!complete.cases(wbcd$coinNum)) 

#------------------------------- hasVideo, hasGithub, hasReddit -------------------------------------------#
ggplot(wbcd)+geom_histogram(aes(hasVideo,fill=success))
ggplot(wbcd)+geom_histogram(aes(hasGithub,fill=success))
ggplot(wbcd)+geom_histogram(aes(hasReddit,fill=success))

table(wbcd$hasVideo, wbcd$success)
table(wbcd$hasGithub, wbcd$success)
table(wbcd$hasReddit, wbcd$success)

sum(!complete.cases(wbcd$hasVideo)) 
sum(!complete.cases(wbcd$hasGithub)) 
sum(!complete.cases(wbcd$hasReddit)) 

#-------------------------------#---------------days_duration---------#-------------------------------#---------------------------#


wbcd <- wbcd %>%
  mutate(days_duration = 
           as.numeric(difftime(as.Date(wbcd$endDate,format = "%d/%m/%Y"),
                               as.Date(wbcd$startDate, format = "%d/%m/%Y"), units = "days")))

sum(!complete.cases(wbcd$days_duration)) 
sum(wbcd$days_duration < 0) 
max(wbcd$days_duration)   #3722
min(wbcd$days_duration) #-203
sum(!complete.cases(wbcd$days_duration)) 


boxplot(wbcd$days_duration,
        main = "Boxplot of days_duration",
        xlab = "days_duration")


#-------------------------------#------------------------------#-------------------------------#---------------------------#


install.packages("corrgram")
library(corrgram)
corrgram(wbcd_raw) #need to make the dependent variable numeric TBD
summary(wbcd_raw)
wbcd_old_raw <- wbcd_raw

corrgram(wbcd_raw) 
wbcd_raw <- wbcd_raw[-1]
corrgram(wbcd_raw) 
corrgram(wbcd_cleaned)
wbcd_cleaned_raw <- wbcd_cleaned
wbcd_cleaned <- wbcd
#wbcd_cleaned_raw$success <- ifelse(wbcd_cleaned_raw$success == "Yes", 1, 0)
wbcd_cleaned$success <- ifelse(wbcd_cleaned$success == "Yes", 1, 0)
wbcd_old_raw$success <- ifelse(wbcd_old_raw$success == "Yes", 1, 0)
wbcd_cleaned <- wbcd_cleaned[-1]
corrgram(wbcd_cleaned)
#wbcd_cleaned <- subset(wbcd_cleaned, select = -is_top_6)

head(wbcd_cleaned)



