################### TEXT ANALYTICS AND TOPIC MODELLING ####################################

rm(list = ls())
install.packages("tm")
install.packages("tokenizers")
install.packages("dplyr")
install.packages("syuzhet")
install.packages("tidyverse")
install.packages('RColorBrewer')
install.packages('topicmodels')
install.packages('wordcloud')
install.packages("servr")
install.packages("LDAvis")
install.packages("ggplot2")
install.packages("rmarkdown")
install.packages("ldatuning")
library(tm)
library(tokenizers)
library(dplyr)
library(syuzhet)
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(topicmodels)
library(LDAvis)
library(servr)
library(ldatuning)

## Masking Personal Details in Directory Path
setwd("XXXXXXX/FABA") 

#Read the CSV HotelsData, from the working directory
hotel_data_raw <- read.csv('HotelsData.csv', stringsAsFactors = FALSE)
head(hotel_data_raw)
num_records <- nrow(hotel_data_raw)

#Total number of observations
num_records 

#Describes the dataframe
str(hotel_data_raw) 

#Summary of the df
summary(hotel_data_raw) 

# To get the review ratings distribution in the data set
review_counts <- hotel_data_raw %>%  count(Review.score)
head(review_counts)
review_counts_df <- as.data.frame(review_counts)
review_counts_df


# Visualise the review frequency distribution
color_palette <- c("1" = "red", "2" = "orange", "3" = "yellow", "4" = "green", "5" = "blue")
ggplot(review_counts_df, aes(x = Review.score, y = n, fill = factor(Review.score))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 3) +  
  scale_fill_manual(values = color_palette) +  # Assign custom colors
  labs(x = "Review Score", y = "Aggregate") +
  ggtitle("Review Frequency Distribution") +
  theme_minimal()

# Sentiment Analysis in the entire data set 
emotions <- get_nrc_sentiment(hotel_data_raw$Text.1)
emo_bar <- colSums(emotions)
emo_sum <- data.frame(count= emo_bar, emotions = names(emo_bar))

# Visualization of Sentiments Across the Dataset
ggplot(emo_sum, aes(x = reorder(emotions, count), y = count)) + 
  geom_bar(stat = 'identity')

# Code to Extract only English Texts
install.packages("textcat")
library(textcat)
hotel_full_data <- cbind(hotel_data_raw, detected_languages = textcat(hotel_data_raw$Text.1))
hotel_data_english_indices <- hotel_full_data$detected_languages == "english"
hotel_english_data <- hotel_data_raw[hotel_data_english_indices, ]
head(hotel_english_data)

# Count of Data with English Texts
nrow(hotel_english_data)

#seed based on the last three digits of the StudentID
set.seed(162)
test<-sample_n(hotel_english_data, 2000)
nrow(test)

review_counts <- test %>%  count(Review.score)
head(review_counts)
review_counts_df <- as.data.frame(review_counts)
review_counts_df

#Add new column with negative and positive based on the review score
test <- test %>%
  mutate(sentiment = case_when(
    Review.score %in% c(4, 5) ~ "positive",
    Review.score %in% c(1, 2) ~ "negative",
    Review.score == 3 ~ "neutral"
  ))

#Pie chart to show the distribution
pie(prop.table(table(test$sentiment)), 
    labels = paste(names(table(test$sentiment)), ": ", round(prop.table(table(test$sentiment)) * 100, 1), "% (Count:", table(test$sentiment), ")"), 
    main = "Pie Chart of Review Score", 
    col = rainbow(length(table(test$sentiment))),
    cex = 0.8) 

# Positive Reviews dataset
positive_reviews <- test %>%
  filter(sentiment == "positive")

head(positive_reviews)

# Negative Reviews dataset
negative_reviews <- test %>%
  filter(sentiment == "negative")

head(negative_reviews)

# Negative Reviews dataset
neutral_Reviews <- test %>%
  filter(sentiment == "neutral")

head(neutral_Reviews)
nrow(neutral_Reviews)

# Sentiment Analysis in the neutral data set 
emotions_neutral <- get_nrc_sentiment(neutral_Reviews$Text.1)
emo_bar_neutral <- colSums(emotions_neutral)
emo_sum_neutral <- data.frame(count= emo_bar_neutral, emotions = names(emo_bar_neutral))

# Visualization of generic emotions in Neutral Data Set
library(ggplot2)
ggplot(emo_sum_neutral, aes(x = reorder(emotions, count), y = count)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +  
  labs(title = "Emotion Counts in Neutral Reviews",
       x = "Emotions",
       y = "Count")

# Converting to UTF-8
test_positive_conv <- stringr::str_conv(positive_reviews$Text.1, "UTF-8")
test_negative_conv <- stringr::str_conv(negative_reviews$Text.1, "UTF-8")

# Create Corpus
myCorpus_positive <- Corpus(VectorSource(test_positive_conv))
myCorpus_negative <- Corpus(VectorSource(test_negative_conv))

###################### TOPIC MODELLING ON POSITIVE DATA SET ############################################ 
# ----------------Create Document Term Matrix for  Positive reviews dataset-------------#
dtmdocs <- DocumentTermMatrix(myCorpus_positive,
                              control = list(lemma=TRUE,
                                             removePunctuation = TRUE,
                                             removeNumbers = TRUE,
                                             stopwords = TRUE,
                                             tolower = TRUE))
raw.sum=apply(dtmdocs,1,FUN=sum)
dtmdocs=dtmdocs[raw.sum!=0,]
dtm.new <- as.matrix(dtmdocs)
frequency <- colSums(dtm.new)
frequency <- sort(frequency, decreasing=TRUE)
doc_length <- rowSums(dtm.new)


#Example of the output
frequency[1:10]

#get back the word
words <- names(frequency) 

#Wordcloud
wordcloud(words[1:100], frequency[1:100], rot.per=0.15, 
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))

# Find number of topics for Positive Data Set
result <- FindTopicsNumber(
  dtm.new,
  topics = seq(from = 5, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),  #"Deveaud2014" not used
  method = "Gibbs",
  control = list(seed = 162),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)  

# Now Employing LDA Model
#Optimum Topic Number - 14 for Postive Data Set
ldaOut <-LDA(dtmdocs,14, method="Gibbs", 
             control=list(iter=1000,seed=162))
phi <- posterior(ldaOut)$terms %>% as.matrix 
theta <- posterior(ldaOut)$topics %>% as.matrix 
ldaOut.terms <- as.matrix(terms(ldaOut, 20))
ldaOut.terms
ldaOut.terms[,14]
ldaOut.topics <- data.frame(topics(ldaOut))
ldaOut.topics$index <- as.numeric(row.names(ldaOut.topics))
positive_reviews$index <- as.numeric(row.names(positive_reviews))
datawithtopic <- merge(positive_reviews, ldaOut.topics, by='index',all.x=TRUE)
datawithtopic <- datawithtopic[order(datawithtopic$index), ]
datawithtopic[0:15,]
topicProbabilities <- as.data.frame(ldaOut@gamma)
topicProbabilities[0:10,1:5]
vocab <- colnames(phi) #vocab list in DTM

# Label the topics
top_terms <- terms(ldaOut, 20)
top_terms
topic_labels <- c("Great Comfort", 
                  "Accomodation Inconvenience", 
                  "Cuisine",
                  "Accessibility to Local Hubs",
                  "Food",
                  "Experience",
                  "Convenient Location",
                  "Amazing Service",
                  "Cleanliness",
                  "Affordability",
                  "Amazing Amenities",
                  "Poor Customer Service",
                  "Lodging Issues",
                  "Dining Excellence"
)
for (i in 1:14) {
  cat(topic_labels[i], "\n")
  print(top_terms[, i])
}
topic_labels

# create the JSON object to feed the visualization in LDAvis for Positive Data
json_lda <- createJSON(phi = phi, theta = theta, 
                       vocab = vocab, doc.length = doc_length, 
                       term.frequency = frequency, k =14)
serVis(json_lda, out.dir = 'vis', open.browser = TRUE)

###################### TOPIC MODELLING ON NEGATIVE DATA SET ############################################ 

# Create DocumentTermMatrix for  Negative reviews dataset
dtmdocs_neg <- DocumentTermMatrix(myCorpus_negative,
                                  control = list(lemma=TRUE,
                                                 removePunctuation = TRUE,
                                                 removeNumbers = TRUE,
                                                 stopwords = TRUE,
                                                 tolower = TRUE))
raw.sum_neg=apply(dtmdocs_neg,1,FUN=sum)
dtmdocs_neg=dtmdocs_neg[raw.sum_neg!=0,]
dtm_neg.new <- as.matrix(dtmdocs_neg)
frequency_neg <- colSums(dtm_neg.new)
frequency_neg <- sort(frequency_neg, decreasing=TRUE)
doc_length_neg <- rowSums(dtm_neg.new)

#Example of the output
frequency_neg[1:10] 

words_neg <- names(frequency_neg)


wordcloud(words_neg[1:100], frequency_neg[1:100], rot.per=0.15, 
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))


# Find number of topics for Negative Data set
result_neg <- FindTopicsNumber(
  dtm_neg.new,
  topics = seq(from = 5, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),        ## Another Metric - Deveaud2014
  method = "Gibbs",
  control = list(seed = 162),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result_neg)

#Optimum Topic Number - 10 for Postive Data Set
ldaOut_neg <-LDA(dtmdocs_neg,10, method="Gibbs", 
                 control=list(iter=5000,seed=162)). #5000 iterations made to make proper modelling
phi_neg <- posterior(ldaOut_neg)$terms %>% as.matrix 
theta_neg <- posterior(ldaOut_neg)$topics %>% as.matrix 

ldaOut_neg.terms <- as.matrix(terms(ldaOut_neg, 20))
ldaOut_neg.terms

ldaOut_neg.topics <- data.frame(topics(ldaOut_neg))
ldaOut_neg.topics$index <- as.numeric(row.names(ldaOut_neg.topics))
negative_reviews$index <- as.numeric(row.names(negative_reviews))
ldaOut_neg.topics
nrow(negative_reviews)
nrow(ldaOut_neg.topics)
datawithtopic_neg <- merge(negative_reviews, ldaOut_neg.topics, by='index',all.x=TRUE)
datawithtopic_neg <- datawithtopic_neg[order(datawithtopic_neg$index), ]
topicProbabilities_neg <- as.data.frame(ldaOut_neg@gamma)
topicProbabilities_neg[0:10,1:5]
vocab_neg <- colnames(phi_neg) #vocab list in DTM

topic_labels_neg <- c("Poor Room Service", 
                      "Terrible Customer Management", 
                      "Pleasant Stay",
                      "Accessibility to Local Hubs",
                      "Bad Rooms",
                      "Substandard Rooms", 
                      "Noisy",
                      "Ventilation","Not Worthy","General Problem")

for (i in 1:10) {
  cat(topic_labels_neg[i], "\n")
  print(ldaOut_neg.terms[, i])
}
topic_labels_neg


# create the JSON object to feed the visualization in LDAvis:
json_lda_neg <- createJSON(phi = phi_neg, theta = theta_neg, 
                           vocab = vocab_neg, doc.length = doc_length_neg, 
                           term.frequency = frequency_neg)
serVis(json_lda_neg, out.dir = 'vis', open.browser = TRUE)
