options(repos = c(CRAN = "https://cran.rstudio.com/"))
library(ggplot2)
library(dplyr)
install.packages('wordcloud')
install.packages("tm")
install.packages("tokenizers")
library(tm)
library(tokenizers)
library(wordcloud)
install.packages("VIM",dependencies = T)
library("VIM")

# 2.1 Import data into R environment

setwd("/Users/deepu/Downloads")
complaints_data <- read.csv("Comcast Telecom Complaints data.csv")
head(complaints_data)
aggr(complaints_data, numbers=TRUE, prop=FALSE)


# 2.2 Provide the trend chart for the number of complaints at monthly and daily granularity levels

complaints_data$Date <- gsub("-", "/", as.character(complaints_data$Date))
complaints_data$Date <- gsub("/", "-", as.character(complaints_data$Date))
complaints_data$Date <- as.Date(complaints_data$Date, format = "%d-%m-%Y")
class(complaints_data$Date)
complaints_data$YearMonth <- format(complaints_data$Date, "%Y-%m")  # Year-Month format
complaints_data$Day <- format(complaints_data$Date, "%d")            # Day format

daily_complaints <- complaints_data %>%
  group_by(Date) %>%
  summarise(complaints_count = n())
ggplot(daily_complaints, aes(x = Date, y = complaints_count)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "red") +
  labs(title = "Daily Complaints Trend", x = "Date", y = "Number of Complaints") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

monthly_complaints <- complaints_data %>%
  group_by(YearMonth) %>%
  summarise(complaints_count = n())
ggplot(monthly_complaints, aes(x = YearMonth, y = complaints_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Monthly Complaints Trend", x = "Month", y = "Number of Complaints") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#2.3 Provide a table with the frequency of complaint types

myCorpuse <- Corpus(VectorSource(complaints_data$Customer.Complaint))
dtmdocs <- DocumentTermMatrix(myCorpuse,
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
words <- names(frequency) 
wordcloud(words[1:100], frequency[1:100], rot.per=0.15, 
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))



assign_complaint_type <- function(complaint) {
  complaint <- tolower(complaint)  # Convert to lowercase
  
  complaint_type <- ifelse(
    grepl("billing|overcharge|monthly bill|billing error", complaint), "Billing Issue", 
    ifelse(
      grepl("internet|service|connection|data|intermittent|download", complaint), "Internet Service Issue", 
      ifelse(
        grepl("contract|refuse|deceptive|plan|policies|Comcast", complaint), "Customer Service Issues", 
        "Other"
      )
    )
  )
  
  return(complaint_type)
}

complaints_data$Complaint.Type <- sapply(complaints_data$Customer.Complaint, assign_complaint_type)

table(complaints_data$Complaint.Type)
table(complaints_data$Customer.Complaint)

complaint_frequency <- table(complaints_data$Complaint.Type)
complaint_frequency

# 2.4 Which complaint types are maximum i.e., around internet, network issues, or across any other domains.

barplot(complaint_frequency, 
        main = "Frequency of Complaint Types", 
        xlab = "Complaint Type", 
        ylab = "Frequency", 
        col = "lightblue", 
        border = "black")



# 2.5 Create a new categorical variable with value as Open and Closed. Open & Pending is to be categorized as Open and Closed & Solved is to be categorized as Closed.

complaints_data <- complaints_data %>%
  mutate(Complaint.Status = case_when(
    Status %in% c("Open", "Pending") ~ "Open",  
    Status %in% c("Closed", "Solved") ~ "Closed"
  ))
table(complaints_data$Complaint.Status)



#2.6 Provide state wise status of complaints in a stacked bar chart

state_complaint_count <- complaints_data %>%
  count(State, Complaint.Status)

#2.7 Which state has the maximum complaints

ggplot(state_complaint_count, aes(x = State, y = n, fill = Complaint.Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Complaint Status by State", 
       x = "State", 
       y = "Number of Complaints") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  scale_fill_brewer(palette = "Set2") +  # Add color palette for better visualization
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "black", size = 3)


#2.8 Which state has the highest percentage of unresolved complaints

unresolved_complaints <- complaints_data %>%filter(Complaint.Status %in% c("Open"))
unresolved_counts <- unresolved_complaints %>%
  count(State)
total_counts <- complaints_data %>%
  count(State)
complaint_summary <- left_join(unresolved_counts, total_counts, by = "State", suffix = c("_unresolved", "_total"))
complaint_summary <- complaint_summary %>%
  mutate(percentage_unresolved = (n_unresolved / n_total) * 100)
highest_percentage_state <- complaint_summary %>%
  filter(percentage_unresolved == max(percentage_unresolved))
highest_percentage_state

#2.9 Provide the percentage of complaints resolved till date, which were received through the Internet and customer care calls.

resolved_complaints <- complaints_data %>% filter(Received.Via %in% c("Internet", "Customer Care Call"))
total_complaints <- resolved_complaints %>% count(Received.Via)
total_complaints_r <- resolved_complaints %>% count(Received.Via)
total_complaints <- complaints_data%>% count(Received.Via)
resolved_complaints_count <- resolved_complaints %>%
  filter(Complaint.Status %in% c("Closed")) %>%
  count(Received.Via)
complaint_summary <- left_join(total_complaints, resolved_complaints_count, by = "Received.Via", suffix = c("_total", "_resolved"))
complaint_summary <- complaint_summary %>%
  mutate(percentage_resolved = (n_resolved / n_total) * 100)
complaint_summary

ggplot(complaint_summary, aes(x = Received.Via, y = n_total, fill = "Total Complaints")) +
  geom_bar(stat = "identity", color = "black") +
  geom_bar(aes(y = n_resolved, fill = "Resolved Complaints"), stat = "identity") +
  geom_text(aes(y = n_resolved, label = paste("Resolved: ", n_resolved, " (", round(percentage_resolved, 1), "%)")), 
            vjust = -0.5, color = "white") +
  labs(title = "Total vs. Resolved Complaints by Channel", x = "Received Via", y = "Number of Complaints") +
  scale_fill_manual(name = "Complaint Status", values = c("Total Complaints" = "#56B4E9", "Resolved Complaints" = "#009E73")) +
  theme_minimal()

