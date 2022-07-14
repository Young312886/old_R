rm(list=ls(all=TRUE))

library(NLP) 
library(tm)
library(SnowballC)      
library(wordcloud)
library(RColorBrewer)  
library(stats) 


room  <- read.csv("room_review.csv")
review <- room[,2]
n_review <- length(review);n_review
review   <- Corpus(VectorSource(review))


review  <- tm_map(review, content_transformer(tolower)) 
for (j in 1:n_review) review[[j]] <- gsub("room", " ", review[[j]])
for (j in 1:n_review) review[[j]] <- gsub("rooms", " ", review[[j]]) 
for (j in 1:n_review) review[[j]] <- gsub("london", " ", review[[j]]) 
for (j in 1:n_review) review[[j]] <- gsub("hotel", " ", review[[j]]) 

review <- tm_map(review, removeWords, stopwords("english"))
review <- tm_map(review, removeNumbers)
review <- tm_map(review, removePunctuation)
for (j in 1:n_review) review[[j]] <- gsub("['*|&|-|/|\\|()|\\.,!-_]", " ", review[[j]]) 
for (j in 1:n_review) review[[j]] <- gsub("\"", " ", review[[j]]) 
review <- tm_map(review, stripWhitespace)
inspect(review[1:3])
review <- tm_map(review, stemDocument) 
inspect(review[1:3])

dtm_review <- DocumentTermMatrix(review)
inspect(dtm_review)

dtm_review_ctrl <- DocumentTermMatrix(review, control=list(wordLength=c(3,15), bounds=list(global=c(30,300))))
dtm_review_ctrl
inspect(dtm_review_ctrl)

findFreqTerms(dtm_review_ctrl, 40)

Freq_term <-colSums(as.matrix(dtm_review_ctrl))
Order_Freq_term <- order(Freq_term, decreasing = TRUE)
Freq_term[Order_Freq_term]

findAssocs(dtm_review_ctrl, "clean", .05) # comfort 0.15, bath 0.08, good 0.05
