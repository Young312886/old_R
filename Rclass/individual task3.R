rm(list=ls(all=TRUE))

library(NLP) 
library(tm)
library(SnowballC)      
library(wordcloud)
library(RColorBrewer)  
library(stats) 

#read csv file
App <- read.csv("Apps_Games500.csv")
head(App)

# Get description of app 
Apps_desc <- App[,4]                   
n_desc <- length(Apps_desc)                 
n_desc

# Subtrack first 300 character
test_300 <- unlist(substr(as.character(Apps_desc[1]),1,300))
test_300
nchar(test_300)
Apps_300 <- unlist(substr(as.character(Apps_desc[1:500]),1,300))
Apps_300[1:5]

#Return the corpus information
Apps <- Corpus(VectorSource(Apps_300)); Apps

#Parsing
#Convert Uppercase to lower case
Apps  <- tm_map(Apps, content_transformer(tolower)) 
# Delete HTML Tags
for (j in 1:n_desc) Apps[[j]] <- gsub("u2019", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("u'", " ", Apps[[j]])  
for (j in 1:n_desc) Apps[[j]] <- gsub("u\"", " ", Apps[[j]])
inspect(Apps[1:3])
# Delete HTML Tags
for (j in 1:n_desc) Apps[[j]] <- gsub("u2605", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("u2606", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("u201c", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("u201d", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("u2011", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("u2013", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("u2014", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("u2022", " ", Apps[[j]])
for (j in 1:n_desc) Apps[[j]] <- gsub("u2122", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("u2026", " ", Apps[[j]])
for (j in 1:n_desc) Apps[[j]] <- gsub("u2028", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("u2729", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("u20ac", " ", Apps[[j]])  
for (j in 1:n_desc) Apps[[j]] <- gsub("amp", " ", Apps[[j]])
for (j in 1:n_desc) Apps[[j]] <- gsub("xae", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("xa0", " ", Apps[[j]])
for (j in 1:n_desc) Apps[[j]] <- gsub("xa3", " ", Apps[[j]])
inspect(Apps[1:3])
# Remove less important terms: Device names
for (j in 1:n_desc) Apps[[j]] <- gsub("apple", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("iphone", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("touch", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("ipod", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("ipad", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("3gs", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("3rd", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("2nd", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("4th", " ", Apps[[j]]) 
inspect(Apps[1:3])

# Remove less important terms 
for (j in 1:n_desc) Apps[[j]] <- gsub("app", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("store", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("game", " ", Apps[[j]])  
for (j in 1:n_desc) Apps[[j]] <- gsub("play", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("mobile", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("free", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("new", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("world", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("now", " ", Apps[[j]])
for (j in 1:n_desc) Apps[[j]] <- gsub("best", " ", Apps[[j]])
for (j in 1:n_desc) Apps[[j]] <- gsub("time", " ", Apps[[j]])
for (j in 1:n_desc) Apps[[j]] <- gsub("will", " ", Apps[[j]])
inspect(Apps[1:3])

# Convert Important numbers to words/terms
for (j in 1:n_desc) Apps[[j]] <- gsub("#1", "numberone", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("99", "nintyninecent", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("%", "percent", Apps[[j]]) 
inspect(Apps[1:3])


# Remove stopwords
Apps <- tm_map(Apps, removeWords, stopwords("english"))
inspect(Apps[1:3])

# You can define stopwords
newstopwords <-c("and", "for", "the", "to", "in", "when", "then", "he", "she", "than", "can", "get", "one");
Apps <- tm_map(Apps, removeWords, newstopwords)

for (j in 1:n_desc) Apps[[j]] <- gsub("don", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("won", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("ing", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("http", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("'ll", " ", Apps[[j]])  
for (j in 1:n_desc) Apps[[j]] <- gsub("www", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("com", " ", Apps[[j]]) 
inspect(Apps[1:3])

# Remove Numbers
Apps <- tm_map(Apps, removeNumbers) 
inspect(Apps[1:3])

# Remove Punctuations and Symbols
Apps <- tm_map(Apps, removePunctuation) 
inspect(Apps[1:3])

# You can also manually delete non-characters
for (j in 1:n_desc) Apps[[j]] <- gsub("['*|&|-|/|\\|()|\\.,!-_]", " ", Apps[[j]]) 
for (j in 1:n_desc) Apps[[j]] <- gsub("\"", " ", Apps[[j]]) 

# Remove Extra White Space
Apps <- tm_map(Apps, stripWhitespace)
inspect(Apps[1:3])
inspect(Apps[1:10])
# Stemming 
Apps <- tm_map(Apps, stemDocument) 
inspect(Apps[1:3])
as.character(Apps[[1]])

#Creating DTM
dtm_Apps <- DocumentTermMatrix(Apps)
dtm_Apps
inspect(dtm_Apps)

#Creat DTM with restriction
dtm_Apps_Ctrl <- DocumentTermMatrix(Apps, control=list(wordLength=c(3,10), bounds=list(global=c(20,500))))
dtm_Apps_Ctrl
inspect(dtm_Apps_Ctrl[1:10,1:10])

# Present Frequency table

# Frequency of terms 
Freq_term <-colSums(as.matrix(dtm_Apps_Ctrl))
Order_Freq_term <- order(Freq_term, decreasing = TRUE)
Freq_term[Order_Freq_term]

# Frequency Diagram
library(grDevices);  
Apps_DTM_DF = as.data.frame(as.matrix(dtm_Apps_Ctrl))
numwords <- 20;
Terms_Freq <- as.matrix(sort(sapply(Apps_DTM_DF, FUN=sum),decreasing=TRUE)[1:numwords], colnames=count)
x <- sort(Terms_Freq[1:numwords,], decreasing=FALSE) 
barplot(x, horiz=TRUE, cex.names=0.5, space=1, las=1, col=grey.colors(10), main="Frequency of Terms")


## Word Cloud
# For Original DTM 
set.seed(3008) #set the same seed each time ensures consistent look across clouds
m <- as.matrix(t(dtm_Apps)) # Convert it to a matrix
v <- sort(rowSums(m),decreasing=TRUE) # Sort the terms in a descending order 
w <- data.frame(word = names(v),freq=v) # Create a data frame indicating the name&frequency of terms

WC_color <- brewer.pal(8,"Set2")
wordcloud(w$word,w$freq, scale=c(3,.1),min.freq=1, max.words=200, random.order=F, rot.per=.3, colors=WC_color)

# For DTM with Controls
dev.off()                                              
set.seed(3008) #set the same seed each time ensures consistent look across clouds
m <- as.matrix(t(dtm_Apps_Ctrl)) # Convert it to a matrix
v <- sort(rowSums(m),decreasing=TRUE) # Sort the terms in a descending order 
w <- data.frame(word = names(v),freq=v) # Create a data frame indicating the name&frequency of terms

WC_color <- brewer.pal(8,"Set2")
wordcloud(w$word,w$freq, scale=c(3,.1),min.freq=1, max.words=200, random.order=F, rot.per=.3, colors=WC_color)


                                 