rm(list=ls(all=TRUE)) # Clear all 

##  Required Packages  ##
install.packages("NLP")         #NLP Functions
install.packages("tm")          #Text-mining
install.packages("SnowballC")   #Stemming
install.packages("wordcloud")   #WordCloud
install.packages("RColorBrewer")#Colours for WordCloud

##  Activate Packages ##
library(NLP) 
library(tm)
library(SnowballC)      
library(wordcloud)
library(RColorBrewer)  
library(stats)           # Clustering


## Data Loading ##
Apps_data <- read.csv("App_Games_Desc.csv") #The original dataset is stored in your project folder. 
head(Apps_data)
Apps_desc <- Apps_data[,10]                 #Descriptions are recorded in the 10th column.  
n_desc <- length(Apps_desc)                 #Check the number of app descriptions
n_desc

## Use the First Two Sentences ##

Test_sent <- unlist(strsplit(as.character(Apps_desc[1]), split = "[.!?]+"))  #Split a document into sentences
Test_sent

Test_sent[1:2]

# Select the first two sentences #
Apps_Sent <- list()                  # Creat an empty list that will include the 2 sentences

for (i in 1:n_desc) { 
temp <- unlist(strsplit(as.character(Apps_desc[i]), split = "[.!?]+"))  
Apps_Sent[[i]] <- temp[1:2]           #Use the first 2 sentences
}  

Apps_Sent[[1]]

Apps_Sent[[4]]

## Convert Vector/List to Corpus ## 
Apps   <- Corpus(VectorSource(Apps_Sent)) 
Apps

Apps[[1]]

as.character(Apps[[1]])
as.character(Apps[[2]])

###Data Pre-Processing ###

## Upper-case Letters to Lower-case Letters ##
Apps  <- tm_map(Apps, content_transformer(tolower)) 
as.character(Apps[[1]])

### Remove unnecessary terms and symbols ###
  
# Delete HTML Tags #
for (j in 1:n_desc) Apps[[j]] <- gsub("u2019", " ", Apps[[j]]) #delete "u2019"
for (j in 1:n_desc) Apps[[j]] <- gsub("u'", " ", Apps[[j]])  #delete u'
for (j in 1:n_desc) Apps[[j]] <- gsub("u\"", " ", Apps[[j]]) #delete u"

as.character(Apps[[1]])

inspect(Apps[1:3])
   
# Delete HTML Tags #
for (j in 1:n_desc) Apps[[j]] <- gsub("u2605", " ", Apps[[j]]) #delete "u2605"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2606", " ", Apps[[j]]) #delete "u2606"
for (j in 1:n_desc) Apps[[j]] <- gsub("u201c", " ", Apps[[j]]) #delete "u201c"
for (j in 1:n_desc) Apps[[j]] <- gsub("u201d", " ", Apps[[j]]) #delete "u201d"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2011", " ", Apps[[j]]) #delete "u2011"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2013", " ", Apps[[j]]) #delete "u2013"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2014", " ", Apps[[j]]) #delete "u2014"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2022", " ", Apps[[j]]) #delete "u2022"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2122", " ", Apps[[j]]) #delete "u2122"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2026", " ", Apps[[j]]) #delete "u2026
for (j in 1:n_desc) Apps[[j]] <- gsub("u2028", " ", Apps[[j]]) #delete "u2028"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2729", " ", Apps[[j]]) #delete "u2729"
for (j in 1:n_desc) Apps[[j]] <- gsub("u20ac", " ", Apps[[j]]) #delete "u20ac"
for (j in 1:n_desc) Apps[[j]] <- gsub("amp", " ", Apps[[j]]) #amp
for (j in 1:n_desc) Apps[[j]] <- gsub("xae", " ", Apps[[j]]) #xae
for (j in 1:n_desc) Apps[[j]] <- gsub("xa0", " ", Apps[[j]]) #xa0
for (j in 1:n_desc) Apps[[j]] <- gsub("xa3", " ", Apps[[j]]) #xa3
inspect(Apps[1:3])
  
# Remove less important terms: Device names #
for (j in 1:n_desc) Apps[[j]] <- gsub("apple", " ", Apps[[j]]) #Apple
for (j in 1:n_desc) Apps[[j]] <- gsub("iphone", " ", Apps[[j]]) #iphone
for (j in 1:n_desc) Apps[[j]] <- gsub("touch", " ", Apps[[j]]) #touch
for (j in 1:n_desc) Apps[[j]] <- gsub("ipod", " ", Apps[[j]]) #ipod
for (j in 1:n_desc) Apps[[j]] <- gsub("ipad", " ", Apps[[j]]) #ipad
for (j in 1:n_desc) Apps[[j]] <- gsub("3gs", " ", Apps[[j]]) #iPohne 3GS 
for (j in 1:n_desc) Apps[[j]] <- gsub("3rd", " ", Apps[[j]]) #3rd Gen. iPod
for (j in 1:n_desc) Apps[[j]] <- gsub("2nd", " ", Apps[[j]]) #2nd Gen. iPod
for (j in 1:n_desc) Apps[[j]] <- gsub("4th", " ", Apps[[j]]) #4th Gen. iPod
inspect(Apps[1:3])
  
# Remove less important terms: App, Game, Play # 
for (j in 1:n_desc) Apps[[j]] <- gsub("app", " ", Apps[[j]]) #app
for (j in 1:n_desc) Apps[[j]] <- gsub("store", " ", Apps[[j]]) #store
for (j in 1:n_desc) Apps[[j]] <- gsub("game", " ", Apps[[j]]) #game 
for (j in 1:n_desc) Apps[[j]] <- gsub("play", " ", Apps[[j]]) #play
for (j in 1:n_desc) Apps[[j]] <- gsub("mobile", " ", Apps[[j]]) #mobile
for (j in 1:n_desc) Apps[[j]] <- gsub("free", " ", Apps[[j]]) #Free
for (j in 1:n_desc) Apps[[j]] <- gsub("new", " ", Apps[[j]]) #new
for (j in 1:n_desc) Apps[[j]] <- gsub("world", " ", Apps[[j]]) #world
inspect(Apps[1:3])
  
# Convert Important numbers to words/terms #
for (j in 1:n_desc) Apps[[j]] <- gsub("#1", "numberone", Apps[[j]]) #1
for (j in 1:n_desc) Apps[[j]] <- gsub("99", "nintyninecent", Apps[[j]]) #$0.99
for (j in 1:n_desc) Apps[[j]] <- gsub("%", "percent", Apps[[j]]) #percent
inspect(Apps[1:3])
  
  
# Remove stopwords #
Apps <- tm_map(Apps, removeWords, stopwords("english"))
inspect(Apps[1:3])
  
# You can define stopwords #
newstopwords <-c("and", "for", "the", "to", "in", "when", "then", "he", "she", "than", "can", "get", "one");
Apps <- tm_map(Apps, removeWords, newstopwords)
  
for (j in 1:n_desc) Apps[[j]] <- gsub("don", " ", Apps[[j]]) #don
for (j in 1:n_desc) Apps[[j]] <- gsub("won", " ", Apps[[j]]) #won't
for (j in 1:n_desc) Apps[[j]] <- gsub("ing", " ", Apps[[j]]) # -ing
for (j in 1:n_desc) Apps[[j]] <- gsub("http", " ", Apps[[j]]) # http
for (j in 1:n_desc) Apps[[j]] <- gsub("'ll", " ", Apps[[j]]) #'ll   
for (j in 1:n_desc) Apps[[j]] <- gsub("www", " ", Apps[[j]]) # www
for (j in 1:n_desc) Apps[[j]] <- gsub("com", " ", Apps[[j]]) # com
inspect(Apps[1:3])
  
# Remove Numbers #
Apps <- tm_map(Apps, removeNumbers) 
inspect(Apps[1:3])
  
# Remove Punctuations and Symbols #
Apps <- tm_map(Apps, removePunctuation) 
inspect(Apps[1:3])
  
# You can also manually delete non-characters #
for (j in 1:n_desc) Apps[[j]] <- gsub("['*|&|-|/|\\|()|\\.,!-_]", " ", Apps[[j]]) # remove non-characters
for (j in 1:n_desc) Apps[[j]] <- gsub("\"", " ", Apps[[j]]) # remove non-characters

# Delete "c" 
for (j in 1:n_desc) Apps[[j]] <- gsub("c ", " ", Apps[[j]]) #There is a space after "c"
inspect(Apps[1:3])

# Remove Extra White Space #
Apps <- tm_map(Apps, stripWhitespace)
inspect(Apps[1:3])
  
## Stemming ##
Apps <- tm_map(Apps, stemDocument) # Remove common word endings ("es", "ed", "s", "ing")
inspect(Apps[1:3])

 
## Create a DTM ##
# Original DTM #
dtm_Apps <- DocumentTermMatrix(Apps)

dtm_Apps

inspect(dtm_Apps)

# DTM with Controls #
dtm_Apps_Ctrl <- DocumentTermMatrix(Apps, control=list(wordLength=c(3,20), bounds=list(global=c(10,200))))
dtm_Apps_Ctrl
inspect(dtm_Apps_Ctrl) # Display DTM for the descriptions 

as.character(Apps[[1]])

inspect(dtm_Apps_Ctrl[1:5,1:10]) # DTM for the first 5 descriptions with the first 10 terms


## Frequency of Terms in DTM ##

# Find the terms that occur at least 30 times #
findFreqTerms(dtm_Apps_Ctrl, 30)

# Frequency of terms #  
Freq_term <-colSums(as.matrix(dtm_Apps_Ctrl))
Order_Freq_term <- order(Freq_term, decreasing = TRUE)
Freq_term[Order_Freq_term]

# Frequency Diagram #
library(grDevices); # for colors 
Apps_DTM_DF = as.data.frame(as.matrix(dtm_Apps_Ctrl))
numwords <- 30; # the most frequent 30 terms  

# sum each column and sort by descending order # 
Terms_Freq <- as.matrix(sort(sapply(Apps_DTM_DF, FUN=sum),decreasing=TRUE)[1:numwords], colnames=count)
x <- sort(Terms_Freq[1:numwords,], decreasing=FALSE) 
barplot(x, horiz=TRUE, cex.names=0.5, space=1, las=1, col=grey.colors(10), main="Frequency of Terms")

## Word Cloud ##
# For Original DTM #
 set.seed(3008) #set the same seed each time ensures consistent look across clouds
 m <- as.matrix(t(dtm_Apps)) # Convert it to a matrix
 v <- sort(rowSums(m),decreasing=TRUE) # Sort the terms in a descending order 
 w <- data.frame(word = names(v),freq=v) # Create a data frame indicating the name&frequency of terms

 WC_color <- brewer.pal(8,"Set2")
 wordcloud(w$word,w$freq, scale=c(3,.1),min.freq=1, max.words=200, random.order=F, rot.per=.3, colors=WC_color)

# For DTM with Controls #
 dev.off()
 set.seed(3008) #set the same seed each time ensures consistent look across clouds
 m <- as.matrix(t(dtm_Apps_Ctrl)) # Convert it to a matrix
 v <- sort(rowSums(m),decreasing=TRUE) # Sort the terms in a descending order 
 w <- data.frame(word = names(v),freq=v) # Create a data frame indicating the name&frequency of terms

 WC_color <- brewer.pal(8,"Set2")
 wordcloud(w$word,w$freq, scale=c(3,.1),min.freq=1, max.words=200, random.order=F, rot.per=.3, colors=WC_color)

 
## Find Associated Terms ##
findAssocs(dtm_Apps_Ctrl, "time", .3);
findAssocs(dtm_Apps_Ctrl, "now", .2);
findAssocs(dtm_Apps_Ctrl, "million", .2);

 
 
 
################################## Session 2 ######################


## Select the number of Terms for Clustering ##
dtm_Apps_Sparse <- removeSparseTerms(dtm_Apps_Ctrl, 0.92) 
#Remove the terms which have at least a 92 percentage of sparse

nrow(dtm_Apps_Sparse); ncol(dtm_Apps_Sparse)
inspect(dtm_Apps_Sparse)




## K-means Clustering ##
dtm_Apps_cluster <-as.matrix(dtm_Apps_Sparse)
library(stats)     # Clustering

set.seed(3008)
Apps_KM <- kmeans(t(dtm_Apps_cluster), 3)
Apps_KM

## Return the output ##
Apps_KM$size
sort(Apps_KM$cluster)

## Identify the optimal k ##
#Within Groups Sum of Squares#
wss <- 1:5
for(i in 1:5) {wss[i] <- sum(kmeans(t(dtm_Apps_cluster),i)$withinss)}
plot(1:5, wss[1:5], type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")
# type="b" creates a plot with lines between points # 
wss

## Hierarchical Clustering ##
dtm_Apps_cluster <-as.matrix(dtm_Apps_Sparse)

# Calculate the Distance between Terms # 
distance <- dist(t(dtm_Apps_cluster), method="euclidean")
distance 

Apps_HC <- hclust(distance, method = "ward.D")
plot(Apps_HC)

# Draw dendrogram with red borders around the 3 clusters #
rect.hclust(Apps_HC, k=3, border="red")

Apps_HC_Cut <- cutree(Apps_HC, k=3) # cut tree into 3 clusters
Apps_HC_Cut

sort(Apps_HC_Cut)



### Regression ###


## Count the number of terms in a description ##
Num_Terms <- matrix(data=0 , n_desc,1);


## Compute Cluster Scores ##
# Identify the terms for each cluster #
cluster1 <- dtm_Apps_cluster[,c("countri", "numberon", "download", "million")]
cluster2 <- dtm_Apps_cluster[,c("limit", "time", "sale","percent")]
cluster3 <- dtm_Apps_cluster[,c("now", "best", "avail", "featur")]

head(cluster1)
head(cluster2)
head(cluster3)

# Sums #
C1_Sum <- rowSums(cluster1)
C2_Sum <- rowSums(cluster2)
C3_Sum <- rowSums(cluster3)

# Create a Score table #
Score <- matrix(data=0 , n_desc,3);
Score[,1] <- as.matrix(C1_Sum)
Score[,2] <- as.matrix(C2_Sum)
Score[,3] <- as.matrix(C3_Sum)

# Name the Columns/Clusters #
colnames(Score) <- c("Cluster1", "Cluster2", "Cluster3")
head(Score)

## Add a Score matrix to the original Data ##
Apps_new <- cbind(Apps_data, Score)
str(Apps_new)

## Run a Regression ##

summary(Apps_new[,c('Rank','Price','Screenshot','Size','StarCurrentVersion', 'RatingCurrentVersion', 'TopInAppPurchases','Cluster1','Cluster2','Cluster3')])

# Variable Transformation #
Sales <- -log(Apps_new$Rank)
Log_Rating_Num <- log(Apps_new$RatingCurrentVersion+1)

Apps_new <-cbind(Apps_new, Sales)
Apps_new <-cbind(Apps_new, Log_Rating_Num)


# Build a regression model #
Apps_Reg <- lm(Sales ~ Price + Screenshot + Size + StarCurrentVersion + Log_Rating_Num + TopInAppPurchases + Cluster1 + Cluster2 + Cluster3, data=Apps_new)
summary(Apps_Reg)
