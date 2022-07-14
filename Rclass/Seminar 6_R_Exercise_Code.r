#### R Exercise_S6 ####
rm(list=ls())

## Data Import ##
Apps_data <- read.csv('Apps.csv')
Ratings_data <- read.csv('Apps_Ratings.csv')

## Step1: Merging Datasets ##
head(Apps_data)
head(Ratings_data)


# Q1: Combine the two datasets by AppID #
Apps <- merge(Apps_data,Ratings_data, by="AppID")

head(Apps)


## Step2: Understand Data##
str(Apps)
summary(Apps[,c('Rank','Price','Screenshots','Rating_Score','Rating_Num')])

# Q2-1: Variable Conversion #


# Q2-2: Convert the value type of Title into characters

str(Apps)
class(Apps$Title)
Apps$Title <- as.character(Apps$Title)
is.character(Apps$Title)


## Step3: Missing Values ##
# Q3-1: Identify Missing Values in Price# 

missing<- subset(Apps, is.na(Apps$Price) == "TRUE",)

nrow(missing)

# Q3-2: Replace the Missing Values with # 

summary(Apps[,c('Rank','Price','Screenshots','Rating_Score','Rating_Num')])


## Step4: Handling Outliers ##
# Q4-1: Identify outliers in Rating_Num 
# Hint: Compute the mean and SD of Rating_Num first #
Mean_Num <- mean(Apps$Rating_Num) ; Mean_Num
SD_Num <- sd(Apps$Rating_Num) ; SD_Num
Upper <- Mean_Num + 3*SD_Num
Lower <- Mean_Num - 3*SD_Num
Outliers <- subset(Apps, Rating_Num > Upper | Rating_Num < Lower)
nrow(Outliers)

# Q4-2: Remove Outliers in Rating_Num#
Apps$Rating_Num[Apps$Rating_Num > Upper] <- NA

#Alternative#
for (i in 1:nrow(Apps)) (
  ifelse(Apps$Rating_Num[i] > Upper, Apps$Rating_Num[i] <- NA, Apps$Rating_Num[i]))

# Q4-3 : Identify the number of Missing Values after handling outliers  #

missing_Rating <- subset(Apps, is.na(Apps$Rating_Num == "TRUE"))

nrow(missing_Rating)
summary(Apps[,c('Rank','Price','Screenshots','Rating_Score','Rating_Num')])

## Step5: Normalization ##
# Q5: Create a function normalizing the values in Rating_Num   from 0.0 to 1.0 
F1 <- function(x) 
  {(x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}

Apps$Rating_Num <- F1(Apps$Rating_Num)
summary(Apps[,c('Rank','Price','Screenshots','Rating_Score','Rating_Num')])

## Step6: Variable Transformation ##
# Q6-1: Transform Rank to -log(Rank) and name it as "Sales"

Sales <- -log(Apps$Rank)

# Q6-2: Add "Sales" column into Apps data

Apps <- cbind(Apps, Sales)
head(Apps)


## Data Summary ##
# Summary Statistices #
summary(Apps[,c('Rank', 'Sales','Price','Screenshots','Rating_Score','Rating_Num')])

# A Correlation Matrix with variables having missing values #
cor(Apps[,c('Rank', 'Sales','Price','Screenshots','Rating_Score','Rating_Num')], use="complete.obs")
#correlation = cor, used for only numbers, complete.obs -> didn't used NA

######### Visialization ###############
install.packages('ggplot2')
library(ggplot2)


## Histogram ##
# Q7: Generate a histogram for Rating_Score using ggplot() 

ggplot(Apps, aes(factor(Rating_Score))) +
  geom_bar(stat = 'count', width =0.3) +
  ggtitle('User Review Socres')+
  xlab('Review Score')+ ylab('Num of Apps')

## Plot ##
plot(Apps$Rank,Apps$Sales)
  
## Scatter Plots ##
# Q8: Generate a scatter for Rating_Score using ggplot() 
ggplot(Apps, aes(x=Price , y=Sales )) +
  geom_point()
  ggtitle('Sales and Price')
  
## Matrix Plot ##
install.packages('ggcorrplot')
library(ggcorrplot)

# Q9: Generate a matirx plot presenting the correlations among Sales, Price, Rating_Score, and Rating_Num using ggcorrplot()

corr <- cor(Apps[,c('Sales','Price','Rating_Score','Rating_Num')], use="complete.obs")
ggcorrplot(corr, type="lower", lab=TRUE)


## Data Export ##
# Q10: Export the new Apps data (named as "Apps_new.csv")to the project folder    
write.csv(Apps, file="Apps_new.csv", row.names = FALSE)


