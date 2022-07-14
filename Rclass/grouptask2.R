rm(list=ls())
getwd()
# Step 2 # Understand Data
# Read As Folder
Productivity <- read.csv('PaidProductivity.csv')
Music <- read.csv('PaidMusic.csv')
Medical <- read.csv('PaidMedical.csv')
Games <- read.csv('PaidGames.csv')
Finance <- read.csv('PaidFinance.csv')
Education <- read.csv('PaidEducation.csv')
Business <- read.csv('PaidBusiness.csv')
Books <- read.csv('PaidBooks.csv')
Apps <- rbind(Productivity, Music, Medical, Games, Finance, Education, Business, Books)


rm(Productivity, Music, Medical, Games, Finance, Education, Business, Books)

summary(Apps[, c('Rank', 'Price', 'Screenshot', 'Size', 'RatingsAllVersions', 'RatingsCurrentVersion')])
summary(Apps)


# Step 3 # Data Preparation
# Create Relevant Variables
attach(Apps)
Sales <- -log(Rank) # Creat New Variables
Age_of_Apps <- as.numeric(as.Date('2013-06-19')-as.Date(ReleaseDate)+1)
Impact_of_Review <- RatingsAllVersions/as.numeric(Age_of_Apps)
Apps <- cbind(Apps, data.frame(Sales, Age_of_Apps, Impact_of_Review)) # Merge

# Replace Missing Values with Mean
Apps$Price[is.na(Price)==T] = mean(Price, na.rm=T)
Apps$Screenshot[is.na(Screenshot==T)] = mean(Screenshot, na.rm=T)
detach(Apps)

# Remove Outliers
for (i in 1:nrow(Apps)) {
  ifelse(Apps$RatingsAllVersions[i] > mean(Apps$RatingsAllVersions, na.rm=T)+3*sd(Apps$RatingsAllVersions, na.rm=T),
         Apps$RatingsAllVersions[i] <- NA, Apps$RatingsAllVersions[i])}
for (i in 1:nrow(Apps)) {
  ifelse(Apps$RatingsCurrentVersion[i] > mean(Apps$RatingsCurrentVersion, na.rm=T)+3*sd(Apps$RatingsCurrentVersion, na.rm=T),
         Apps$RatingsCurrentVersion[i] <- NA, Apps$RatingsCurrentVersion[i])}

# Normalize Data
Normalize=function(x) {log(x+1)}
Apps$RatingsAllVersions=(Normalize(Apps$RatingsAllVersions))
Apps$RatingsCurrentVersion=(Normalize(Apps$RatingsCurrentVersion))

summary(Apps[, c('Sales', 'Rank', 'Price', 'Screenshot', 'Age_of_Apps', 'Impact_of_Review',
                 'Size', 'RatingsAllVersions', 'RatingsCurrentVersion')])

# Create Dummies Variables
Apps <- transform(Apps,
                  Productivity=ifelse(Category=='Productivity',1,0),
                  Music=ifelse(Category=='Music',1,0),
                  Medical=ifelse(Category=='Medical',1,0),
                  Finance=ifelse(Category=='Finance',1,0),
                  Education=ifelse(Category=='Education',1,0),
                  Business=ifelse(Category=='Business',1,0),
                  Book=ifelse(Category=='Book',1,0))
#summary statistics of the selected variable

summary(Apps[, c('Sales', 'Rank', 'Price', 'Screenshot', 'Age_of_Apps', 'Impact_of_Review',
                 'Size', 'RatingsAllVersions', 
                 'RatingsCurrentVersion', 'Productivity', 
                 'Music', 'Medical', 'Finance','Education', 'Business', 'Book')])
#sd
sd(Apps$Sales)
sd(Apps$Rank)
sd(Apps$Price)
sd(Apps$Screenshot)
sd(Apps$Age_of_Apps)
sd(Apps$Impact_of_Review, na.rm=T)
sd(Apps$Size, na.rm=T)
sd(Apps$RatingsAllVersions,na.rm=T)
sd(Apps$RatingsCurrentVersion, na.rm=T)
sd(Apps$Productivity)
sd(Apps$Music)
sd(Apps$Medical)
sd(Apps$Finance)
sd(Apps$Education)
sd(Apps$Business)
sd(Apps$Book)

#observation
NumberofObservation <- function(x) {
  nrow(Apps)-nrow(subset(Apps, is.na(x) == "TRUE",))
}
NumberofObservation(Apps$Sales)
NumberofObservation(Apps$Rank)
NumberofObservation(Apps$Price)
NumberofObservation(Apps$Screenshot)
NumberofObservation(Apps$Age_of_App)
NumberofObservation(Apps$Impact_of_Review)
NumberofObservation(Apps$Size)
NumberofObservation(Apps$RatingsAllVersions)
NumberofObservation(Apps$RatingsCurrentVersion)
NumberofObservation(Apps$Productivity)
NumberofObservation(Apps$Music)
NumberofObservation(Apps$Medical)
NumberofObservation(Apps$Finance)
NumberofObservation(Apps$Education)
NumberofObservation(Apps$Business)
NumberofObservation(Apps$Book)


# Correlation with Complete Obeservations
Apps_Cor <- round(cor(Apps[, c('Sales', 'Rank', 'Price', 'Screenshot', 'Age_of_Apps', 'Impact_of_Review',
                               'RatingsAllVersions', 'RatingsCurrentVersion')], use='complete.obs'), digit=5)
Apps_Cor[upper.tri(Apps_Cor)] <- NA; Apps_Cor



# Step 5
# Regression
Apps_Reg <- lm(Sales ~ Price + Screenshot + Age_of_Apps + Impact_of_Review +RatingsAllVersions + RatingsCurrentVersion
               + Productivity + Music + Medical + Finance + Education + Business + Book, data=Apps)
summary(Apps_Reg)


# Step 6
# Data Export
write.csv(Apps, file='GBA3042_A2_G09.csv',row.names = F)