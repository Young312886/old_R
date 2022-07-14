##rm(list=ls())
#Importing Data#

list.files()
admission <- read.csv("admission.csv")
head(admission) # read first 6 row of data

# Data manipulation#
#Data overview#
View(admission)#uppercase "V" 400 obs. 변수개수, 4 variables
str(admission) #structure 
summary(admission) # min, max, median, mean 1Quarter - 25%, 3 Quar - 75%

colnames(admission)
dim(admission)

colnames(admission)[1] <- 'admit_rate' #Assign a new column
head(admission)
colnames(admission) [1] <- 'admit'

#indexing#
head(admission[,2])
head(admission[,'gre'])
head(admission$gre)

admission[3,]
admission[c(1,2),]
admission[,c(1,4)]
admission[,1:3]
admission[1:2,3:4]
