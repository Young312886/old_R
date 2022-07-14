rm(list=ls(all=TRUE)) # Clear all 


### Step 2: Understand Data ###
teens <- read.csv("snsdata.csv")
str(teens)

### Step 3: Prepare Data ###

## Convert characters to numbers ##
teens$female <- ifelse(teens$gender == "F", 1, 0)

## Select the variables ##
interests <- teens[5:40]

## Standardize the variables ##
interests_Stand <- scale(interests)

## Before Standardization ##
head(interests)
## After Standardization ##
head(interests_Stand)

### Step 5: Train a Model on the Data ##
## K-means Clustering ##
library("stats")
set.seed(1234)
teen_KM <- kmeans(interests_Stand, 5)

## Available Attributes ##
attributes(teen_KM)

### Step 6: Evaluating model performance ###
## Output ##
teen_KM

# look at the size of the clusters#
teen_KM$size

# look at the cluster centrers #

teen_KM$centers
# look at the clusters for teenagers# 
teen_KM$cluster

## Determine the appropriate value of k ##
#Within Cluster Sum of Squares (WSS)#
wss <- 1:36

for(i in 1:36) {wss[i] <- sum(kmeans(interests_Stand,i)$withinss)}

plot(1:36, wss[1:36], type="b", xlab="Number of Clusters", ylab="Within Cluster Sum of Squares")

#type="b" creates a plot with lines between points # 



############################# Hierarchical Clustering #############################


### Step 5: Train a Model on the Data ###
### Hierarchical Clustering: Case1 ###
## Distance between records ##
distance <- dist(t(interests_Stand), method = "euclidean") # Euclidean distance matrix

## HC with Ward's Distance  ##
teen_HC <- hclust(distance, method = "ward.D")

plot(teen_HC)

# Draw dendogram with red borders around the 4 or 5 clusters #
plot(teen_HC)
rect.hclust(teen_HC, k=4, border="red")

plot(teen_HC)
rect.hclust(teen_HC, k=5, border="red")

# cut tree into 5 clusters
teen_HC_Cut <- cutree(teen_HC, k=5) 
teen_HC_Cut

#######################################################################
### Hierarchical Clustering ###
## Distance between records: Case 2 ##
distance2 <- dist(interests_Stand, method = "euclidean") # Euclidean distance matrix

## HC with Ward's Distance  ##
teen_HC2 <- hclust(distance2, method = "ward.D")

plot(teen_HC2)

# Draw dendogram with red borders around the 4 or 5 clusters #
plot(teen_HC2)
rect.hclust(teen_HC2, k=4, border="red")

plot(teen_HC2)
rect.hclust(teen_HC2, k=5, border="red")

# cut tree into 5 clusters
teen_HC_Cut2 <- cutree(teen_HC2, k=5) 
teen_HC_Cut2
table(teen_HC_Cut2)


