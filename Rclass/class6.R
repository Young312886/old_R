rm(list=ls())
admission <- read.csv("admission.csv")
str(admission)

#Subset#
admission[admission$gpa > 3.5,]
subset(admission, gpa > 3.5)

admission[admission$rank == 4,]
subset(admission, rank == 4)

admission[(admission$gpa > 3.5 & admission $ gre > 780),]
subset(admission, gpa > 3.5 & gre>780)

admission[(admission$gpa > 3.5 | admission$gre > 780),]
subset(admission, gpa > 3.5 | gre> 780)

#Summary Statistics#
mean_gpa <- mean(admission$gpa); mean_gpa
median_gpa <- median(admission$gpa); median_gpa
sd_gpa <- sd(admission$gpa); sd_gpa
min_gpa <- min(admission$gpa); min_gpa
max_gpa <- max(admission$gpa); max_gpa
range_gpa <- range(admission$gpa); range_gpa

#Frequency#
freq <- table(admission$rank); freq

#Variable Transformation#
gre_gpa <- admission$gre * admission$gpa; gre_gpa
log_gpa <- log(admission$gpa); log_gpa

#Additional Row and Columns#

admission_new <- cbind(admission, data.frame(GRE_GPA = gre_gpa))
head(admission_new)

admission_new2 <- rbind(admission_new, data.frame(admit = 0, gre = 590, 
                                                 gpa = 2.92, rank = 4, GRE_GPA = 590*2.92))
nrow(admission_new2)

# Exporitng Data#
write.csv(admission_new2, file = "admission_new.csv")
write.csv(admission_new2, file="admission_new2.csv", row.names=FALSE) #remove row numbers


## For Loops ##
for (i in 1:5) {print (i)} # for ('variable' in 'sequence')

#REturn the number up to x#
for1 <- function(x) {for (i in 1:x) {print(i)}}
for1(5)

for2 <- function(x) {
  n <- 0;
  for (i in  1:x) {n <- n+1};
  n
}
for2(5)
#Exercise
M <- sum(seq(1:20)^4) * sum(1/(3+seq(1,5)))
M

for3 <- function(x){
  n <- 0;
  for (i in 1:x) {n <- n+i^4}
  n
}
for3(20)
for4 <- function(x){
  n<-0;
  for(i in 1:x) {n <- n+1/(3+i)}
  n
}
for4(5)
for3(20)*for4(5)

