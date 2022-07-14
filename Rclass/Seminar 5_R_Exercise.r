#### R Exercise_S5 ####
rm(list=ls())



## Importing Data ##
# Make Sure to store admission.csv in the project folder #

list.files()   # list files in the project folder 

admission <- read.csv("admission.csv")
head(admission)

## Data Manipulation ##
# Data Overview #
View(admission)   
str(admission)
summary(admission)

colnames(admission)
dim(admission)

colnames(admission)[1] <- 'admit_rate' # Assign a new column name
head(admission)
colnames(admission)[1] <- 'admit'

# Indexing #
head(admission[,2])
head(admission[, 'gre'])
head(admission$gre)

admission[3,]
admission[c(1,3),]
admission[,c(1,4)]
admission[,1:3]
admission[1:2, 3:4]



# Subset #
admission[admission$gpa > 3.5,]    #GPA > 3.5
subset(admission, gpa > 3.5)       #GPA > 3.5   

admission[admission$rank == 4,] 
subset(admission, rank == 4)       

admission[(admission$gpa > 3.5 & admission$gre > 780), ] 
subset(admission, gpa > 3.5 & gre > 780) 

admission[(admission$gpa > 3.5 | admission$gre > 780), ] 
subset(admission, gpa > 3.5 | gre > 780) 




# Summary Statistics #
mean_gpa   <- mean(admission$gpa); mean_gpa
median_gpa <- median(admission$gpa); median_gpa
sd_gpa     <- sd(admission$gpa); sd_gpa
min_gpa    <- min(admission$gpa); min_gpa
max_gpa    <- max(admission$gpa); max_gpa
range_gpa  <- range(admission$gpa); range_gpa


# Frequency #
freq <- table(admission$rank); freq





# Variable Transformation #
gre_gpa <- admission$gre * admission$gpa; gre_gpa 
log_gpa <- log(admission$gpa); log_gpa


# Additional Rows and Columns #
admission_new <- cbind(admission, data.frame(GRE_GPA = gre_gpa))
head(admission_new)

admission_new2 <- rbind(admission_new, data.frame(admit = 0, gre = 590,
                            gpa = 2.92, rank = 4, GRE_GPA = 590*2.92))
										 										  
nrow(admission_new2)

# Exporting Data #
write.csv(admission_new2, file="admission_new.csv")

write.csv(admission_new2, file="admission_new2.csv", row.names=FALSE)




## For Loops ##
for (i in 1:5) {print(i)}   # for ('variable' in 'sequence') {instruction}


# Return the numbers up to x #
for1 <- function(x) {for (i in 1:x) {print(i)}}
for1(5)


# Sum the numbers up to x #
for2 <- function(x) {
  m <- 0; 
  for (i in 1:x) {m <- m +i}; 
  m 
}   
for2(5)

