rm(list=ls())
#STEP 0
adm <- read.csv("admission.csv")

#STEP 01
gre_Rank <- ifelse (adm$gre < mean(adm$gre), "LOW","HIGH")
gpa_Rank <- ifelse (adm$gpa < mean(adm$gpa), "LOW","HIGH")
sch_Rank <- ifelse (adm$rank < mean(adm$rank), "LOW","HIGH")

adm <- cbind(adm, data.frame(GRE_Rank = gre_Rank)
              ,data.frame(GPA_Rank = gpa_Rank)
              ,data.frame(SCH_Rank = sch_Rank))
head(adm)

#STEP 02
#Q1
nrow(subset(adm, GPA_Rank == "HIGH" & GRE_Rank == "LOW"))
#Q2
nrow(subset(adm, SCH_Rank=="LOW" & admit==1))
#Q3
mean(adm[adm$SCH_Rank!='HIGH', 'gpa'])

#STEP 03
x <- ifelse(adm$GPA_Rank=='HIGH', 100, 50)
y <- ifelse(adm$GRE_Rank=='HIGH', 100, 50)  
z <- ifelse(adm$SCH_Rank=='HIGH', 100, 50)

overall_score <- 0.4*x+0.4*y+0.2*z
adm <- cbind(adm, data.frame(Overall_Score=overall_score))
head(adm)

#Step 04

Cal_Prob <- function(x,y) {nrow(y)/nrow(x)} # Used the function for repeating calculation

#Q4. 
A <- subset(adm, GRE_Rank == "LOW")
B <- subset(A, admit ==1)
prob_Q4 <- Cal_Prob(A,B) ; prob_Q4

#Q5
C <- subset(adm, GPA_Rank != "HIGH")
D <- subset(C, admit==0)
prob_Q5 <- Cal_Prob(C,D) ;prob_Q5

#Q6
E <- subset(adm, SCH_Rank == "LOW")
G <- subset(E, admit == 1)
prob_Q6 <- Cal_Prob(E,G) ;prob_Q6

#Q7
H <- subset(adm, GRE_Rank == "HIGH" & GPA_Rank != "HIGH")
I <- subset(H, admit == 0)
prob_Q7 <- Cal_Prob(H,I) ;prob_Q7

#Q8
J <- subset(adm, GPA_Rank == "LOW" | SCH_Rank == "LOW") 
K <- subset(J, admit == 1) 
prob_Q8 <- Cal_Prob(J,K) ;prob_Q8

#Q9
L <- subset(adm, Overall_Score > 75 & SCH_Rank == "HIGH")
M <- subset(L, admit == 1)
prob_Q9 <- Cal_Prob(L,M) ; prob_Q9

#Q10
N <- subset(adm, Overall_Score < 30 | GPA_Rank != "HIGH")
O <- subset(N, admit == 0)
prob_Q10 <- Cal_Prob(N,O) ; prob_Q10

#Step 05
write.csv(adm, file = "GBA3042_A1_G09.csv")


