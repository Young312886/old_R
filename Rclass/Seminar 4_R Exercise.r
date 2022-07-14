#### R Exercise 4 ####
rm(list=ls())

## Functions ##
M <- seq(1,5)
mean(M) 
help(mean)
example(mean)

#Create a function#
My_mean <- function(x) {sum(x)/length(x)} 
My_mean(M)

My_mean2 <- function(x,y) {sum(x:y)/length(x:y)}
My_mean2(1,5)

Mat1 <- matrix(1,2,3); Mat1

Mat_dim <- function(x) {c(nrow(x),ncol(x))}
Mat_dim(Mat1)




## IF Else Statements##
Num1 <- 3
Num2 <- -3
Num3 <- 0

# with one condition#
if(Num1 > 0) {"positive number"}  
if(Num2 > 0) {"positive number"} 
if(Num3 > 0) {"positive number"} 

# with two conditions #
if(Num1 > 0) {"positive number"} else {"negative number"}
if(Num2 > 0) {"positive number"} else {"negative number"}
if(Num3 > 0) {"positive number"} else {"negative number"}

# with three or more conditions #
if(Num1 > 0) {"positive number"} else if (Num1 <0) {"negative number"} else {"Zero"}
if(Num2 > 0) {"positive number"} else if (Num2 <0) {"negative number"} else {"Zero"}
if(Num3 > 0) {"positive number"} else if (Num3 <0) {"negative number"} else {"Zero"}



# ifelse(a,b,c): If a is true, it returns b. otherwise, it returns c. #
ifelse(Num1 > 0, "Positive Number", "Negative Number")

ifelse(Num3 > 0, "Positive Number", ifelse(Num3 <0, "Negative Number", "Zero"))

3%%2  # Modulus operation 
      # %% finds the remainder after division of one number by another

ifelse(Num1%%2 == 0, "Even Number", "Odd Number") # X%%Y is X mod Y






# Return A*B if A and B are both positive #
# Return A+B if either A or B is negative or zero #
fn1 <- function(A,B) {
  if ((A >= 1) & (B >= 1)){A*B}
  else {A+B}
}

fn1(2,3)
fn1(2,-1)

fn2 <- function(A,B) {
  if ((A <= 0) | (B <= 0)){A+B}  # | means 'or'
  else {A*B}
}

fn2(2,3)
fn2(2,-1)

fn3 <- function(A,B) {ifelse((A >= 1) & (B >= 1), A*B, A+B)}

fn3(2,3)
fn3(2,-1)	







