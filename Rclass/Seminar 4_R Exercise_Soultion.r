##R Exercise Tasks, S4##
rm(list=ls())



## Task 1 ##
FN1 <- function(Vec) {Vec^seq(1,length(Vec))}
Vec <- 1:3
FN1(Vec)

FN2 <- function(p,d) {p*(1-d/100)}
FN2(2.99, 10)


## Task 2: Function ##
FN3 <- function(x, n) {sum((x^seq(1,n)/seq(1,n)))}
FN3(1,3) 


## Task 3: IF-ELSE ##

FN4 <- function(A,B){
  P <- A > 0 & B > 0
  N <- A < 0 & B < 0
  
  E <- A%%2 == 0 & B%%2 == 0
  O <- A%%2 != 0 & B%%2 != 0
  
  A_PE <- A > 0 &  A%%2 == 0   
  B_PE <- B > 0 &  B%%2 == 0
  
  A_NO <- A < 0 &  A%%2 != 0 
  B_NO <- B < 0 &  B%%2 != 0
  
  if(P & O) {A+B} else if(N & E) {A-B} 
  else if(A_PE | B_PE) {A*B} else if(A_NO | B_NO) {A/B}
  else {A^B}
}

FN4(1,1)
FN4(1,2)
FN4(2,1)
FN4(2,2)
FN4(-1,-1)
FN4(-1,-2)
FN4(-2,-1)
FN4(-2,-2)