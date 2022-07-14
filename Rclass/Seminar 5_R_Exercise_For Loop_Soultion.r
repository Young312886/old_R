## Task 1: Review ##

M <- sum(seq(1:20)^4) * sum(1/(3+seq(1,5))); M 
#or, M <- sum((1:20)^4) * sum( 1/(4:8))



## Task 2: Extra Credit Point ##

# Answer 1
A <- 0
for(i in 1:20) { for(k in 1:5)  { A <- A + (i^4/(3+k)) } }

A
 
 
# Answer 2
m <- 0
n <- 0
F1 <- for (i in 1:20) {m <- m + i^4}
F2 <- for (k in 1:5)  {n <- n + 1/(3+k)}

m*n
  

