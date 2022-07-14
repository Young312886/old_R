for3 <- function(x){
  n <- 0;
  for (i in 1:x) {n <- n+i^4}
  n
}
for4 <- function(x){
  n<-0;
  for(i in 1:x) {n <- n+1/(3+i)}
  n
}

for3(20)*for4(5)
F1 <- for (i in 1:20) {m <- m + i^4}
F2 <- for (k in 1:5)  {n <- n + 1/(3+i)}
m*n