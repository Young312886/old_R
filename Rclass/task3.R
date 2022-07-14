FN3 <- function(x,n) {sum((x^seq(n))/seq(n))}
FN3(1,3)

FN4 <- function(A,B) { 
     if(A>0 & B>0 & A%%2 == 1 & B%%2 == 1 ) (A+B)
     else if (A <0 & B <0 & A%%2 ==0 & B%%2 ==0) (A-B)
     else if ((A >0 & A%%2 ==0) | (B>0 & B%%2 ==0)) (A*B)
     else if ((A <0 & A%%2 ==1) | (B<0 & B%%2 ==1)) (A/B)
     else (A^B) }
FN4(1,1)