E <- seq(0.1, 1.7, by =0.4) ; E
F <- seq(10,6);  F
G <- (log(E,base=10) / F)*100;G

G[4] #1.627762

length(G) #5

H<- G[c(1:3,5)] ;H
H[5:6] <- c(10,20) ; H
names(H) <- c('First', 'Second', 'Third', 'Fourth', 'Fifth', 'Sixth') ;H
H[c('Second','Fifth')] <- c(30,40) ; H
H['First'] <- (H['Second']+H['Fourth'])/2 ; H
mean(H)
sd(H)

#Extra
X=c(3^seq(3,15,by=3)*4^seq(5,1,by=-1))
X
Y<-sum(5*(seq(10,100,by=1)^2)+3*(seq(10,100,by=1)^3))
Y