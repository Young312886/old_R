## Matrix ##

A<- seq(1,6) ; A # vector
Mat1 <-matrix(A) ;Mat1 
Mat2 <-matrix(A,3) ;Mat2 # A vectors , rows, fill in columns
Mat3 <-matrix(A,3,byrow=TRUE) ; Mat3 #byrow - fill in row first

Mat4 <- matrix(A,2,3) ;Mat4 # values , rows, columns
Mat5 <- matrix(A,2,3,byrow=TRUE) ;Mat5 
Mat6 <- matrix(0,2,3) ; Mat6 

#indexing#
Mat5
Mat5[,1]#first columns
Mat5[1,]#fisrt row
Mat5[1,1]
Mat5[1, 1:2]

# Add columns and rows#
Mat5 <- rbind(Mat5, c(10,20,30)) ;Mat5
Mat5 <- cbind(Mat5, c(30,40,50)) ;Mat5

#Naming#
colnames(Mat5)<- c('First', 'Second', 'Third', 'Fourth')
Mat5
Mat5[,'Second'] # = Mat5[,2]
nrow(Mat5)
ncol(Mat5)

Mat7 <- matrix(c(1,2,'a','d'),2,2); Mat7
class(Mat7) # Matrix is also class
class(Mat7[,1]) #Matrix value are same class - character
class(Mat7[,2])

#Data Frame#
idx <- 1:4; idx
name <- c('Apple', 'Peach', 'Banana', 'Grape') ; name
unit_price <- c(10,8,6,12);unit_price
quantitiy <- c(5,2,4,7); quantitiy
sales <- data.frame(ID=idx, Product=name, Price = unit_price, QTY = quantitiy)
sales
#indexing#
sales[,2]
sales[,'Product']
sales$Product # = sales[,'Product'] 

sales[3,]
sales[c(1,2),]
sales[,c(1,2)]
sales[,1:3]
sales[1:2,3:4]


#subset#
sales[sales$QTY<5,]
sales[sales$Price == 10,]
sales[sales$Product == 'Banana',]

subset(sales, QTY<5) #subset = $
subset(sales, Price ==10)
subset(sales, Product == 'Banana')

#Math Operation#
total_amount = sales$Price * sales$QTY; total_amount

#additional rows and columns

sales <- cbind(sales,data.frame(Amount=total_amount)); sales
sales <- rbind(sales,data.frame(ID=5,Product='Berry', Price =7,QTY=5, Amount = 35))
sales # put every right value in right name

#useful Functions#
nrow(sales)
ncol(sales)
names(sales)
colnames(sales)
#matrix = all the value in matrix must be same
#data.frame = more flexible, different row is different combination

#Task1#

id <- c('PO1','PO2','PO3','PO4')
name<- c('Monitor','Mouse','Keyboard','Printer')
price <- c(300.99, 50.50, 40.38, 75.52)
quantitiy <- c(9, 30, 25, 9)
sale <- c(50, 20, 0, 30)

Sales <- data.frame(ID = id, Name = name, Price = price, QTY = quantitiy, Discount = sale);Sales
Sales <- rbind(Sales, data.frame(ID='PO5', Name = 'Speaker', Price = 39.99, QTY = 9, Discount = 15)) ;Sales
discount = (Sales$Price * (100-Sales$Discount)/100)
Sales <- cbind(Sales, data.frame(Final_price = discount));Sales
subset(Sales,Final_price<mean(Sales$Final_price),Name)
