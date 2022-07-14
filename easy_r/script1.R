a <- 1
a
b <- 2
b *3
b <- 2
b
c <- 3
c
d <- 3.5
d
a+b
a+b+c
4/b
5*b
var1 <- c(1,2,4,5,6)
var2 <- c(1:5)
var3 <- seq(1,5)
var4 <- seq(1,10, by = 3)
var4
var1 + var3
str1<- 'a'
str2 <- 'text'
str2
print(str1)
c(str2)
str4 <- c('a','b','c')
str4
##to input more than one element, use c
mean(var1)
max(var3)
min(var4)
paste(str4,collapse = ',')
paste(str4,collapse= ' ')
#if there are return value, we can designated those values to the x
install.packages('ggplot2')
library(ggplot2)
x<- c('a','b','c')
qplot(x)
num <- c(80,60,70,50,90)
mean(num)
num_mean <- mean(num)
num_mean
install.packages('readxl')
library(readxl)
df_exam <- read_excel('excel_exam.xlsx')
df_exam
df_exam <- read_excel('data/excel_exam.xlsx')
df_exam
df_exam_nova <- read_excel('data/excel_exam_novar.xlsx')
df_exam_nova
df_exam_nova <- read_excel('data/excel_exam_novar.xlsx', col_names = F)
df_exam_nova
df_excel_sheet <- read_excel('data/excel_exam_sheet.xlsx', sheet = 3)
df_excel_sheet
df_csv <- read.csv("data/csv_exam.csv")
df_csv
df_midterm <- data.frame(english = c(90,80,60,40),
                         math = c(50,60,40,100),
                         class = c(1,1,2,2))
write.csv(df_midterm, file = 'df_midterm.csv')
saveRDS(df_midterm, file = 'df_midterm.rds')
#faster, lighter but only use R
rm(df_midterm) #rm() = delet,reset
df_midterm
df_midterm <- readRDS('df_midterm.rds')
df_midterm
View(exam)
exam <- df_excel_sheet
View(exam)
mpg <- as.data.frame(ggplot2::mpg)
#ggplot2에는 다양한 데이터프레임이 저장되어 있다
head(mpg)
?mpg
summary(mpg)
install.packages('dplyr')
library(dplyr)
df_raw <- data.frame(var1 = c(1,2,1), var2 = c(2,3,2))
df_new <- df_raw
df_new<- rename(df_new, v2 = var2)
df_new
df_raw
df_raw$var_sum <- df_raw$var1 + df_raw$var2
df_raw
#지시는 $ 표시로로
mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)
mean(mpg$total)
summary(mpg$total)
hist(mpg$total)
#조건 넣기
mpg$test <- ifelse(mpg$total >= 20, 'pass','fail')
head(mpg,20)
tabel(mpg$test)
table(mpg$test)
#value count랑 동일, 빈도표이다
gplot(mpg$test)
library(ggplot2)
qplot(mpg$test)
mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse (mpg$total >= 20, "B", "C"))
head(mpg,10)
qplot(mpg$grade)
mpg <- as.data.frame(ggplot2::mpg)
library(dplyr)
library(ggplot2)

head(mpg)
tail(mpg)
View(mpg)
dim(mpg)
str(mpg)
summary(mpg)

mpg<-rename(mpg, company = manufacturer)

mpg$total <- (mpg$cty + mpg$hwy)/2

qplot(mpg$total)

mid <- as.data.frame(ggplot2::midwest)
View(mid)
summary(mid)
mid2 <- rename(mid, total = poptotal, asian = popasian)
mid2
head(mid)
head(mid2)
mid2$per <- (mid2$asian/mid2$total)
qplot(mid2$per)
hist(mid2$per)
k <- mean(mid2$per)
mid2$over <- ifelse(mid2$per > k, "large", "small")
table(mid2$over)
qplot(mid2$over)
