##missing values
library(ggplot2)
library(dplyr)

df <- data.frame(sex = c("M","F",NA,"F","S"),
                 score = c(5,4,6,4,NA))
df

table(is.na(df))

df %>% filter(is.na(score))
df %>% filter(!is.na(score)) #na 제외 출력
df_nomis <- df %>% filter(!is.na(score))
mean(df_nomis$score)
df_nomiss <- df %>% filter(!is.na(score)&!is.na(sex))
df_nomiss

df_nomiss2 <- na.omit(df)
df_nomiss2 #erase all the missing values, but we have to use it carefully

mean(df$score, na.rm = T ) #na.rm enable to calculate by neglecting the NA

exam <- read.csv('data/csv_exam.csv')
exam[c(3,8,15), 'math'] <-NA #change the values into NA
exam
exam %>% summarise(mean_math = mean(math))
exam %>% summarise(mean_math = mean(math,na.rm =T))

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212),'hwy'] <- NA

df_mis <- mpg %>% filter(is.na(drv) | is.na(hwy))
table(is.na(df_mis))

outlier <- data.frame(sex = c(1,2,3,1,2),
                      score = c(2,5,4,2,7))
outlier
table(outlier$sex)
table(outlier$score)

outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex )
outlier$score <- ifelse(outlier$score >5, NA, outlier$score)
table(is.na(outlier))

outlier %>% 
  filter(!is.na(score) & !is.na(sex)) %>% 
  group_by(sex) %>% 
  summarise(mean_s = mean(score))

boxplot(mpg$hwy)$stats  #picture + number print
mpg [c(10,14,58,93), 'drv'] <- 'k'
mpg[c(29,42,129,203),'cty'] <- c(3,4,13,42)

table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"),mpg$drv, NA)
# if value is one of them, return original ,if not, return NA
table(mpg$drv)
