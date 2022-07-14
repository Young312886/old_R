library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(MASS)

HR_df <- read.csv('C:/Users/0ju/Desktop/금융데이터 분석가 과정/R programming/Data/HR_comma_sep.csv')


#2. 부서와 만족도가 영향이 있을 것인가.
sales_df <- HR_df %>%
  group_by(sales) %>%
  summarise(sales_mean = mean(satisfaction_level)) %>% 
  arrange(desc(sales_mean))
sales_df

#부서별 만족도 막대 그래프
# 막대그래프는 갯수를 표현하는데에 더 좋아보임
ggplot(data = sales_df, 
       aes(x=reorder(sales,sales_mean), 
           y=sales_mean))+geom_col()+coord_flip()

#만족도를 좀 다르게 표현해볼까..?
summary(HR_df$satisfaction_level)

HR_df<- HR_df %>% mutate(satis_char = ifelse (satisfaction_level<0.44, "bad",
                                      ifelse (satisfaction_level <0.64, "soso",
                                                      ifelse (satisfaction_level<0.82, "good",
                                                              ifelse (satisfaction_level<=1, "very good","best")))))
HR_df
# 적당한 수로 나누어졌는지 각 그룹의 갯수 확인
HR_df %>%
  group_by(satis_char) %>%
  summarise(count=n())


pie_df <- HR_df %>% group_by(sales, satis_char) %>% summarise(count=n())

pie_df


ggplot(data=pie_df, aes(x="", y=count, fill=satis_char)) +
  facet_grid(facets=. ~ sales)+
  geom_bar(stat="identity", width=1)#+
  coord_polar(theta="y") #pie plot

