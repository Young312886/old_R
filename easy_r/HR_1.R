#HR project
HR_df <- read.csv('C:/Users/0ju/Desktop/금융데이터 분석가 과정/R programming/Data/HR_comma_sep.csv')

library(readxl)
library(dplyr)
library(tidyverse)

#14999개의 row와 10개의 col으로 구성
dim(HR_df)

#첫번째 줄 출력해서 어떤 값들이 들어가있는지 확인
HR_df%>% head(1)

#결측치 확인
table(is.na(HR_df))
#전체 데이터프레임에서 NA값을 봤을때 True가 없는 것으로 보아 결측치 없음

#################################################################
#가설1. 연봉이 높다면 만족도도 높을것이다.
salary_df <- HR_df %>%
  group_by(salary) %>%
  summarise(meandata = mean(satisfaction_level))
salary_df

#막대 그래프
ggplot(data = salary_df, aes(x=salary, y=meandata))+geom_col()
#연봉 high, medium, low 군집별 만족도의 평균을 낸 값을 막대 그래프로 보았기 때문에
# 만족도 평균은 high그룹이 높다는 것을 알 수 있지만  
# 해당 군집별 만족도 분포를 알기 어렵다는 단점이 있었다.
# box plot으로 대략적인 분포까지 확인해보자

#box plot
#전체 satisfaction_level의 분포를 보면 
boxplot(HR_df $ satisfaction_level)
max(HR_df$satisfaction_level)
min(HR_df$satisfaction_level)
#min 0.09~ max 1까지 넓게 존재

#이제 연봉 h,m,l 별 만족도 분포를 그려보자자
boxplot(satisfaction_level~salary,
        data = HR_df,
        main = "연봉에 따른 만족도 분포",
        xlab = "salary",
        ylab = 'satisfaction',
        col = 'orange')
# 연봉이 high인 군집이 medium, low에 비해 
# 제1사분위수부터 제3사분위수까지의 영역이 다른 군집에 비해 좁은 것으로 보인다.