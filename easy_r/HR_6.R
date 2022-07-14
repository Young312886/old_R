library(readxl)
library(dplyr)
library(tidyverse)

HR_df <- read.csv('C:/Users/0ju/Desktop/금융데이터 분석가 과정/R programming/Data/HR_comma_sep.csv')

#6. 마지막 평가 점수 와 진행 프로젝트 수(프로젝트 수가 늘어날수록 평가 점수가 높다)

ev_num <- HR_df %>% 
  group_by(number_project) %>% 
  summarise(mean_ev = mean(last_evaluation)) 
ggplot(data = ev_num, aes(x=number_project, y=mean_ev))+geom_col() 
