library(readxl)
library(dplyr)
library(tidyverse)

HR_df <- read.csv('C:/Users/0ju/Desktop/금융데이터 분석가 과정/R programming/Data/HR_comma_sep.csv')


#so we can't expect that if someone is taking more works, probability of the accident increase
#5. 임금 차이에 따른 근속 년수 차 (높은 임금 = 긴 근속 년수)
salary_time <- HR_df %>% 
  group_by(salary) %>% 
  summarise(mean_time = mean(time_spend_company))
salary_time
ggplot(data = salary_time, aes(x = reorder(salary, -mean_time), y = mean_time)) +geom_col()