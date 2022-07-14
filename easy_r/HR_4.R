library(readxl)
library(dplyr)
library(tidyverse)

HR_df <- read.csv('C:/Users/0ju/Desktop/금융데이터 분석가 과정/R programming/Data/HR_comma_sep.csv')


#4. 사건 발생 여부에 따른 근무 시간 차이
pro <- HR_df %>% 
  select(Work_accident,average_montly_hours,time_spend_company,number_project) %>% 
  group_by(Work_accident) %>% 
  summarise(mean_pro = mean(number_project),mean_hour = mean(average_montly_hours),mean_time = mean(time_spend_company))
pro  
