library(readxl)
library(dplyr)
library(tidyverse)
select <- dplyr::select
HR_df <- read.csv('C:/Users/0ju/Desktop/금융데이터 분석가 과정/R programming/Data/HR_comma_sep.csv')


#3. 부서별, 임금별 만족도 비교
part_df <- HR_df %>%
  group_by(sales, salary) %>%
  summarise(mean_satisf = mean(satisfaction_level)) 

part_df

high_df<- part_df %>% 
  filter(salary == 'high')%>% 
  select(sales,mean_satisf) %>% 
  rename(high_satis = mean_satisf)

medium_df<- part_df %>% 
  filter(salary == 'medium')%>% 
  select(sales, mean_satisf ) %>%  
  rename(med_satis = mean_satisf)


low_df <- part_df %>% 
  filter(salary == 'low') %>% 
  select(sales, mean_satisf ) %>% 
  rename(low_satis = mean_satisf)


high_df_t <- t(high_df)
medium_df_t <- t(medium_df)
low_df_t <- t(low_df)

bind_df <- rbind(high_df_t[2,],medium_df_t[2,],low_df_t[2,])

new_col = c("accounting", "hr", "IT","management", "marketing", "product_mng", "RandD", "sales", "support", "technical")
new_row = c('high', 'medium','low')

colnames(bind_df)<- new_col
rownames(bind_df)<- new_row


bind_df
bind_df_copy <- bind_df

bind_df_copy$max_col = colnames(bind_df)[apply(bind_df, 1, which.max)]

max <- bind_df_copy$max_col

bind_df_real <- cbind(bind_df, max)
bind_df_real

#시각화화
ggplot(part_df, aes(x =sales, y = salary, fill=mean_satisf)) + 
  geom_tile()+scale_fill_distiller(palette = "RdPu") + coord_flip() +scale_y_discrete(limits = c("high", "medium", "low"))



