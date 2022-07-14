#HR project
HR_df <- read.csv('data/HR_comma_sep.csv')

library(readxl)
library(dplyr)
library(ggplot2)


#14999개의 row와 10개의 col으로 구성
dim(HR_df)
HR_df%>% head(1)
#전체 데이터프레임에서 NA값을 봤을때 True가 없는 것으로 보아 결측치 없음
table(is.na(HR_df))

#1. 연봉이 높다면 만족도도 높을것이다.
salary_df <- HR_df %>%
  group_by(salary) %>%
  summarise(meandata = mean(satisfaction_level))
salary_df
#표 그리기
ggplot(data = salary_df, aes(x=salary, y=meandata, fill = salary))+geom_col()+scale_x_discrete(limits = c("high", "medium", "low"))+scale_fill_brewer(palette = 'Oranges')

#2. 부서와 만족도가 영향이 있을 것인가.
sales_df <- HR_df %>%
  group_by(sales) %>%
  summarise(sales_mean = mean(satisfaction_level)) %>% 
  arrange(desc(sales_mean))
sales_df
#표 그리기 
ggplot(data = sales_df, aes(x=reorder(sales,sales_mean), y=sales_mean,fill = sales))+geom_col()+coord_flip()+scale_fill_brewer(palette='Set3')
#3. 부서별, 임금별 만족도 비교
part_df <- HR_df %>%
  group_by(sales, salary) %>%
  summarise(mean_satisf = mean(satisfaction_level)) 
part_df

# 표 그림(Heatmap, 하지만 수정 예정정)
ggplot(part_df, aes(x =sales, y = salary, fill=mean_satisf)) + 
  geom_tile()+scale_fill_distiller(palette = "RdPu") + coord_flip() +scale_y_discrete(limits = c("high", "medium", "low"))


high_df<- part_df %>% 
  filter(salary == 'high') %>% 
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

#new_bind <- left_join(low_df,medium_df)
#new_bind <- left_join(new_bind, high_df)
#new_bind

high_df_t <- t(high_df)
medium_df_t <- t(medium_df)
low_df_t <- t(low_df)

bind_df <- rbind(high_df_t[2,],medium_df_t[2,],low_df_t[2,] )

new_col = c("accounting", "hr", "IT","management", "marketing", "product_mng", "RandD", "sales", "support", "technical")
new_row = c('high', 'medium','low')
colnames(bind_df)<- new_col
rownames(bind_df)<- new_row
bind_df
bind_df_copy <- bind_df
bind_df_copy$max_col = colnames(bind_df)[apply(bind_df, 1, which.max)]
max <- bind_df_copy$max_col

bind_df_real <- cbind(bind_df, max)

#참고 사항
data <- data.frame(x1 = c(0,1,0,1,7),
                   x2 = c(5,6,0,0,0),
                   x3 = 3)
data
colnames(data)
data[1,1:3]
max.col(data[1,1:3])
colnames(data)[max.col(data[1,1:3])]
colnames(data)[max.col(data[2,1:3])]


#4. 임금 차이에 따른 근속 년수 차 (높은 임금 = 긴 근속 년수)
salary_time <- HR_df %>% 
  group_by(salary) %>% 
  summarise(mean_time = mean(time_spend_company))
salary_time

# 표 
ggplot(data = salary_time, aes(x = reorder(salary, -mean_time), y = mean_time)) +geom_col()

#5. 마지막 평가 점수 와 진행 프로젝트 수(프로젝트 수가 늘어날수록 평가 점수가 높다)

ev_num <- HR_df %>% 
  group_by(number_project) %>% 
  summarise(mean_ev = mean(last_evaluation)) 
#유의미한 수치의 표표
ggplot(data = ev_num, aes(x=number_project, y=mean_ev,fill=number_project))+geom_col()+ scale_colour_manual(values=c('red', 'blue'))
