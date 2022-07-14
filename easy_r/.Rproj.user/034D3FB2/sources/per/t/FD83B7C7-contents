data <- data.frame(x1 = c(0,1,0,1,7),
                   x2 = c(5,6,0,0,0),
                   x3 = 3)
data
colnames(data)
data[1,1:3]
max.col(data[1,1:3])
colnames(data)[max.col(data[1,1:3])]
colnames(data)[max.col(data[2,1:3])]

library(ggplot2)
data<- as.data.frame(ggplot2::mpg)
ggplot(data = mpg, aes(x = displ, y=hwy)) #배경 생성
ggplot(data = mpg, aes(x = displ, y=hwy)) + geom_point()
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point()+
  xlim(3,6)+
  ylim(10,30)
ggplot( data= mpg, aes (x = cty, y = hwy)) +
  geom_point()
ggplot(data = midwest, aes(x=poptotal,y=popasian))+
  geom_point()+
  xlim(0,500000)+
  ylim(0,10000)
library(dplyr)
mpg <- as.data.frame(ggplot2::mpg)
df_mpg <-mpg %>% group_by(drv) %>% 
  summarise((mean_hwy = mean(hwy)))
df_mpg

ggplot(data = df_mpg, aes(x=drv,y= mean_hwy)) + geom_col() #단순 막대 그래프

ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col() 

ggplot(data = mpg, aes(x =drv))+geom_bar() # 빈도수, we don't need y value

df_new <- mpg %>% filter(class == 'suv') %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)
df_new


ggplot(data = df_new, aes(x=reorder(manufacturer,-mean_cty), y=mean_cty)) + geom_col()

ggplot(data = mpg, aes(x = class)) + geom_bar()
economics = as.data.frame(ggplot2::economics)
ggplot(data = economics, aes(x=date, y = psavert))+geom_line()
ggplot(data = mpg, aes(x=drv,y=hwy)) +geom_boxplot()

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss(file = 'data/Koweps.sav', to.data.frame = T)
welfare <- raw_welfare
head(welfare)
summary(welfare)
welfare <- rename(welfare, 
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_regio = h10_reg7)
#변수 전처리 this is how to prevent the error
#우선 변수의 클래스 확인
class(welfare$sex)
#변수들의 상태 확인(빈도수)
table(welfare$sex)  
#결측치 처리
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
#확인
table(is.na(welfare$sex))
#Changing the key to what we want
welfare$sex <- ifelse(welfare$sex == 1, 'male','female')
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)
#we can check the merge
summary(welfare$income) #to check the outlier
qplot(welfare$income)+xlim(0,1000)
#as the value should be range of 0,9998
welfare$income <- ifelse(welfare$income %in% c(0,9999),NA, welfare$income)
table(is.na(welfare$income))
sex_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by (sex) %>% summarise(mean_income = mean(income))
sex_income
ggplot(data= sex_income, aes(x=sex,y=mean_income)) +geom_col()

class(welfare$birth)
summary(welfare$birth)
table(is.na(welfare$birth)) #반드시 결측치 체크
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)

welfare$age <- 2015 - welfare$birth


age_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by (age) %>% summarise(mean_income = mean(income))
age_income
ggplot(data = age_income, aes(x=age,y=mean_income))+geom_line()


welfare <- welfare %>% mutate(age_group = age %/% 10)
table(welfare$age_group)
group_income <- welfare %>% filter(!is.na(income)) %>% group_by(age_group) %>%   summarise(mean_income = mean(income))
group_income
ggplot(data= group_income, aes(x=age_group, y=mean_income)) + geom_col()

sex_income<-welfare %>% filter(!is.na(income)) %>% 
  group_by(age_group,sex) %>% 
  summarise(mean_income = mean(income))
sex_income
ggplot(data = sex_income, aes(x =age_group, y= mean_income, fill=sex))+geom_col(position = 'dodge') #분리하기  없으면 위아래로 표시됨
ggplot(data = sex_income, aes(x =age_group, y= mean_income, fill=sex))+geom_line(position = 'dodge')

#직업별 월급 차이 
class(welfare$code_job)
#숫자이다
library(readxl)
job_list <- read_excel('data/Koweps_Codebook.xlsx', sheet = 2, col_names =T)
class(job_list)
welfare <- left_join(welfare, job_list, by="code_job")

welfare %>% filter(!is.na(code_job)) %>% select(code_job, job) %>% head()

job_salary <- welfare %>% filter(!is.na(income)&!is.na(job)) %>% 
  group_by(job) %>% summarise(mean_in = mean(income)) 
job_salary
top_10 <- job_salary %>% arrange(desc(mean_in)) %>% head(10)
top_10
ggplot(data = top_10, aes(x = reorder(job, mean_in), y=mean_in))+geom_col()+coord_flip()
#아래에서 10
bot10 <- job_salary %>% arrange(mean_in) %>% head(10)
ggplot(data = top_10, aes(x = reorder(job, -mean_in),y=mean_in))+geom_col()+coord_flip()+ylim(0,850)

#남성 직업 빈도수

job_male <- welfare %>% 
  filter(!is.na(job)&sex == 'male') %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
ggplot(data = job_male, aes(x = reorder(job,n), y = n)) + geom_col()+coord_flip()

job_female <- welfare %>% 
  filter(!is.na(job)&sex == 'female') %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
ggplot(data = job_female, aes(x = reorder(job,n), y = n)) + geom_col()+coord_flip()

#종교
class(welfare$religion)
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion == 1, 'Yes','No')
qplot(welfare$religion)
#Change the class
class(welfare$religion)

#결혼 구체 사항

class(welfare$marriage)
table(welfare$marriage)

welfare$marriage <- ifelse(welfare$marriage == 1, 'marriage',
                           ifelse(welfare$marriage==3, 'divorce',NA))
table(welfare$marriage)                           
table(is.na(welfare$marriage))
#결혼&종교 혼합 
rel_mar <- welfare %>% 
  filter(!is.na(marriage)) %>% 
  group_by(religion,marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))
rel_mar
#count로 다르게 측정할 수도 있다.
# 이혼 추출
divorce <- rel_mar %>% 
  filter(marriage == 'divorce') %>% 
  select(religion, pct)
divorce

ggplot(data = divorce, aes(x = religion, y = pct))+geom_col()

#age & marriage

age_mar <- welfare %>% 
  filter(!is.na(marriage)) %>% 
  group_by(age_group,marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))
age_mar

#특정 연령대 이혼률 검색
mar_10 <- age_mar %>% 
  filter(age_group != 10 & marriage == 'divorce') %>% 
  select(age_group , pct)
ggplot(data = mar_10, aes(x = age_group, y = pct)) + geom_col()

# 변수 3개, 연령대 별 종교에 따라서의 결혼 율
age_mar_rel <- welfare %>% 
  filter(!is.na(marriage) & age_group != 10) %>% 
  group_by(age_group, religion, marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot*100))

df_divorce <- age_mar_rel %>% 
  filter(marriage == 'divorce') %>% 
  select(age_group, religion, pct)
ggplot(data = df_divorce, aes(x = age_group, y = pct, fill = religion))+geom_bar(stat = 'identity',position = 'dodge')+guides(fill=guide_legend(reverse=TRUE))

