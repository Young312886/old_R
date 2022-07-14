library(dplyr)
library(ggplot2)
exam <- read.csv('data/csv_exam.csv')
exam
exam %>% filter(class == 1)
# pipeline 함수(연산자) link the 함수 c+s+m
exam %>% filter(class == 2)
exam %>% filter (math >= 90 | english >= 90)
#| == or 
exam %>% filter (class == 1| class == 3)
exam %>% filter (class %in% c(1,3))
class1 <- exam %>% filter (class ==1)
mean(class1)
mean(class1$math)


df <- as.data.frame(ggplot2::mpg)
lower <- df %>% filter(displ <= 4)
higher <- df %>% filter (displ >= 5)
k = mean(lower$hwy)
j = mean(higher$hwy)
k
j

audi <- df %>% filter(manufacturer == 'audi')
toyota <- df %>% filter(manufacturer == 'toyota')
mean(toyota$cty)
mean(audi$cty)

group1 <- df %>% filter(manufacturer %in% c('chevorlet','ford','honda'))
mean(group1$hwy)

exam %>% select(math)
exam %>% select(english)
exam %>% select(class, math, english)


exam %>% select(-math)
exam %>% 
  filter(class == 1) %>% 
  select(english)

exam %>% select(id, math) %>% head
#head also can be connected to the rest of the codes

car <- as.data.frame(ggplot2::mpg)
new_car <- car %>% select(class, cty) 
head(new_car)

suv <- new_car %>% filter(class == 'suv')
compact <- new_car %>% filter(class == 'compact')
mean(suv$cty)
mean(compact$cty)

exam %>% arrange(class, math) #기본 오름차순, 앞의 변수가 우선적으로 작용


car %>% filter( manufacturer == 'audi') %>% arrange(hwy) %>% tail(5)

car %>% filter( manufacturer == 'audi') %>% arrange(desc(hwy)) %>% head(5) ##desc means descend, upsidedown


#####mutate => adding directly to the pipeline in form of mathematical format

exam %>% mutate(total = math + english + science,
                mean = (math + english + science)/3) %>% head  
exam %>% mutate(test = ifelse(science >= 60, 'Pass','Fail')) %>% head

exam %>% 
  mutate(total = math+english+science) %>% 
  arrange(desc(total)) %>% 
head

car1 <- car %>% mutate( ady = cty+hwy)
head(car1)

car1 <- car1 %>% mutate(meany = ady/2)
head(car1)

car1 %>% arrange(desc(meany)) %>% head(3)

rank <-car %>% 
  mutate(ady = cty + hwy, meany = ady/2) %>% 
  arrange(desc(ady)) %>% 
  head(3)
rank
##pipeline code => able to write codes in order of right to left have to import magrittr  

exam %>%  summarise(mean_math = mean(math))

exam %>% group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n()) #빈도수 함수

mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

car %>% 
  filter(class == 'suv') %>% 
  mutate (ady = cty + hwy, meany = ady/2) %>% 
  group_by(manufacturer) %>% 
  summarise(mean_y = mean(meany)) %>% 
  arrange(desc(mean_y)) %>% 
  head(5)

car %>% 
  group_by(class) %>% 
  summarise(cty_mean = mean(cty)) %>% 
  arrange(desc(cty_mean))

car %>% 
  filter(class == "compact") %>% 
  group_by(manufacturer) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))
##datfarame adding (join) We need key to integrate two dataframes

test1<- data.frame(id = c(1,2,3,4,5),
                   mid = c(60,50,30,50,90))

test2<- data.frame(id = c(1,2,3,4,5),
                   final = c(69,30,20,50,80))
total <- left_join(test1, test2, by = "id")
total
data3 <- data.frame(id = c(6,7,8,9,10),
                    mid = c(02,50,70,30,60))
mid_total <- bind_rows(data3, total)
total
mpg %>% 
  left_join(mpg, fuel, by='fl')
mpg %>% 
  select(model, fl, price_fl) %>% 
  head(5)

df <- as.data.frame(ggplot2 :: midwest)
df
midwest <- midwest %>% 
  mutate( per = (poptotal-popadults) / poptotal*100)

midwest %>% 
  select(county, per) %>% 
  arrange(desc(per)) %>% head(5)
qplot(midwest$per)
midwest <- midwest %>% 
  mutate(ratio = ifelse(per >= 40, "large", ifelse ( per >=30, "middle","small"))) %>% group_by(ratio) 
table(midwest$ratio)
midwest <- midwest %>% mutate(asian_ratio = popasian/poptotal*100)
midwest %>% select(state, county, asian_ratio) %>% 
  arrange(asian_ratio) %>% head(10)
