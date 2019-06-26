### section 01. 그래프

setwd('D:/Heechul/R_Statistics/Lecture/chapter02')
library(ggplot2)
par(mfrow=c(1,1))

## 예제 2-1. 두 변수 간의 관계를 나타내는 산점도
# cars 선점도
str(cars)
head(cars)
ggplot(cars, aes(x=speed, y= dist)) +
  geom_jitter(pch=1, col='red') +
  xlab('속도(mph)') +
  ylab('제동거리(ft)')


plot(cars$speed, cars$dist, main='속도와 제동거리', 
     xlab='속도(mph)',ylab='제동거리(ft)', col='red')

# Nile 시계열그래프와 선점도 그래프
Nile
str(Nile)
head(Nile)

plot(Nile, main='Nile강의 연도별 유량 변화',
     xlab='연도', ylab='유량')

plot(Nile, main='Nile강의 연도별 유량 변화', type='p',
     xlab='연도', ylab='유량')

str(Nile)
class(Nile)

# Nile 데이터 프레임으로 만들어서 ggplot으로 그래프 그리기(1)
df_Nile01 <- as.data.frame(Nile)
year <- 1871:1970
df_Nile01$year <- year
df_Nile01
ggplot(df_Nile01, aes(x=year, y= x)) +
  geom_line() +
  xlab('연도') +
  ylab('유량')

# Nile 데이터 프레임으로 만들어서 ggplot으로 그래프 그리기(2)
df_Nile02 <- data.frame(year=time(Nile),
                      y= as.matrix(Nile))
head(df_Nile02)
ggplot(df_Nile02, aes(x=year, y= y)) +
  geom_line() +
  ggtitle('Nile강의 연도별 유량 변화') +
  xlab('연도') +
  theme(axis.text.x = element_text(angle=90, hjust=1, size=5)) +
  ylab('유량')


## 예제 2-2. 막대그래프와 히스토그램
load('data.rda')
tableV5 <- table(data$V5)
tableV5

# 막대그래프 (plot)
barplot(tableV5,
        main = '출생아(남자)별 빈도', xlab='출생아수', ylab='빈도')

tableV1.V4 <- table(data$V1, data$V4)
class(tableV1.V4)

barplot(tableV1.V4, legend.text = T, col=c('orange', 'green'),
        main='학력에 따른 성별 인원수', xlab='학력',ylab='빈도')

# 막대그래프 (ggplot)
tableV5
class(tableV5)
df_tableV5 <- data.frame(tableV5)
df_tableV5
ggplot(df_tableV5, aes(x= Var1, y= Freq)) +
  geom_bar(stat = 'identity')

# 히스토그램
par(mfrow=c(1,1))
hist(data$V2, main='연령별 분포', xlab='연령', ylab='빈도')

hist(data$V2, breaks=c(seq(0,90,10)), right=F,
     main='연령별 분포', xlab='연령', ylab='빈도')


## 예제 2-3. 원 도표
load('data.rda')
table.V4 <- table(data$V4)
table.V4
pie(table.V4, main='학력수준별 비중')


