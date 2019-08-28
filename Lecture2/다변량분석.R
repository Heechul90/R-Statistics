# 다변량분석석
setwd('D:/Heechul/R_Statistics/Lecture2')
getwd()
crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
head(crime)
rownames(crime) = crime[, 1]
rownames(crime)

stars(crime[, 2:8])
stars(crime[, 2:8], flip.labels = FALSE)
stars(crime[, 2:8], flip.labels = FALSE, key.loc = c(1,20))
stars(crime[, 2:8], flip.labels = FALSE, key.loc = c(1,20),draw.segments = TRUE)

install.packages("aplpack")
library(aplpack)
faces(crime[, 2:8])
education = read.csv("http://datasets.flowingdata.com/education.csv")
head(education)
str(education)
library(lattice)
parallel(education[, 2:7], horizontal.axis = FALSE, col = 1)
summary(education$reading)
color = education$reading > 523
color
color+1
parallel(education[, 2:7], horizontal.axis = FALSE, col = color + 1)
summary(education$math)
color = education$dropout_rate > 5.3
parallel(education[, 2:7], horizontal.axis = FALSE, col = color + 1)
summary(education$dropout_rate)
color = education$math > 525.5

parallel(education[, 2:7], horizontal.axis = FALSE, col = color + 1)
data = read.csv("20140528_baseball.csv")
head(data)

model = prcomp(data[, 2:6], scale = T)
summary(model)
plot(model)
biplot(model)
rownames(data)=data[,1]
data
model=prcomp(data[,2:6],scale=T)
biplot(model)


###
install.packages("foreign")  # foreign 패키지 설치

library(foreign)             # SPSS 파일 로드
library(dplyr)               # 전처리
library(ggplot2)             # 시각화
library(readxl)              # 엑셀 파일 불러오기

# 데이터 불러오기
raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
install.packages("plotly")
library(plotly)
mpg=as.data.frame(ggplot2::mpg)

tt=ggplot(mpg,aes(x=drv,y=hwy))+geom_bar(stat='identity',fill="red")
ggplotly(tt)
geom_text(aes(label=max(mpg$hwy),vjust=-0.2),na.rm=T,col=2)
