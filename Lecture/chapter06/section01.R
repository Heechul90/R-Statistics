### section 01. 가설검정

setwd('D:/Heechul/R_Statistics/Lecture/chapter06')

## 예제 chapter 05. 예제 5-7. R에서 불러오기와 추출하기
data <- read.csv('data/2016.6th.csv', header = T)
str(data)

tmp <- subset(data, data$나이==7)
height.p <- tmp$X104.키

set.seed(9)
height <- height.p[sample(length(height.p), 15)]
height



mean(height)
sd(height)
t.test(height)
t.test(height, mu=1220)
