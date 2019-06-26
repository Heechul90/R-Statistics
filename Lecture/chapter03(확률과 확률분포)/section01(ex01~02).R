### section01 확률

setwd('D:/Heechul/R_Statistics/Lecture/chapter03')
install.packages('prob')
library(prob)
library(ggplot2)

## 예제3-1. prob 패키지를 이용한 확률 계산
tosscoin(1)
rolldie(1)
urnsamples(1:3, size = 2)
urnsamples(1:3, size = 3,replace=T)
urnsamples(c(rep('R', 3), rep('B', 2)), size = 2)
tosscoin(2, makespace = T)


## 예제3-2. 확률변수의 평균(기댓값)과 분산
x <- c(0:2)
px <- c(1/4, 2/4, 1/4)
EX <- sum(x*px)
EX
x2 <- x^2
x2
EX2 <- sum(x2 * px)     # x 제곱의 기댓값
VARX <- EX2 - EX^2
VARX


