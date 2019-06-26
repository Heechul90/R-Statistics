### section01. 표본분포

setwd('D:/Heechul/R_Statistics/Lecture/chapter04')
library(prob)

## 예제 4-1.표본평균 x바 의 분포

m10 <- rep(NA, 1000)
m40 <- rep(NA, 1000)

set.seed(9)
for(i in 1:1000) {
  m10[i] <- mean(rnorm(10))   # rnorm 에서 평균과 표준편차가 없으면 정규 표준분포
  m40[i] <- mean(rnorm(40))
}

options(digits = 4)
c(mean(m10), sd(m10))
c(mean(m40), sd(m40))

par(mfrow=c(2,1))
hist(m10, xlim = c(-1.5, 1.5), main = 'm10', xlab = 'x', ylab = '',
     col = 'red', border = 'black')
hist(m40, xlim = c(-1.5, 1.5), main = 'm40', xlab = 'x', ylab = '',
     col = 'blue' , border = 'black')

par(mfrow=c(1,1))

