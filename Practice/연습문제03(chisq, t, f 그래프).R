### 연습문제03. chisq, t, f 그래프 그리기

setwd('D:/Heechul/R_Statistics/Practice')
library(prob)


## 1. χ2 표본수에 따른 정규분포 그래프 변화

set.seed(9)
n <- 1000

chisq.1.mean <- rep(NA, n)
chisq.2.mean <- rep(NA, n)
chisq.4.mean <- rep(NA, n)
chisq.8.mean <- rep(NA, n)
chisq.16.mean <- rep(NA, n)
chisq.32.mean <- rep(NA, n)
chisq.128.mean <- rep(NA, n)
chisq.256.mean <- rep(NA, n)

for(i in 1:n) {
   chisq.1.mean[i] <- rchisq(1, df=10)
   chisq.2.mean[i] <- rchisq(2, df=10)
   chisq.4.mean[i] <- rchisq(4, df=10)
   chisq.8.mean[i] <- rchisq(8, df=10)
   chisq.16.mean[i] <- rchisq(16, df=10)
   chisq.32.mean[i] <- rchisq(32, df=10)
   chisq.128.mean[i] <- rchisq(128, df=10)
   chisq.256.mean[i] <- rchisq(256, df=10)
}

options(digits = 4)
c(mean(chisq.1.mean), sd(chisq.1.mean))
c(mean(chisq.2.mean), sd(chisq.2.mean))
c(mean(chisq.4.mean), sd(chisq.4.mean))
c(mean(chisq.8.mean), sd(chisq.8.mean))
c(mean(chisq.16.mean), sd(chisq.16.mean))
c(mean(chisq.32.mean), sd(chisq.32.mean))


par(mfrow=c(2,2))

hist(chisq.4.mean, probability = T,
     main='표본크기=4',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
chisq_x <- seq(min(chisq.4.mean), max(chisq.4.mean), length.out = 1000)
chisq_y <- dnorm(x=chisq_x, mean = 10, sd = 20/sqrt(4))
lines(chisq_x, chisq_y, lty=2, lwd=2, col='blue')

hist(chisq.16.mean, probability = T,
     main='표본크기=16',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
chisq_x <- seq(min(chisq.16.mean), max(chisq.16.mean), length.out = 1000)
chisq_y <- dnorm(x=chisq_x, mean = 10, sd = 20/sqrt(16))
lines(chisq_x, chisq_y, lty=2, lwd=2, col='blue')

hist(chisq.32.mean, probability = T,
     main='표본크기=32',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
chisq_x <- seq(min(chisq.32.mean), max(chisq.32.mean), length.out = 1000)
chisq_y <- dnorm(x=chisq_x, mean = 10, sd = 20/sqrt(32))
lines(chisq_x, chisq_y, lty=2, lwd=2, col='blue')

hist(chisq.128.mean, probability = T,
     main='표본크기=128',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
chisq_x <- seq(min(chisq.128.mean), max(chisq.128.mean), length.out = 1000)
chisq_y <- dnorm(x=chisq_x, mean = 10, sd = 20/sqrt(128))
lines(chisq_x, chisq_y, lty=2, lwd=2, col='blue')

mtext("표본 변화에 따른 χ2분포, 자유도=10", side = 3, line=1, outer=T)



## 2. t-분포. 표본수에 따른 정규분포 그래프 변화
set.seed(9)
n <- 1000

t.1.mean <- rep(NA, n)
t.2.mean <- rep(NA, n)
t.4.mean <- rep(NA, n)
t.8.mean <- rep(NA, n)
t.16.mean <- rep(NA, n)
t.32.mean <- rep(NA, n)
t.64.mean <- rep(NA, n)
t.128.mean <- rep(NA, n)
t.256.mean <- rep(NA, n)
t.512.mean <- rep(NA, n)


for(i in 1:n) {
   t.1.mean[i] <- rt(1,df=10)
   t.2.mean[i] <- rt(2,df=10)
   t.4.mean[i] <- rt(4,df=10)
   t.8.mean[i] <- rt(8,df=10)
   t.16.mean[i] <- rt(16,df=10)
   t.32.mean[i] <- rt(32,df=10)
   t.64.mean[i] <- rt(64,df=10)
   t.128.mean[i] <- rt(128,df=10)
   t.256.mean[i] <- rt(256,df=10)
   t.512.mean[i] <- rt(512,df=10)
}

options(digits = 4)
c(mean(t.1.mean), sd(t.1.mean))
c(mean(t.2.mean), sd(t.2.mean))
c(mean(t.4.mean), sd(t.4.mean))
c(mean(t.8.mean), sd(t.8.mean))
c(mean(t.16.mean), sd(t.16.mean))
c(mean(t.32.mean), sd(t.32.mean))
c(mean(t.64.mean), sd(t.64.mean))
c(mean(t.128.mean), sd(t.128.mean))
c(mean(t.256.mean), sd(t.256.mean))
c(mean(t.512.mean), sd(t.512.mean))

