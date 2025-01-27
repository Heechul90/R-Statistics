### 연습문제03. chisq, t, f 그래프 그리기

setwd('D:/Heechul/R_Statistics/Practice')
library(prob)


## 1. χ2 표본수에 따른 정규분포 그래프 변화
# E(X) = k, Var(X) = 2k
set.seed(9)
n <- 1000
df <- 10
ex <- df
varx <- 2*df

chisq.1.mean <- rep(NA, n)
chisq.2.mean <- rep(NA, n)
chisq.4.mean <- rep(NA, n)
chisq.8.mean <- rep(NA, n)
chisq.16.mean <- rep(NA, n)
chisq.32.mean <- rep(NA, n)
chisq.128.mean <- rep(NA, n)
chisq.256.mean <- rep(NA, n)

for(i in 1:n) {
   chisq.1.mean[i] <- mean(rchisq(1, df=df))
   chisq.2.mean[i] <- mean(rchisq(2, df=df))
   chisq.4.mean[i] <- mean(rchisq(4, df=df))
   chisq.8.mean[i] <- mean(rchisq(8, df=df))
   chisq.16.mean[i] <- mean(rchisq(16, df=df))
   chisq.32.mean[i] <- mean(rchisq(32, df=df))
   chisq.128.mean[i] <- mean(rchisq(128, df=df))
   chisq.256.mean[i] <- mean(rchisq(256, df=df))
}

options(digits = 4)
c(mean(chisq.1.mean), sd(chisq.1.mean))
c(mean(chisq.2.mean), sd(chisq.2.mean))
c(mean(chisq.4.mean), sd(chisq.4.mean))
c(mean(chisq.8.mean), sd(chisq.8.mean))
c(mean(chisq.16.mean), sd(chisq.16.mean))
c(mean(chisq.32.mean), sd(chisq.32.mean))
c(mean(chisq.128.mean), sd(chisq.128.mean))
c(mean(chisq.256.mean), sd(chisq.256.mean))

par(mfrow=c(2,2))

hist(chisq.4.mean, probability = T, xlim = c(4, 15), ylim = c(0, 0.8),
     main='표본크기=4',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
chisq_x <- seq(min(chisq.4.mean), max(chisq.4.mean), length.out = 1000)
chisq_y <- dnorm(x=chisq_x, mean = ex, sd = sqrt(varx/4))
lines(chisq_x, chisq_y, lty=2, lwd=2, col='blue')

hist(chisq.16.mean, probability = T,xlim = c(4, 15), ylim = c(0, 0.8),
     main='표본크기=16',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
chisq_x <- seq(min(chisq.16.mean), max(chisq.16.mean), length.out = 1000)
chisq_y <- dnorm(x=chisq_x, mean = ex, sd = sqrt(varx/16))
lines(chisq_x, chisq_y, lty=2, lwd=2, col='blue')

hist(chisq.32.mean, probability = T,xlim = c(4, 15), ylim = c(0, 0.8),
     main='표본크기=32',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
chisq_x <- seq(min(chisq.32.mean), max(chisq.32.mean), length.out = 1000)
chisq_y <- dnorm(x=chisq_x, mean = ex, sd = sqrt(varx/32))
lines(chisq_x, chisq_y, lty=2, lwd=2, col='blue')

hist(chisq.128.mean, probability = T,xlim = c(4, 15), ylim = c(0, 0.8),
     main='표본크기=128',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
chisq_x <- seq(min(chisq.128.mean), max(chisq.128.mean), length.out = 1000)
chisq_y <- dnorm(x=chisq_x, mean = ex, sd = sqrt(varx/128))
lines(chisq_x, chisq_y, lty=2, lwd=2, col='blue')

mtext("표본 변화에 따른 χ2분포, 자유도=10", side = 3, line=1, outer=T)



## 2. t-분포. 표본수에 따른 정규분포 그래프 변화
# E(X) = 0, Var(X) = k/(k-2)
set.seed(9)
n <- 1000
df <- 10
ex <- 0
varx <- df/(df-2)

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
   t.1.mean[i] <- mean(rt(1,df=df))
   t.2.mean[i] <- mean(rt(2,df=df))
   t.4.mean[i] <- mean(rt(4,df=df))
   t.8.mean[i] <- mean(rt(8,df=df))
   t.16.mean[i] <- mean(rt(16,df=df))
   t.32.mean[i] <- mean(rt(32,df=df))
   t.64.mean[i] <- mean(rt(64,df=df))
   t.128.mean[i] <- mean(rt(128,df=df))
   t.256.mean[i] <- mean(rt(256,df=df))
   t.512.mean[i] <- mean(rt(512,df=df))
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
sqrt(varx/64)

par(mfrow=c(2,2))

hist(t.16.mean, probability = T, xlim = c(-0.6, 0.6), ylim = c(0, 8),
     main='표본크기=16',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
t_x <- seq(min(t.16.mean), max(t.16.mean), length.out = 1000)
t_y <- dnorm(x=t_x, mean = ex, sd = sqrt(varx/16))
lines(t_x, t_y, lty=2, lwd=2, col='blue')

hist(t.32.mean, probability = T, xlim = c(-0.6, 0.6), ylim = c(0, 8),
     main='표본크기=32',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
t_x <- seq(min(t.32.mean), max(t.32.mean), length.out = 1000)
t_y <- dnorm(x=t_x, mean = ex, sd = sqrt(varx/32))
lines(t_x, t_y, lty=2, lwd=2, col='blue')

hist(t.128.mean, probability = T, xlim = c(-0.6, 0.6), ylim = c(0, 8),
     main='표본크기=128',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
t_x <- seq(min(t.128.mean), max(t.128.mean), length.out = 1000)
t_y <- dnorm(x=t_x, mean = ex, sd = sqrt(varx/128))
lines(t_x, t_y, lty=2, lwd=2, col='blue')

hist(t.512.mean, probability = T, xlim = c(-0.6, 0.6), ylim = c(0, 8),
     main='표본크기=512',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
t_x <- seq(min(t.512.mean), max(t.512.mean), length.out = 1000)
t_y <- dnorm(x=t_x, mean = ex, sd = sqrt(varx/512))
lines(t_x, t_y, lty=2, lwd=2, col='blue')

mtext("표본 변화에 따른 t분포, 자유도=10", side = 3, line=1, outer=T)

## 3. f-분포. 표본수에 따른 정규분포 그래프 변화
# E(X) = m / (m-2), Var(X) = 2*(m^2)*(n+m-2) / n*((m-2)^2)*(m-4)
set.seed(9)
n <- 1000
df1 <- 50
df2 <- 50
ex <- df2 / (df2-2)
varx <- (2*(df2^2)*(df1+df2-2)) / (df1*((df2-2)^2)*(df2-4))

f.1.mean <- rep(NA, n)
f.2.mean <- rep(NA, n)
f.4.mean <- rep(NA, n)
f.8.mean <- rep(NA, n)
f.16.mean <- rep(NA, n)
f.32.mean <- rep(NA, n)
f.64.mean <- rep(NA, n)
f.128.mean <- rep(NA, n)
f.256.mean <- rep(NA, n)
f.512.mean <- rep(NA, n)


for(i in 1:n) {
   f.1.mean[i] <- mean(rf(1, df1=df1, df2=df2))
   f.2.mean[i] <- mean(rf(2, df1=df1, df2=df2))
   f.4.mean[i] <- mean(rf(4, df1=df1, df2=df2))
   f.8.mean[i] <- mean(rf(8, df1=df1, df2=df2))
   f.16.mean[i] <- mean(rf(16, df1=df1, df2=df2))
   f.32.mean[i] <- mean(rf(32, df1=df1, df2=df2))
   f.64.mean[i] <- mean(rf(64, df1=df1, df2=df2))
   f.128.mean[i] <- mean(rf(128, df1=df1, df2=df2))
   f.256.mean[i] <- mean(rf(256, df1=df1, df2=df2))
   f.512.mean[i] <- mean(rf(512, df1=df1, df2=df2))
}

options(digits = 4)
c(mean(f.1.mean), sd(f.1.mean))
c(mean(f.2.mean), sd(f.2.mean))
c(mean(f.4.mean), sd(f.4.mean))
c(mean(f.8.mean), sd(f.8.mean))
c(mean(f.16.mean), sd(f.16.mean))
c(mean(f.32.mean), sd(f.32.mean))
c(mean(f.64.mean), sd(f.64.mean))
c(mean(f.128.mean), sd(f.128.mean))
c(mean(f.256.mean), sd(f.256.mean))
c(mean(f.512.mean), sd(f.512.mean))

par(mfrow=c(2,2))

hist(f.4.mean, probability = T, xlim = c(0.6, 1.6), ylim = c(0, 8),
     main='표본크기=4',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
f_x <- seq(min(f.4.mean), max(f.4.mean), length.out = 1000)
f_y <- dnorm(x=f_x, mean = ex, sd = sqrt(varx/4))
lines(f_x, f_y, lty=2, lwd=2, col='blue')

hist(f.16.mean, probability = T, xlim = c(0.6, 1.6), ylim = c(0, 8),
     main='표본크기=16',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
f_x <- seq(min(f.16.mean), max(f.16.mean), length.out = 1000)
f_y <- dnorm(x=f_x, mean = ex, sd = sqrt(varx/16))
lines(f_x, f_y, lty=2, lwd=2, col='blue')

hist(f.32.mean, probability = T, xlim = c(0.6, 1.6), ylim = c(0, 8),
     main='표본크기=32',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
f_x <- seq(min(f.64.mean), max(f.64.mean), length.out = 1000)
f_y <- dnorm(x=f_x, mean = ex, sd = sqrt(varx/64))
lines(f_x, f_y, lty=2, lwd=2, col='blue')

hist(f.64.mean, probability = T, xlim = c(0.6, 1.6), ylim = c(0, 8),
     main='표본크기=64',
     xlab = '', ylab = '',
     col = 'orange', border = 'red')
f_x <- seq(min(f.64.mean), max(f.64.mean), length.out = 1000)
f_y <- dnorm(x=f_x, mean = ex, sd = sqrt(varx/64))
lines(f_x, f_y, lty=2, lwd=2, col='blue')

mtext("표본 변화에 따른 f분포, 자유도=(50, 50)", side = 3, line=1, outer=T)
