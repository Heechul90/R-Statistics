### 연습문제03. chisq, t, f 그래프 그리기

setwd('D:/Heechul/R_Statistics/Practice')
library(prob)
library(extrafont)
windowsFonts(dohyeon=windowsFont("BM DoHyeon"))
windowsFonts(jalnan=windowsFont("Jalnan"))
windowsFonts(binggrae=windowsFont("Binggrae Taom"))
windowsFonts(nbg=windowsFont("나눔바른고딕"))

## χ2 분포 그래프 그리기
# 평균 : k, 분산 : 2k
set.seed(9)
n <- 1000

chisq.2.mean <- rep(NA, n)
chisq.4.mean <- rep(NA, n)
chisq.8.mean <- rep(NA, n)
chisq.16.mean <- rep(NA, n)

rchisq(2, df=1)

for(i in 1:n) {
   chisq.2.mean[i] <- rchisq(2, df=1)
   chisq.4.mean[i] <- rchisq(4, df=1)
   chisq.8.mean[i] <- rchisq(8, df=1)
   chisq.16.mean[i] <- rchisq(16, df=1)
}

options(digits = 4)
c(mean(chisq.2.mean), sd(chisq.2.mean))
c(mean(chisq.4.mean), sd(chisq.4.mean))
c(mean(chisq.8.mean), sd(chisq.8.mean))
c(mean(chisq.16.mean), sd(chisq.16.mean))


par(mfrow=c(2,2))
hist(chisq.2.mean, probability = T,
     main='Chisq',
     col = 'red')
chisq_x <- seq(min(chisq.2.mean), max(chisq.2.mean), length.out = 1000)
chisq_y <- dnorm(x=chisq_x, mean = 1, sqrt(2/2))
lines(chisq_x, chisq_y, lty=2, lwd=2, col='black')

hist(chisq.16.mean, probability = T,
     main='Chisq',
     col = 'red')
chisq_x <- seq(min(chisq.4.mean), max(chisq.4.mean), length.out = 1000)
chisq_y <- dnorm(x=chisq_x, mean = 1, sqrt(2/2))
lines(chisq_x, chisq_y, lty=2, lwd=2, col='black')