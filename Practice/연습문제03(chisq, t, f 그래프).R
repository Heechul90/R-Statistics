### 연습문제03. chisq, t, f 그래프 그리기

setwd('D:/Heechul/R_Statistics/Practice')
library(prob)

## χ2 분포 그래프 그리기
options(digits = 4)

n <- 1000
x <- seq(0, 30, length.out = n)
k <- 1
y <- dchisq(x,k)
y4 <- dchisq(x,4)
y8 <- dchisq(x,8)
y16 <- dchisq(x,16)

par(mfrow=c(2,2))
plot(x, y, type='l', col = 'red', lty=1, lwd=2, xlim=c(0,30), ylim=c(0,0.2),
     main ='자유도 : 1', xlab= '', ylab ='χ2 분포')

plot(x, y4, type='l', col = 'black', lty=1, lwd=2, xlim=c(0,30), ylim=c(0,0.2),
     main ='자유도 : 4', xlab= '', ylab ='χ2 분포')
 
plot(x, y8, type='l', col = 'green', lty=1, lwd=2, xlim=c(0,30), ylim=c(0,0.2),
     main ='자유도 : 8', xlab= '', ylab ='χ2 분포')

plot(x, y16, type='l', col = 'blue', lty=1, lwd=2, xlim=c(0,30), ylim=c(0,0.2),
     main ='자유도 : 16', xlab= '', ylab ='χ2 분포')



## t 분포 그래프 그리기

n <- 1000
t <- seq(-5, 5, length.out = n)
df <- 1
y <- dt(t, df)
y2 <- dt(t, 2)
y4 <- dt(t, 4)
y8 <- dt(t, 8)
y32 <- dt(t, 32)



par(mfrow=c(2,2))
plot(t, y, type='l', col = 'red', lty=1, lwd=2, xlim=c(-5, 5), ylim=c(0,0.4),
     main ='df : 1', xlab= '확률변수 Z', ylab ='')

plot(t, y2, type='l', col = 'black', lty=1, lwd=2, xlim=c(-5, 5), ylim=c(0,0.4),
     main ='df : 2', xlab= '확률변수 Z', ylab ='')

plot(t, y8, type='l', col = 'green', lty=1, lwd=2, xlim=c(-5, 5), ylim=c(0,0.4),
     main ='df : 8', xlab= '확률변수 Z', ylab ='')

plot(t, y32, type='l', col = 'blue', lty=1, lwd=2, xlim=c(-5, 5), ylim=c(0,0.4),
     main ='df : 32', xlab= '확률변수 Z', ylab ='')



## f 분포 그래프 그리기
n <- 1000
x <- seq(0, 5, length.out = n)
y1 <- df(x, 5, 50)
y2 <- df(x, 5, 500)
y3 <- df(x, 50, 50)
y4 <- df(x, 50, 500)


par(mfrow=c(2,2))
plot(x, y1, type='l', col = 'red', lty=1, lwd=2, xlim=c(0,2), ylim=c(0,2),
     main ='df : 5, 50', xlab= '확률변수 Z', ylab ='')

plot(x, y2, type='l', col = 'black', lty=1, lwd=2, xlim=c(0,2), ylim=c(0,2),
     main ='df : 5,500', xlab= '확률변수 Z', ylab ='')
      
plot(x, y3, type='l', col = 'green', lty=1, lwd=2, xlim=c(0,2), ylim=c(0,2),
     main ='df : 50, 50', xlab= '확률변수 Z', ylab ='')

plot(x, y4, type='l', col = 'blue', lty=1, lwd=2, xlim=c(0,2), ylim=c(0,2.),
     main ='df : 50, 500', xlab= '확률변수 Z', ylab ='')
