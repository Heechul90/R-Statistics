### section 02. 구간추정

setwd('D:/Heechul/R_Statistics/Lecture/chapter05')

## 예제 5-4. 모평균에 대한 95% 신뢰구간
set.seed(9)
n <- 10
x <- 1:100
y <- seq(-3, 3, by = 0.01)

smps <- matrix(rnorm(n * length(x)), ncol = n)
smps

xbar <- apply(smps, 1, mean)
se <- 1 / sqrt(10)
alpha <- 0.05
z <- qnorm(1 - alpha/2)
ll <- xbar - z*se
ul <- xbar + z*se

par(mfrow=c(1,1))
plot(y, type='n', xlim=c(1,100), ylim=c(-1.5, 1.5), cex.lab=1.8,
     main='95% Confidence Interval for Population mean',
     xlab='trial',
     ylab='z')
abline(h=0, col='red', lty=2)
l.c <- rep(NA, length(x))
l.c <- ifelse(ll * ul >0, 'red', 'black')
arrows(1:length(x), ll, 1:length(x), ul, code=3, angle=90, length=0.02, col=l.c, lwd=1.5)



## 예제 5-5. 모평균에 대한 95% 신뢰구간(모분산을 모를 때)
ci.t <- function(x, alpha=0.05) {
  n <- length(smp)
  m <- mean(x)
  s <- sd(x)
  t <- qt(1-(alpha/2), df = n-1)
  ll <- m-t * (s/sqrt(n))
  ul <- m+t * (s/sqrt(n))
  ci <- c(1-alpha, ll, m, ul)
  names(ci) <- c('Contidence Level', 'Lower limit', 'Mean', 'Upper limit')
  return(ci)
}

smp <- c(520, 498, 481, 512, 515, 542, 520, 518, 527, 526)
ci.t(smp)
ci.t(smp, 0.1)
