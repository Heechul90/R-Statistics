### section02 분포함수

setwd('D:/Heechul/R_Statistics/Lecture/chapter03')
library(prob)

## 예제 3-3. R을 이용한 이항분포 계산
n <-6
p <- 1/3
x <- 0:n

# d = 어떤 값일때 확률, p = 누적확률, q = 확률 -> x, r = 난수 생성
# dbinom() 어떤 값일때 확률
dbinom(2, size = n, prob = p)     # x가 2일 때 확률
dbinom(4, size = n, prob = p)     # x가 4일 때 확률
px <- dbinom(x, size = n, prob = p)
px    # 1일때, 2일때, 3일때, 4일때, 5일때, 6일때

plot(x, px, type= 's', xlab= '성공횟수(x)', ylab='확률(p[X=x]', main='B(6, 1/3)')
plot(x, px, type= 'h', xlab= '성공횟수(x)', ylab='확률(p[X=x]', main='B(6, 1/3)',
     col = 'red', lwd=10)

# pbinom() 누적확률
pbinom(2, size = n, prob = p)     # 0, 1, 2가 일어날 확률을 더한 값
pbinom(4, size = n, prob = p)     # 0, 1, 2, 3, 4가 일어날 확률을 더한 값
pbinom(6, size = n, prob = p)     # 0, 1, 2, 3, 4, 5, 6가 일어날 확률을 더한 값


# 3번, 4번 그래프와 같다
pbinom(4, size = n, prob = p) - pbinom(2, size = n, prob = p)
dbinom(3, size = n, prob = p) + dbinom(4, size = n, prob = p)

# qbinom() 누적확률이 !일때 확률
qbinom(0.1, size = n, prob = p)     # 누적확률이이 0.1 일때 x값
qbinom(0.5, size = n, prob = p)     # 누적확률이이 0.1 일때 x값 
qbinom(0.9821674, size = n, prob = p)

# rbinom() 랜덤으로 나옴
rbinom(10, size = n, prob = p)


## 예제3-4. R의 분포함수를 이용한 기댓값과 분산
n <-6
p <- 1/3
x <- 0:n
px <- dbinom(x, size = n, prob = p)
ex <- sum(x*px)       # Expectation value
ex2 <- sum(x^2*px)
varx <- ex2 -ex^2     # Variance
varx

n*p                   # 이항분포의 기댓값 = np
n*p*(1-p)             # 이항분포의 분산 = np(1-p)


## 예제 3-5. R을 이용한 정규분포 계산
options(digits = 3)
mu <- 170
sigma <- 6
ll <- mu - 3*sigma     # lower list
ul <- mu + 3*sigma     # upper list

x <- seq(ll, ul, by=0.01)
x
nd <- dnorm(x, mean = mu, sd= sigma)
nd
plot(x, nd, type='l', xlab='x', ylab='P(X=x)', lwd=2, col='red',
     main= '평균 170, 표준편차 6인 정규분포곡선')

pnorm(mu, mean = mu, sd= sigma)
pnorm(158, mean = mu, sd= sigma)

# 키가 160이상이고 180이하일 사람들의 분포가 0.904, 나머지는 작거나 크다
pnorm(180, mean = mu, sd= sigma) - pnorm(160, mean = mu, sd= sigma)

# 면적이 0.25가 되는 때의 x 값이 166이다. 
qnorm(0.25, mean = mu, sd= sigma)    # 1분위수
qnorm(0.75, mean = mu, sd= sigma)    # 3분위수
qnorm(0.5, mean = mu, sd= sigma)

options(digits = 5)
set.seed(5)
smp <- rnorm(400, mean = mu, sd= sigma)
c(mean(smp), sd(smp))
hist(smp, prob= T,
     main='N(170, 6^2)으로부터 추출한 표본의 분포(n=400)',
     xlab='', ylab = '', col = 'white', border = 'black')
lines(x, nd, lty=1, col= 'red')



## 예제3-6. R을 이용해 정규분포의 특징 알아보기
options(digits = 4)
mu <- 0
sigma <- 1

p0.05 <- qnorm(0.05, mean = mu, sd = sigma)
p0.025 <- qnorm(0.025, mean = mu, sd = sigma)
p0.05; p0.025

pnorm(1.645) - pnorm(-1,645)
pnorm(1.96) - pnorm(-1.96)


# 그림 3-17
z <- seq(-3, 3, by=0.001)
z.p <- dnorm(z)
plot(z, z.p, axes=F, type='l',
     main='표준정규분포(95%)', ylab='', ylim=c(-0.04, 0.4))
axis(1)

lines(c(-3, 3), c(0, 0))
points(-1.96, -0.02, pch=17, col='red')

