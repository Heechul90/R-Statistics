### section 01. 상관계수

setwd('D:/Heechul/R_Statistics/Lecture/chapter09(상관과 회귀)')

# 그림 9-1
set.seed(9)
rvnorm <- function(r) {
  x <- rnorm(50, 0, 1)
  y <- rnorm(50, r*x, sqrt(1-r^2))
  return(cbind(x,y))
}

par(mfrow=c(1, 3), mar=c(2, 2, 2, 1), oma=c(0,0,0,0))

r1 <- rvnorm(0.8)
plot(r1, main="r=0.8")
abline(lm(r1[,2] ~ r1[,1]), col="red")

plot(rvnorm(0), main="r=0")
abline(h=0, col="red")

r3 <- rvnorm(-0.8)
plot(r3, main="r=-0.8")
abline(lm(r3[,2] ~ r3[,1]), col="red")

par(mfrow=c(1, 1))

## 예제 9-1. 아버지와 아들 키의 공분산과 상관계수
hf <- read.table('http://www.randomservices.org/random/data/Galton.txt',
                 header = T, stringsAsFactors = FALSE)
hf
str(hf)
hf$Gender <- factor(hf$Gender, levels = c('M', 'F'))
hf.son <- subset(hf, Gender == 'M')
hf.son <- hf.son[c('Father', 'Height')]
str(hf.son)

# 아버지 키 평균, 아들 키 평균
f.mean <- mean(hf.son$Father)
f.mean
s.mean <- mean(hf.son$Height)
s.mean

# 공분산 계산하기
cov.num <- sum((hf.son$Father-f.mean) * (hf.son$Height-s.mean))
cov.num

# 표본공분산 계산하기
cov.xy <- cov.num / (nrow(hf.son) - 1)
cov.xy
# R로 표본공분산 계산하기
cov(hf.son$Father, hf.son$Height)

# 상관계수 게산하기
r.xy <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height))
r.xy

# R로 상관계수 계산하기
cor(hf.son$Father, hf.son$Height)
