### section 01. 상관계수

setwd('D:/Heechul/R_Statistics/Lecture/chapter09(상관과 회귀)')

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
