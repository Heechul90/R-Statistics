### section 02. 모집단이 세 개 이상

setwd('D:/Heechul/R_Statistics/Lecture/chapter07(여러 모집단의 평균 비교 검정)')

## 필요 패키지
library(prob)

## 예제
ad <- read.csv('data/age.data.csv', header = T)
ad
head(ad)
str(ad)

## 분석을 위한 통계량 계산과 오차제곱합 구하기
# 각 처리별로 나이를 저장
y1 <- ad$age[ad$scale == '1']
y1
y2 <- ad$age[ad$scale == '2']
y2
y3 <- ad$age[ad$scale == '3']
y3

# 각 처리별로 평균을 저장
y1.mean <- mean(y1)
y1.mean
y2.mean <- mean(y2)
y2.mean
y3.mean <- mean(y3)
y3.mean

# 각 처리별 편차제곱합을 구함
sse.1 <- sum((y1-y1.mean)^2)
sse.1
sse.2 <- sum((y2-y2.mean)^2)
sse.2
sse.3 <- sum((y3-y3.mean)^2)
sse.3

# 오차제곱과 자유도를 구함
sse <- sse.1 + sse.2 + sse.3
sse
dfe <- (length(y1)-1) + (length(y2)-1) + (length(y3)-1)
dfe


## 처리제곱합 구하기
# 전체 평균을 구함
y <- mean(ad$age)

# 처리제곱합 계산을 위해 각 처리별로 전체 평균과의 편차제곱합을 구함
sst.1 <- length(y1) * sum((y1.mean-y)^2)
sst.1
sst.2 <- length(y2) * sum((y2.mean-y)^2)
sst.2
sst.3 <- length(y3) * sum((y3.mean-y)^2)
sst.3

# 처리제곱합과 자유도를 구함
sst <- sst.1 + sst.2 + sst.3
sst
dft <- length(levels(ad$scale))-1   ## 다시확인
dft <- 2
dft

# 제곱합의 비교
tsq <- sum((ad$age - y)^2)
tsq
ss <- sst+sse
ss

#검정통계량 구하기
mst <- sst /dft
mst
mse <- sse / dfe
mse
f.t <- mst / mse
f.t

# 기각역을 위한 임계값 구하기
alpha <- 0.05
tol <- qf(1-alpha, df1=2, df2=147)
tol

# 유의확률 구하기
p.value <- 1-pf(f.t, df1=2, df2=147)
p.value

# 그래프

# lm() 함수와 anova() 함수를 사용한 분석
ow <- lm(age~scale, data=ad)
anova(ow)   ## 책과 다르게 나옴. 확인할 것.
