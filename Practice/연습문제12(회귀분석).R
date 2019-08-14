setwd('D:/Heechul/R_Statistics/Practice')


library(prob)
library(dplyr)


### 연습문제12. 회귀분석
## 문제 01. 부모의 키가 클수록 자식의 키도 상대적으로 크다고 하는데,
##          회귀식을 구하고 아버지의 키가 165cm일 때
##          아들의 키는 얼마인지 예측하시오.
x <- c(150, 160, 170, 180, 190)
y <- c(176, 179, 182, 181, 185)
height <- data.frame('x'=x, 'y'=y)
height

fit <- lm(y ~ x, data = height)
summary(fit)

# 회귀식 
# yhat <- 146.60000 + 0.2*x1


# 아버지의 키가 165cm일 때 아들의 키
x1 <- 165
yhat <- 146.60000 + 0.2*x1
yhat

## 문제 02. 소득이 높을수록 신용카드 사용량이 많아진다고 하는데,
##          회귀식을 구하고
##          월 소득이 250만원일 때 신용카드 사용량을 예측하시오
x <- c(100, 200, 300, 400, 500)
y <- c(30, 70, 85, 140, 197)
credit <- data.frame('x' = x, 'y' = y)
credit

fit <- lm(y ~ x, data = credit)
summary(fit)

# 회귀식
# yhat <- -16.8 + 0.404*x1

# 월 소득이 250만원일 때 신용카드 사용량
x1 <- 250
yhat <- -16.8 + 0.404*x1
yhat


## 문제 03. mtcars 데이터셋에서 배기량(disp)에 따른 마력(hp)의 회귀식을 구하시오.
mtcars
head(mtcars)
str(mtcars)
disp <- mtcars %>%
  select(disp, hp)
disp
str(disp)

fit <- lm(hp ~ disp, data = mtcars)
summary(fit)

# 회귀식
# yhat <- 45.7345 + 0.4375*x1


## 문제 04. MASS 패키지를 설치하고, 
##          이 패키지 안에 있는 Boston 데이터셋을 이용하여 
##          Boston 인근의 집값을 결정하는 다중회귀 모델을 만드시오.

# CRIM: 자치시(town) 별 1인당 범죄율
# ZN: 25,000 평방피트를 초과하는 거주지역의 비율
# INDUS: 비소매상업지역이 점유하고 있는 토지의 비율
# CHAS: 찰스강에 대한 더미변수(강의 경계에 위치한 경우는 1, 아니면 0)
# NOX: 10ppm 당 농축 일산화질소
# RM: 주택 1가구당 평균 방의 개수
# AGE: 1940년 이전에 건축된 소유주택의 비율
# DIS: 5개의 보스턴 직업센터까지의 접근성 지수
# RAD: 방사형 도로까지의 접근성 지수
# TAX: 10,000 달러 당 재산세율
# PTRATIO: 자치시(town)별 학생/교사 비율
# B: 1000(Bk-0.63)^2, 여기서 Bk는 자치시별 흑인의 비율을 말함.
# LSTAT: 모집단의 하위계층의 비율(%)
# MEDV: 본인 소유의 주택가격(중앙값) (단위: $1,000)

library(MASS)
head(Boston)
str(Boston)

# 산점도 그려보기
pairs(Boston)
# 해석: medv와 상관관계를 봤을때 어느정도 상관이 있는것으로 보여짐

## 1. Linear Regression Model
fit1 <- lm(medv~., data = Boston)
summary(fit1)
anova(fit1)
# 해석: nox랑 rad는 별표시가 없어 medv에 큰 영향력이 없어 보인다

# 1) backward elimination (하나씩 제거해 나가는 방법)
step(fit1, direction = "backward")
# 해석: age와 indus가 영향력이 없는 변수로 판단되어 제거

# 2) forward selection (하나씩 추가하면서 판단)
fit2 <- lm(medv ~1, data = Boston)
step(fit2,direction = "forward", scope = ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat)
# 해석: lstat + rm + ptratio + dis + nox + chas + black + zn + crim + rad + tax 이 선택됨

# 3) stepwise regression (모든경우를 해서 판단)
fit3 <- lm(medv~., data = Boston)
step(fit3 , direction = "both")
# 해석: lstat + rm + ptratio + dis + nox + chas + black + zn + crim + rad + tax 이 선택됨

# 4) All possible regression (모든 가능한 회귀모형을 구축하고 선택)
library(leaps)
subsets1 <- regsubsets(medv~., data = Boston , method = 'seqrep' , nbest = 13) 
plot(subsets1)

subsets2 <- regsubsets(medv~., data = Boston , method = 'exhaustive' , nbest = 13) 
plot(subsets2)

## 결론: backward elimination, forward selection,stepwise regression 경우에  age와 indus가 제외됨
##       age와 indus 를 제외하고 fit4 로 지정
fit4 <- lm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat , data = Boston)
summary(fit4)

# 결론: All possible regression 경우에  crime, indus, age, rad, tax 가 제외됨
##      crime, indus, age, rad, tax 제외하고 fit5로 지정

fit5 <- lm(medv ~ zn + chas + nox + rm + dis + ptratio + black + lstat, data = Boston)
summary(fit5)


### 정규성, 등분산성, 성형성, 독립성 검토

## fit4의 정규성, 등분산성, 선형성, 독립성 검토
par(mfrow = c(2, 2))
plot(fit4)
par(mfrow = c(1, 1))

# 1) 정규성
qqnorm(fit4$residuals) ; qqline(fit4$residuals)
shapiro.test(fit4$residuals)
# p-value값이 유의수준 0.05 보다 작으므로 정규성을 따르지 않는다.

# 2) 선형성, 등분산성
library(gvlma)
gvmodel2 <- gvlma(fit4)
summary(gvmodel2)
# p-value값이 유의수준 0.05 보다 작으므로 선형성, 등분산성을 따르지 않는다.

# 3) 독립성
library(car)
durbinWatsonTest(fit4)
# p-value값이 유의수준 0.05 보다 작으므로 독립성을 따르지 않는다.

## fit5의 정규성, 등분산성, 선형성, 독립성 검토
par(mfrow = c(2, 2))
plot(fit5)
par(mfrow = c(1, 1))

# 1) 정규성
qqnorm(fit4$residuals) ; qqline(fit4$residuals)
shapiro.test(fit4$residuals)
# p-value값이 유의수준 0.05 보다 작으므로 정규성을 따르지 않는다.

# 2) 선형성, 등분산성
library(gvlma)
gvmodel2 <- gvlma(fit4)
summary(gvmodel2)
# p-value값이 유의수준 0.05 보다 작으므로 선형성, 등분산성을 따르지 않는다.

# 3) 독립성
library(car)
durbinWatsonTest(fit4)
# p-value값이 유의수준 0.05 보다 작으므로 독립성을 따르지 않는다.

## 결론: fit4와 fit5 둘다 정규성, 등분산성, 선형성, 독립성을
##       만족하지 못한다
##       그러므로 다중회귀식을 구할 수 없다