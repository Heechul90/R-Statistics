setwd('D:/Heechul/R_Statistics/Practice')

install.packages('prob')
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
yhat <- 146.60000 + 0.2*x1


# 아버지의 키가 165cm일 때 아들의 키
x1 <- 165
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
yhat <- -16.8 + 0.404*x1

# 월 소득이 250만원일 때 신용카드 사용량
x1 <- 250
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
yhat <- 45.7345 + 0.4375*x1


## 문제 03. 
