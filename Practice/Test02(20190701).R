### Test 02.

setwd('D:/Heechul/R_Statistics/Practice')

# 필요 패키지
library(prob)
library(dplyr)
library(ggplot2)
library(stringr)

### 01.
## 문제 01-1. 시행횟수가 6이고 성공확률이 1/3인 이항분포에서 성공횟수가 3이 될 확률?
n <- 6
p <-1/3
dbinom(3, size=n, prob=p)

## 문제 01-2. 평균이 170이고 표준편차가 6인 정규분포에서 상위 20%되는 사람들의 키 범위?
mu <- 170
s <- 6
qnorm(0.8, mean=mu, sd=s) # 175.0497 이상

## 문제 01-3. 자유도가 3인 카이제곱분포에서 누적확률이 95%일 때의 값?
df <- 3
pchisq(0.95, df=df)

## 문제 01-4. 자유도가 2인 t-분포에서 누적확률이 0.975일 대의 값?
df <- 2
qt(0.975, df=df)

## 문제 01-5. 표준정규분포에서 확률번수의 값이 1일 때의 누적확률?
mu <- 0
s <- 1
pnorm(1, mean=mu, sd=s)



### 02. 베르누이 시행인지 판단하시오.
## 2) 전화가 왔을 때, 전화를 한 사람이 여자인지를 측정한다.
##    p = 0.5, n : 무한시도 가능
## 4) 동정의 앞면이 위로 향하고 있는지 체크한다.
##    p = 0.5, n : 무한시도 가능



### 03. 'setosa' 종 Sepal.Length의 모평균에 대한 95% 신뢰구간.
str(iris)
Sepal.L <- iris %>%
  filter(Species == 'setosa') %>%
  select(Sepal.Length)
Sepal.L
# 표본수
n <- length(Sepal.L$Sepal.Length)
n

# 표본평균
xbar <- mean(Sepal.L$Sepal.Length)
xbar

# 표본표준편차
s <- sd(Sepal.L$Sepal.Length)
s

# 최솟값
ll <- xbar - (1.96 * s/sqrt(n))
ll

# 최댓값
ul <- xbar + (1.96 * s/sqrt(n))
ul

### 04. 한 농구선수가 자유투를 던지면 10번중에서 7번을 성공한다.

## 04-1. 이 선수가 자유투를 10번 던져서 9번 이상 성공할 확률?
p <- 0.7
n <- 10
1-pbinom(8, size=n, prob=0.7)

## 04-2. 이 선수가 자유투를 10번 던질 때 5번 이상 8번 이하로 성공할 확률?
pbinom(8, size=n, prob=0.7) - pbinom(4, size=10, prob=0.7)



### 05. 2006년 한국인의 1인 1일 평균 알코올 섭취량은 8.1g
###     10명을 무작위 추출한 결과
###     (16.90, 13.21, 15.67, 9.87, 13.15, 9.98, 3.56, 14.50, 8.12, 6.97)
###     평균 알코올 섭취량이 달라졌다고 할 수 있는가?
al <- c(16.90, 13.21, 15.67, 9.87, 13.15, 9.98, 3.56, 14.50, 8.12, 6.97)
al
mu <- 8.1
mu
n <- length(al)
n
xbar <- mean(al)
xbar
s <- sd(al)
s

## 정규성 검토
shapiro.test(al)
qqnorm(al); qqline(al)
# 정규성 검토 결론 : p-value가 0.05보다 크므로 정규분포를 따른다고 결론을 낼 수 있다.

## 검정
# 영가설 : 평균 알코올 섭취량은 변화가 없다.
# 대안가설 : 평균 알코올 섭취량은 변화가 있다.
t.test(al, mu=8.1, alternative = 'two.sided')

## 결론 : 평균 알코올 섭취량이 달라졌는지 알아보기 위해 무작위로 10명을 측정한 결과
##        평균 알코올 섭취량 11.193+-4.219126 로 나타났습니다. 또한 유의수준 0.05에서
##        검정통계량 2.3182 (p-value = 0.04562)로 통계적으로 유의한 결론을 낼 수없어
##        영가설을 기각하고 대안가설을 채택합니다.
##        따라서 평균 알코올 섭취량에 변화가 있다고 판단됩니다.



### 06. 정규분포에서 from <= x <= to 의 확률을 구하는 함수
###     rangenorm(from, to, mean, sd)을 작성하고
###     rangenorm(-1.96, 1.96, 0, 1)의 값을 구하시오.
pnorm(1.96, mean=0, sd=1) - pnorm(-1.96, mean=0, sd=1)


### 07.
## 문제 07-1. subcompact 자동차와 midsize 자동차의 도시 연비를 검정하시오.
mpg
cty07.1 <- mpg %>%
  filter(class %in% c('subcompact', 'midsize')) %>%
  select(cty, class)
str(cty07.1)

## 등분산성 검정
# 영가설 : 분산이 같다
# 대안가설 : 분산이 다른다
var.test(cty07.1$cty ~ cty07.1$class)

# 등분산성 검정 결론 : p-value 값이 0.05보다 작으므로 영가설 기각
#                      분산은 다르다

## 평균 동일성 검정
# 영가설 : 평균이 동일하다.
# 대안가설 : 평균이 다르다
t.test(cty07.1$cty ~ cty07.1$class, mu=0, alternative='two.sided', var.equal=F)

## 결론 : subcompact 자도창와 midsize 자동차의 도시 연비의 검정결과
##        p-value 값이 0.05보다 높아 영가설을 기각할 수 없다.
##        따라서 두 자동차의 도시연비는 차이가 없다.



## 07-2. 일반 위발류(r)와 고급 휘발유(p)의 고속도로 연비을 검정하시오.
mpg
hwy07.2 <- mpg %>%
  filter(fl %in% c('r', 'p'))
hwy07.2

## 등분산성 검정
# 영가설 : 분산이 같다.
# 대안가설 : 분산이 다르다.
var.test(hwy07.2$hwy ~ hwy07.2$fl)

# 등분산성 검정 결론 : p-value 값이 0.05보다 작으므로 영가설 기각한다.
#                      분산은 다르다

## 평균 동일성 검정
# 영가설 : hwy 차이가 없다
# 대안가설 : hwy 차이가 있다.
t.test(hwy07.2$hwy ~ hwy07.2$fl, mu=0, var.equal=F)

## 결론 : 일반 휘발류(r)와 고급 휘발유(p)의 고속도로 연비를 검정한 결과
##        p-value 값이 유의수준 0.05보다 작으므로 영가설을 기각한다.
##        따라서 두 휘발유에 딸 hwy의 차이가 있다고 판단되다.


### 08.


### 09. 키와 몸무게의 곡선회귀분석을 통한 2차 회귀식을 구하라
women
fit <- lm(weight ~ height +I(height^2), data =women)
summary(fit)
## 2차 회귀식
## weight.hat = 261.87818 - (7.34832*height) + (0.08306*heigh^2)

