### 연습문제01. 이항분포

setwd('D:/Heechul/R_Statistics/Practice/연습문제01')
# d = 어떤 값일때 확률, p = 누적확률, q = 확률 -> x, r = 난수 생성

## 1. 

# 1-1.


# 1-2.


# 1-3.



# 1-4. 



## 2. 패널티킥 성공확률 4/5, 10번 시도해서 7골 성공 확률? 
# 성공확률 = 4/5, 실행 횟수 = 10
n <- 10
p <- 0.8
x <- 1:n

# 7번 성공할때 확률
dbinom(7, size = n, prob = p)


## 3. 불량률 5%, 20개 조사해서 불량이 2개 이하 나올 확률?
# 불량률 5%, 실행 횟수 = 20
n <- 20
p <- 0.05
x <- 1:n

# 불량이 0개, 1개, 2개가 나올 확률
dbinom(0, size = n, prob = p) + dbinom(1, size = n, prob = p) + dbinom(2, size = n, prob = p)
pbinom(2, size = n, prob = p)

## 4. 치료율 20%, 20명을 치료했을때 2명 이상이 회복할 확률?
n <- 20
p <- 0.2
x <- 1:n

# 2명~20명이 치료될 확률은 20명이 치료될 확률 - 0명,1명이 치료될 확률
pbinom(20, size = n, prob = p) - pbinom(1, size = n, prob = p)


## 5. 주사위 두 개를 던졌을때, 눈금의 합이 6이 될 확률?
# 합이 6 나올 확률 5/36, 
5/36




### 연습문제02. 정규 분포
## 1. 전구의 수명 800일, 표준편차 40일, 전구의 수명이 750일 이하일 확률?
# 전구의 평균 수명 800, 표준편차 40, 
options(digits = 3)
mu <- 800
sigma <- 40
ll <- mu - 3*sigma
ul <- mu + 3*sigma

x <- seq(ll, ul, by=0.01)
x
nd <- dnorm(x, mean = mu, sd = sigma)
plot(x, nd, type='l')

pnorm(750, mean = mu, sd = sigma)


## 2. 근무기간 평균 11년, 분산이 16년이 정규분포
# 2-1. 20년 이상 근무한 종업원의 비율
mu <- 11
sigma <- 4
pnorm(mu, mean = mu, sd = sigma)

# 19년 이하 근무한 종업원 확률을 1에서 빼주면 나머지 20년 이상 근무한 종업원 확률
1- pnorm(19, mean = mu, sd = sigma)


# 2-2. 근무연수가 가장 오래된 10%의 종업원은 몇 년 이상 근무?

# 전체에서 90%의 종업원 
qnorm(0.9, mean = mu, sd = sigma)


## 3. 수학성적 평균 70, 표준편차 8, 점수가 80점 이상이고 90점 이하인 학생 비율?
mu <- 70
sigma <- 8

# 성적이 90이하인 학생 비율에서 80이하인 학생 비율 빼주기
pnorm(90, mean = mu, sd = sigma) - pnorm(80, mean = mu, sd = sigma)



## 4. 평균 1.5, 표준편차 2, H(t) = P(t <= X <= t+1)
# H(0) + H(2) ?
mu <- 1.5
sigma <- 2

