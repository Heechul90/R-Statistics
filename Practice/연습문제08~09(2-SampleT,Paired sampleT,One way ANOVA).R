setwd('D:/Heechul/R_Statistics/Practice')

# 필요 패키지 
library(prob)
library(dplyr)
library(ggplot2)
library(MASS)

# MASS와 dplyr 같이 사용하면 충돌이 일어남(dplyr에 select이용 지정)
# 아니면 MASS먼저 로딩하고 그 다음 dplyr 로딩하면 됨
select <- dplyr::select 


### 연습문제08. 2-Sample T 테스트
## 문제 02. mtcars 데이터셋에서 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가 
##          통계적으로 유의한지 t-test를 통해 확인해 보시오.

head(mtcars)
str(mtcars)

mpg_mean <- mtcars %>%
  select(mpg, am) %>%
  group_by(am) %>%
  summarise(mpg_mean = mean(mpg))
mpg_mean

mpg_sd <- mtcars %>%
  select(mpg, am) %>%
  group_by(am) %>%
  summarise(mpg_sd = sd(mpg))
mpg_sd

mpg_n <- mtcars %>%
  select(mpg, am) %>%
  group_by(am) %>%
  summarise(mpg_n = n())
mpg_n

# 1. 분산의 동일성 검정
# 영가설 : 오토/수동 집단의 분산은 서로 동일하다.
# 대안가설 : 오토/수동 집단의 분산은 서로 다르다.
str(mtcars)
var.test(mtcars$mpg ~ mtcars$am)

# 결론 : am:오토의 mpg의 분산과 am:수동의 mpg의 분산의 동일성을 검정한 결과
#        분산이 서로 동일하다는 가정을 만족합니다.

# 2. 모평균의 차이 검정
# 영가설 : am:0의 mpg 평균과 am:1의 mpg 평균의 동일하다. (오토평균-수동평균=0)
# 대안가설 : am:0의 mpg 평균과 am:1의 mpg 평균의 동일하지 않다. (오토평균-수동평균<0)
t.test(mtcars$mpg ~ mtcars$am, mu=0, alternative='two.sided', var.equal=T)

# 결론 : 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가 통계적으로 유의한지
#        알아보기위해 am:0 19개, am:1 13개를 측정한 결과
#        am:0은 17.14737+-3.83, am:1은 24.39231+-6.17 로 나타났습니다.
#        이를 유의수준 0.05에서 가설검정하면 검정통계량 -4.1061 (p-value = 0.0001425)로 나타나
#        am:0의 mpg 평균이 am:1의 mpg 평균과 동일하다는 통계학적으로 유의한 결론을 내릴수 없습니다.
#        즉, am:0의 mpg 평균과 am:1의 mpg 평균이 같지 않은 것으로 판단됩니다.


## 문제 02. MASS 패키지에 내장된 Cars93 데이터프레임에 대해서 생산국(Origin)이
#           USA vs. non-USA 2개의 group 에 대해서 차 가격(Price)의 평균이 
#           차이가 있는지를검정해보시오.
Cars93
head(Cars93)
str(Cars93)

price_n <- Cars93 %>%
  select(Price, Origin) %>%
  group_by(Origin) %>%
  summarise(price_n = n())
price_n

price_mean <- Cars93 %>%
  select(Price, Origin) %>%
  group_by(Origin) %>%
  summarise(price_mean = mean(Price))
price_mean

price_sd <- Cars93 %>%
  select(Price, Origin) %>%
  group_by(Origin) %>%
  summarise(price_sd = sd(Price))
price_sd

# 1. 분산의 동일성 검정
# 영가설 : USA 집단과 non-USA 집단의 분산은 서로 동일하다. (V1 / V2 = 1)
# 대안가설 : USA 집단과 non-USA 집단의 분산은 서로 다르다. (V1 / V2 != 1)
str(Cars93)
var.test(Cars93$Price ~ Cars93$Origin)

# 결론 : USA 집단의 Price 분산과 non-USA 집단의 분산의 동일성을 검정한 결과
#        유의확률(p-value = 0.01387)이 유의수준 0.05보다 작아
#        분산이 서로 동일하다는 가정을 만족하지 않습니다.

# 2. 모평균의 차이 검정
# 영가설 : USA 집단과 non-USA 집단의 평균은 서로 동일하다.(mu1 - mu2 = 0)
# 영가설 : USA 집단과 non-USA 집단의 평균은 서로 동일하지 않다.(mu1 - mu2 != 0)
t.test(Cars93$Price ~ Cars93$Origin, mu=0, alternative='two.sided', var.equal= F)

# 결론 : USA 집단과 non-USA 집단에 따라 가격에 차이가 있는지 알아보기 위해
#        집단에서 각각 48. 45 표본을 조사한 결과
#        USA집단의 가격 평균과 표준편차는 18.6+-7.82,
#        non-USA 집단의 가격 평균과 표준편차는 20.5+-11.3로 나타났습니다.
#        또한 유의수준 0.05에서 가설검정하면 검정통계량 -5.974255(p-value = 0.3428)로
#        나타나 'USA 집단과 non-USA 집단의 평균은 서로 동일하다'는 통계적으로
#        유의한 결론을 내릴 수 있습니다.
#        즉 USA 집단과 non-USA 집단의 가격은 차이가 없는것으로 판단됩니다.


## 문제 03. mpg 데이터셋에서 다음을 검정해 보시오.
## 문제 03-1. subcompact 자동차와 midsize 자동차의 고속도로 연비
mpg
str(mpg)

mpg3 <- mpg %>%
  filter(class %in% c('subcompact', 'midsize'))

mpg3_n <- mpg3 %>%
  select(class, hwy) %>%
  group_by(class) %>%
  summarise(mpg3_n = n())
mpg3_n

mpg3_mean <- mpg3 %>%
  select(class, hwy) %>%
  group_by(class) %>%
  summarise(mpg3_mean = mean(hwy))
mpg3_mean

mpg3_sd <- mpg3 %>%
  select(class, hwy) %>%
  group_by(class) %>%
  summarise(mpg3_sd = sd(hwy))
mpg3_sd
View(mpg3)

## 1. 분산의 동일성 검정
# 영가설 : subcompact 자동차의 분산과 midsize 자도창의 분산은 서로 동일하다. (V1 / V2 = 1)
# 대안가설 : subcompact 자동차의 분산과 midsize 자도창의 분산은 서로 다르다. (V1 / V2 != 1)
var.test(mpg3$hwy ~ mpg3$class)

# 결론 : subcompact, midsize 자동차의 고속도로 연비 분산이 동일한지 검정한 결과
#        유의확률(p-value = 8.825e-08)이 유의 수준0.05보다 높아 
#        두 자동차의 고속도로 연비 분산은 동일하다는 가정을 만족합니다.

## 2. 평균의 동일성 검정
# 영가설 : subcompact 자동차의 hwy와  midsize 자동차의 hwy이 차이가 없다. (mu1 - mu2 = 0)
# 대안가설 : subcompact 자동차의 hwy와  midsize 자동차의 hwy이 차이가 있다.(mu1 - mu2 != 0)
t.test(mpg3$hwy ~ mpg3$class, mu=0, alternative='two.sided', var.equal=T)

# 결론 : subcompact 자동자의 hwy 평균이 midsize 자동차의 hwy보다 큰지를 알아보기 위해
#        subcompact 35대, midsize 41대의 hwy를 측정한 결과 각각 평균과 표준편차는
#        28.14286+-5.38, 27.29268+-2.14 로 나타났습니다.
#        이를 유의수준 0.5에서 가설점정하면 검정통계량 -0.93116(p-value = 0.3548)로 나타나
#        'subcompact 자동차의 hwy와  midsize 자동차의 hwy이 차이가 없다'는 
#        결론을 내릴수 있습니다.
#        따라서 subcompact 자동차의 hwy와 midsize 자동차의 hwy는 차이가 없습니다.


## 문제 03-2. 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비
mpg
View(mpg)

mpg3.2 <- mpg %>%
  filter(fl %in% c('r', 'p'))
View(mpg3.2)

mpg3.2_n <- mpg3.2 %>%
  select(fl, cty) %>%
  group_by((fl)) %>%
  summarise(mpg3.2_n = n())
mpg3.2_n

mpg3.2_mean <- mpg3.2 %>%
  select(fl, cty) %>%
  group_by((fl)) %>%
  summarise(mpg3.2_mean = mean(cty))
mpg3.2_mean

mpg3.2_sd <- mpg3.2 %>%
  select(fl, cty) %>%
  group_by((fl)) %>%
  summarise(mpg3.2_sd = sd(cty))
mpg3.2_sd

# 1. 분산의 동일성 검정
# 영가설 : 일반휘발유의 도시연비와 고급 휘발유의 도시연비 분산은 같다(v1 / v2 = 1)
# 대안가설 : 일반휘발유의 도시연비와 고급 휘발유의 도시연비 분산은 다르다(v1 / v2 != 1)
var.test(mpg3.2$cty ~ mpg3.2$fl)

# 결론 : 일반휘발유의 도시연비 분산과 고급 휘발유의 도시연비 분산이 동일한지 검정한 결과
#        유의확율(p-value = 0.04282)이 유의수준 0.05보다 적게 나와 
#        분산이 동일하다는 가정을 유의한 결론을 내릴 수 없습니다.
#        즉 일반휘발유와 고급휘발유의 도시연비 분산은 같지 않은것으로 판단됩니다.

# 2. 평균의 동일성 검정
# 영가설 : 일반 휘발유와 고급 휘발유의 cty는 차이가 없다 (mu1 - mu2 = 0)
# 대안가설 : 일반 휘발유와 고급 휘발유의 cty는 차이가 있다 (mu1 - mu2 != 0)
t.test(mpg3.2$cty ~ mpg3.2$fl, mu=0, alternative='two.sided', var.equal=F)

# 결론 : 일반 휘발유와 고급 휘발유의 cty는 차이가 있는지 알아보기 위해
#        일반 휘발유와 고급휘발유를 각각 168, 52 표본을 측정한 결과
#        일반 휘발유의 cty 평균과 표준편차는 16.7+-3.89
#        고급 휘발유의 cty 평균과 표준편차는 17.4+-3.04 로 나타났습니다.
#        또한 유의수준 0.05에서 가설검정을 하면 검정통계량 1.2118(p-value = 0.2283)
#        로 나타나 '일반 휘발유와 고급 휘발유의 cty는 차이가 없다'는
#        통계적으로 유의한 결론을 내릴수 있습니다.
#        즉 일반 휘발유와 고급 휘발유의 cty는 차이가 없다고 판단됩니다.


## 문제 03-3. subcompact 자동차의 전륜구동(f)이냐 후륜구동(r)이냐에 따른 도시 연비
mpg
View(mpg)

mpg3.3 <- mpg %>%
  filter(class == 'subcompact') %>%
  filter(drv %in% c('f', 'r'))
mpg3.3

mpg3.3_n <- mpg3.3 %>%
  select(drv, cty) %>%
  group_by(drv) %>%
  summarise(mpg3.3_n = n())
mpg3.3_n

mpg3.3_mean <- mpg3.3 %>%
  select(drv, cty) %>%
  group_by(drv) %>%
  summarise(mpg3.3_mean = mean(cty))
mpg3.3_mean

mpg3.3_sd <- mpg3.3 %>%
  select(drv, cty) %>%
  group_by(drv) %>%
  summarise(mpg3.3_sd = sd(cty))
mpg3.3_sd

# 1. 분산 동일성 검정
# 영가설 : 전륜구동의 도시연비 분산과 후륜구동의 도시연비 분산이 같다 (v1 / v2 = 1)
# 대안가설 : 전륜구동의 도시연비 분산과 후륜구동의  도시연비 분산이 다르다(v1 / v2 != 1)
var.test(mpg3.3$cty ~ mpg3.3$drv)

# 결론 : subcompact 자동차의 전륜구동의 도시연비 분산과 후룬규동의 도시연비 분산이
#        같다는 가정을 검정한 결과
#        유의확률(p-value = 0.002612)이 유의수준 0.05 보다 적게 나와
#        분산이 동일하다는 가정을 유의한 결론을 내릴 수 없습니다.
#        즉 전륜구동의 도시연비 분산과 후륜구동의 도시연비 분산이 
#        같지 않다고 판단됩니다.

# 2. 평균 동일성 검정
# 영가설 : 전륜구동과 후륜구동의 cty 차이가 없다 (mu1 - mu2 = 0)
# 대안가설 : 전륜구동과 후륜구동의 cty 차이가 있다 (mu1 - mu2 != 0)
t.test(mpg3.3$cty ~ mpg3.3$drv, mu=0, alternative='two.sided', var.equal=F)

# 결론 : 전륜구동, 후륜구동 22, 9 표본 추출하여 측정한 결과
#        전륜구동의 평균과 표준편차 22.36364 +-4.52
#        후륜구동의 평균과 표준편차 15.88889+-1.45 로 측정
#        유의수준 0.05에서 가설검정하면
#        검정통계량 6.003(p-value = 1.759e-06)로 나타나
#        영가설을 채택합니다.
#        따라서 전륜구동과 후륜구동의 cty 차이가 없습니다.


### 연습문제09. Paired sample T 테스트
## 문제 01.
ex7.1 <- data.frame('placebo' = 
                      c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1),
                    'newmedicine' = 
                      c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0))
ex7.1

# 영가설 : 새로운 당뇨병 치료제는 효과가 없다. (mu1-mu2 < 0, mu1-mu2 = 0)
# 대안가설 : 새로운 당뇨병 치료제는 할당량을 줄여준다. (mu1 - mu2 > 0)
# 직접 구해보기
n <- length(ex7.1$placebo - ex7.1$newmedicine)
n
m <- mean(ex7.1$placebo - ex7.1$newmedicine)
m
s <- sd(ex7.1$placebo - ex7.1$newmedicine)
s
t <- m / (s/sqrt(n))
t

# R로 구해보기
t.test(ex7.1$placebo, ex7.1$newmedicine, paired = T, alternative = 'greater')

# 결론 : 새로운 당뇨병 치료제가 치료에 지대한 영향을 주는 외부요인을 통제하기 위해 
#       10명의 당뇨병 환자를 선별하여 1달 동안 placebo 투여한 혈당 수치와
#       신약 newmedicine을 투여한 혈당 수치를 비교한 결과 평균 혈당 수치 0.64 만큼
#       감소하였고 표준편차는 +-0.5699903로 나타났습니다.
#       또한 유의수준 0.05에서 통계검정량은 3.5507(p-value = 0.003105)로 나타나
#       영가설을 기각하고 대안가설을 채택합니다.
#       즉 새로운 당뇨병 치료제는 혈당수치를 줄여줍니다.



## 문제 02. 
# 영가설 : A 밑창과 B 밑창의 원재료가 닳는 정도의 차이가 없다 (muA - muB = 0)
# 대안가설 : A 밑창과 B 밑창의 원재료가 닳는 정도의 차이가 있다 (muA - muB != 0)
# 직접 검정 통계량 구해보기
n <- length(ex7.2$A - ex7.2$B)
n
m <- mean(ex7.2$A - ex7.2$B)
m
s <- sd(ex7.2$A - ex7.2$B)
s
t <- m / (s/sqrt(n))
t

# R로 구하기
t.test(ex7.2$A, ex7.2$B, paired = T, alternative = 'two.sided')

# 결론 : A 밑창과 B 밑창의 원재료가 닳는 정도의 차이를 검정하기 위해 10명의 소년을 대상으로
#        왼쪽엔 A 다른쪽에 B를 신기고 일정 기간이 지난후에 신발을 수거하여 밑창의 닳은 정도를
#        비교한 결과 B가 평균 0.41 더 높았으며, 표준편차는 +-0.3871549로 나타났다.
#        또한 유의수준 0.05에서 검정통계량은 -3.3489(p-value = 0.008539)로 나타나
#        영가설을 기각하고 대안가설을 채택합니다.
#        즉 A 밑창과 B 밑창의 원재료가 닳는 정도의 차이가 있다고 판단됩니다.


### 연습문제10. 일원 분산분석(One way ANOVA)
## 문제 01.
## 문제 01. 3개 호수의 산소량의 차이가 있는지 없는지 알아보자.
lake1 <- c(5, 7, 6, 8, 6, 7, 8, 8, 6, 10)
lake2 <- c(6, 8, 9, 11, 13, 12, 10, 8, 9, 10)
lake3 <- c(14, 25, 26, 18, 19, 22, 21, 16, 20, 30)
group <- c(rep('1',10), rep('2',10), rep('3',10))
group
lake <- data.frame('group' = group, ppm=c(lake1, lake2, lake3))
lake
par(mfrow=c(1,1))

# 1. 각 호수의 정규성 검점
# 1) lake1
shapiro.test(lake1)
qqnorm(lake1); qqline(lake1)

# 2) lake2
shapiro.test(lake2)
qqnorm(lake2); qqline(lake2)

# 3) lake3
shapiro.test(lake3)
qqnorm(lake3); qqline(lake3)

# 정규성 검토 결론 : lake1~3의 p_value가 0.05 이상이므로 정규분포를 따른다고 결론을 내린다. 

# 2. anova 검정
# 영가설 : 호수에 따른 ppm 차이가 없다
# 대안가설 : 호수에 따른 ppm 차이가 있다
anova(lm(ppm ~ group, data = lake))

## 결론 : 유의수준 0.05에서 검정한 결과, p-value가 0.05보다 작음으로 영가설을 기각하고
##        대안가설을 채택한다.
##        따라서 호수에 따른 ppm은 차이가 난다고 판단됩니다. 


## 문제 02.
A <- c(15.5, 14.3, 16.3, 13.5, 15.7, 16.4, 14.7)
B <- c(14.7, 16.3, 15.5, 15.2, 16.3, 13.5, 15.4)
C <- c(15.5, 13.2, 16.5, 15.7, 15.3, 15.2, 14.8)
group <- c(rep('1',7), rep('2',7), rep('3',7))
group
vege <- data.frame('group' = group, 'price' = c(A, B, C))
vege

# 1. 정규성 검정
# 1) A채소
shapiro.test(A)
qqnorm(A); qqline(A, col = 'red')

# 2) B채소
shapiro.test(B)
qqnorm(B); qqline(B, col = 'red')

# 3) C채소
shapiro.test(C)
qqnorm(C); qqline(C, col = 'red')

# 정규성 검토 결론 : A,B,C채소의 p_value가 0.05 이상이므로 정규분포를 따른다고 결론을 내린다. 

# 2. anova 검정
# 영가설 : 3개 채소에 대한 도매시장 7곳의 가격 차이는 없다 (mu1 = mu2 = mu3)
# 대안가설 : 3개 채소에 대한 도매시장 7곳의 가격 차이는 있다.
anova(lm(price ~ group, data = vege))

# 결론 : A, B, C 3개 채소에 대한 가격이 차이가 있는지 없는지 알아보기 위해
#        도매시장 7곳의 가격을 조사한 결과
#       A 채소의 가격 평균과 표준편차는 15.2+-1.075484,
#       B 채소의 가격 평균과 표준편차는 15.27143+-0.9707631
#       C 채소의 가격 평균과 표준편차는 15.17143+-1.016061로 나타났습니다.
#       또한, 일원분산분석을 통해 채소의 가격 차이를 검정한 결과
#       유의수준 0.05에서 p-value가 0.98로 나타나 영가설을 채택합니다.
#       따라서 채소에 따라서 가격의 변화는 없다고 판단됩니다.



### 연습문제11. 적합도 / 독립성 검정
## 문제 01. 어느 공정의 부적합품률 15%
##          시료 80개를 추출
##          검사한 결과 불량이 16개
##          유의수준 5%로 적합도 검정을 하시오.
p <- 0.15
q <- 1-p
n <- 80

 # 영가설 : 부적합률이 15% 이다. (p = 0.15)
 # 대안가설 : 부적합율이 15%가 아니다 (p != 0.15)

# 관찰도수 64, 16     
# 기대도수 80*0.85=68, 80*0.15=12  

# 검정
x <- c(64, 16)
chisq.test(x, p=c(85,15)/100)

# 결론 : 어느 공정의 부적합품률이 15%인지 알아보기 위해 시료 80개를 추출하여
#       측정한 결과 적합품 64개, 부적합품 16개로 나타났습니다. 또한 
#       유의수준 0.05에서 검정통계량 1.5686(p-value = 0.2104)로 
#       영가설을 기각할수 없습니다. 따라서
#       이 공정의 부적합률은 15%로 판단됩니다.


options(digits=7)

## 문제 02.
ex11.2 <- matrix(c(23, 31, 13, 21, 48, 23, 63, 159, 119), nrow=3, ncol=3)
colnames(ex11.2) <- c('1갑이상', '1갑이하', '안피움')
rownames(ex11.2) <- c('반병이상', '반병이하', '못마심')
ex11.2
addmargins(ex11.2)

# 영가설 : 음주량과 흡연량은 관련이 없습니다(서로 독립)
# 대안가설 : 음주량과 흡연량은 관련이 있습니다(서로 연관이 있다)

# 검정
chisq.test(ex11.2)

# 결론 : 음주량과 흡연량 사이에 연관이 있는지 알아보기위해 500명을 추출해 
#        측정한 결과 유의수준 0.05에서 검정통계량 12.827(p-value = 0.01215)로
#        영가설을 기각합니다. 따라서 음주량과 흡연량은 서로 연관이 
#        있다고 판단됩니다.