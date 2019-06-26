setwd('D:/Heechul/R_Statistics/Practice')

library(prob)
library(dplyr)
library(MASS)

### 연습문제06. 2-Sample T 테스트
## 문제 02. mtcars 데이터셋에서 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가 
##          통계적으로 유의한지 t-test를 통해 확인해 보시오.

head(mtcars)
str(mtcars)

# 1. 분산의 동일성 검정
# 영가설 : 오토/수동 집단의 분산은 서로 동일하다.
# 대안가설 : 오토/수동 집단의 분산은 서로 다르다.
var.test(mtcars$mpg ~ mtcars$am)

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

# 결론 : am:오토의 mpg의 분산과 am:수동의 mpg의 분산의 동일성을 검정한 결과
#        분산이 서로 동일하다는 가정을 만족합니다.

# 2. 모평균의 차이 검정
# 영가설 : am:0의 mpg 평균과 am:1의 mpg 평균의 동일하다. (오토평균-수동평균=0)
# 대안가설 : am:0의 mpg 평균과 am:1의 mpg 평균의 동일하지 않다. (오토평균-수동평균<0)
t.test(mtcars$mpg ~ mtcars$am, mu=0, alternative='less', var.equal=T)

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
