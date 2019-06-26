setwd('D:/Heechul/R_Statistics/Practice')

library(prob)
### 연습문제06. 2-Sample T 테스트
## 문제 02. mtcars 데이터셋에서 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가 
##          통계적으로 유의한지 t-test를 통해 확인해 보시오.

mtcars

# 1. 분산의 동일성 검정
# 영가설 : 오토/수동 집단의 분산은 서로 동일하다.
# 대안가설 : 오토/수동 집단의 분산은 서로 다르다.
var.test(mtcars$mpg ~ mtcars$am)

# 결론 : am:오토의 mpg의 분산과 am:수동의 mpg의 분산의 동일성을 검정한 결과
#        분산이 서로 동일하다는 가정을 만족합니다.

# 2. 모평균의 차이 검정
# 영가설 : am:오토의 mpg의 평균과 am:수동의 mpg의 평균의 동일하다. (오토평균-수동평균=0)
# 대안가설 : am:오토의 mpg의 평균과 am:수동의 mpg의 평균의 동일하지 않다. (오토평균-수동평균<0)
t.test(mtcars$mpg ~ mtcars$am, mu=0, alternative='less', var.equal=T)

# 결론 : 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가 통계적으로 유의한지
#        알아보기위해 

### 데이터 바꾸고 다시 해야함.