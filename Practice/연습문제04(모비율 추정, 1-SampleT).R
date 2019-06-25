setwd('D:/Heechul/R_Statistics/Practice')

library(prob)

### 연습문제04. 모비율 추정
## 문제 01. 학교 학생 중 n 명을 임의 추출
##          조사한 결과 50%의 학생이 대중교통 이용
##          대중교통을 이용해 등교하는 학생의 비율 p에 대한 신뢰도 95%의 신뢰 구간은?

# 표본비율(N, pq/n)
n <- 1000
phat <- 0.5
qhat <- 0.5

ll <- phat-1.96*sqrt(phat*qhat/n)
ul <- phat+1.96*sqrt(phat*qhat/n)
ll
ul

prop.test(500, 1000, 0.5, alternative = 'two.sided', conf.level = 0.95)


## 문제 02. 임의 추출 100명
##          4/5가 선호,
##          모비율 p에 대한 신뢰도 95%의 신뢰 구간?
n <- 100
phat <- 0.8
qhat <- 0.2

ll <- phat-1.96*sqrt(phat*qhat/n)
ul <- phat+1.96*sqrt(phat*qhat/n)
ll
ul


## 문제 03. 무작위로 1000명 추출,
##          430명이 흡연
##          모비율의 90% 신뢰구간은?
n <- 1000
phat <- 0.43
qhat <- 1-0.43

ll <- phat-1.645*sqrt(phat*qhat/n)
ul <- phat+1.645*sqrt(phat*qhat/n)
ll
ul
