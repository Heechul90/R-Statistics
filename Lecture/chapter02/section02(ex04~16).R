### section 02. 모수와 통계량

setwd('D:/Heechul/R_Statistics/Lecture/chapter02')
ranicafe <- read.csv('data/cafedata.csv', stringsAsFactors = F)
head(ranicafe)
str(ranicafe)

## 예제 2-4. 최댓값과 최솟값
ranicafe$Coffees <- as.numeric(ranicafe$Coffees)
ranicafe$Coffees
sort(ranicafe$Coffees)
sort(ranicafe$Coffees)[1]                     # 최솟값
sort(ranicafe$Coffees, decreasing = TRUE)
sort(ranicafe$Coffees, decreasing = T)[1]     # 최댓값
min(ranicafe$Coffees, na.rm = T)              # na.rm = na를 없애고 진행하라
max(ranicafe$Coffees, na.rm = T)

# 최빈값
rc <- ranicafe$Coffees
rc
stem(rc)     # 최빈값을 구하는 함수는 없다. 개념만 파악하자

# 평균과 중앙값
mean(rc, na.rm = T)     # 평균값

## 예제 2-6. 양 끝 값의 변화에 대한 평균의 변화 - 민감하게 반응
rc <- ranicafe$Coffees
rc[rc == max(rc, na.rm = T)] <- 480
mean(rc, na.rm = T)
rc

## 예제 2-7. 중앙값
rc <- ranicafe$Coffees
(median.idx<- (length(rc)+1)/2)
(rc.srt <- sort(rc))
rc.srt[median.idx]
median(rc, na.rm = T)


## 예제 2-8. 양 끝 값의 변화에 대한 중앙값의 변화 - 민간하지 않음
rc <- ranicafe$Coffees
rc[rc == max(rc)] <- 480
median(rc, na.rm = T)


## 예제 2-9. 개별 관찰값과 평균과의 차이에 대한 평균
height <- c(164, 166, 168, 170, 172, 174, 176)
height
height.m <- mean(height)
height.m
height.dev <- height - height.m
height.dev
sum(height.dev)


## 예제 2-10. 편차 제곡의 평균 구하기
height.dev2 <- height.dev^2
height.dev2
sum(height.dev2)
mean(height.dev2)     # 분산(variance)
variance <- sum(height.dev2/length(height.dev2))     # 분산 구하는 공식
variance


## 예제 2-11. 표준편차 구하기
sqrt <- sqrt(sum(height.dev2/length(height.dev2)))
sqrt
sqrt <- sqrt(variance)
sqrt


## 예제 2-12. 분산과 표준편차 구하기
# R에서는 표본의 갯수를 n-1 로 하기 때문에 수치가 다르게 나온다
var(height)     # 분산 구하는 함수
sd(height)      # 표준편차 구하는 함수 


## 예제 2-13. '라니의 카페' 커피 판매량에 대한 평균과 표준편차
rc <- ranicafe$Coffees
rc
rc.m <- mean(rc, na.rm = T)
rc.m
rc.sd <- sd(rc, na.rm = T)
rc.sd

# round(변수값, 1) = 변수값을 소수점 2째 자리에서 반올림
cat('커피 판매량', round(rc.m, 1), '±',round(rc.sd, 2),'잔')


## 2-14. 변동계수 구하기(안함)


## 2-15. 사분위수 범위와 상자도표
qs <- quantile(rc, na.rm = T)
qs
qs[4]-qs[2]
IQR(rc, na.rm = T)
bp <- boxplot(rc, main= '커피 판매량에 대한 상자도표표')


## 2-16. 이상치 판별
boxplot(cars$dist)
Q[2]
cars$dist
Q <- quantile(cars$dist)
Q
Q[4]-Q[2]
IQR(cars$dist)
ll <- Q[2] - 1.5*IQR(cars$dist)
ll
ul <- Q[4] + 1.5*IQR(cars$dist)
ul

cars$dist[cars$dist <ll]
cars$dist[cars$dist >ul]

boxplot(cars$dist, main= 'Boxplot of distance')
