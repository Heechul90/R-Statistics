### section 01. 모집단이 두 개

setwd('D:/Heechul/R_Statistics/Lecture/chapter07(여러 모집단의 평균 비교 검정)')

## 필요 패키지
install.packages('PairedData')
library(prob)
library(PairedData)

## 7장을 위한 준비
data <- read.table('http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt', header = F)
names(data) <- c('time', 'gender', 'weight', 'minutes')
head(data)
names(data)[1] <- 'time.24Hrs'
chapter7 <- data[,c(2,3)]
write.table(chapter7, 'chapter7.txt', row.names=F)


## 예제 01. 남아 신생아와 여아 신생아의 몸무게
data <- read.table('data/chapter7.txt', header = T)
data

# R을 이용한 분산의 동일성 검정
# 영가설 : 분산이 같을 때, 대안가설 : 분산이 다를때
var.test(data$weight ~ data$gender)

# 모평균의 차이 검정
# 영가설 : 여 몸무게와 남 몸무게의 평균은 같다. 여평균 - 남평균 = 0
# 대안가설 : 여 몸무게는 남 몸무게 보다 작다. 여평균 - 남평균 < 0
t.test(data$weight ~ data$gender, mu=0, alternative='less', var.equal=T)


## 예제 02. 식욕부진증 치료요법의 효과 검정
anorexia
head(anorexia)
summary(anorexia)
# 영가설 : 신경성 식욕부진증 치료요법은 효과가 없다. (H0 : mu>=0, mu=0)
# 대안가설 : 신경성 식욕부진증 치료요법은 효과가 있다. (H0 : mu<0)

n <- length(anorexia$Prewt - anorexia$Postwt)
n
xbar <- mean(anorexia$Prewt - anorexia$Postwt)
xbar
sd <- sd(anorexia$Prewt - anorexia$Postwt)
sd

# 검정통계량
t <- xbar / (sd/sqrt(n))
t

# 유의 수준
alpha <- 0.05
c.u <- qt(alpha, df=n-1)
c.u

# 유의 확률
p_value <- pt(t, df=n-1)
p_value

t.test(anorexia$Prewt, anorexia$Postwt, paired = T, alternative = 'less')

# 결론 : 새롭게 개발한 신경성 식욕부진증 치료요법의 효과가 있는지 알아보기 위해
#        72명을 대상으로 치료요법 시행 전 몸무게를 측정하고 시행 후 측정하여
#        차이를 구한 결과 몸무게가 평균 2.763889 증가하였고, 표준편차는 +-7.983598로
#        나타났습니다.또한 유의수준 0.05에서 검정통계량 -2.9376(p-value = 0.002229)로 나타나
#        '신경성 식욕부진증 치료요법은 효과가 있다'는 통계적으로 유의한 결론을 얻을수 있습니다.
#        따라서 식욕부진증 치료요법은 효과가 있다고 판단됩니다.

