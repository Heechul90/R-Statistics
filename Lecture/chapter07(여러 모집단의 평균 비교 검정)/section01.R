### section 01. 모집단이 두 개

setwd('D:/Heechul/R_Statistics/Lecture/chapter07(여러 모집단의 평균 비교 검정)')
library(prob)
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
