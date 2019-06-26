### Section 2. R과 R Studio 
setwd('D:/Heechul/R_Statistics/Lecture/chapter01')

## 예제 1-1. 프로젝트 생성


## 예제 1-2. R Script 파일 생성, 명령 실행하기와 종료
# R 기초
seq(1, 5, 2)
seq(from=1, to= 5, by= 2)
seq(by=2, to=5, from=1)
seq(2, 5, 1)
?seq()
seq(-3, 3, length.out = 60)   # 길이가 60개가 되게끔 -3부터 3까지 나열
seq(-3, 3, length.out = 61)                 
seq(-3, 3, 0.1)
seq(1,100,3)
x <- seq(-3, 3, 0.1)
x[1]
x[1, 11, 21, 31]              # 에러가 나기때문에 밑에처럼 입력해준다.
x[c(1, 11, 21, 31)]
x[x>2.45]

# factor
x <- 1:5
x

factor(x, levels = c(1:4))                                                # factor가 되었음
factor(x, levels = c(1:4), labels = c('a', 'b', 'c', 'd'))                # level이 a,b,c,d 가 되었음
factor(x, levels = c(1:4), labels = c('a', 'b', 'c', 'd'), ordered = T)   # 순서가 a,b,c,d, 가 되었음

# factor로 일월화수목금토 만들기
week <- factor(c(1:7), levels = c(1:7), labels = c('일', '월', '화', '수', '목', '금', '토'), ordered = T)
week

# data.frame
name <- c('철수', '영희', '길동')
age <- c(21,20,31)
gender <- factor(c('M', 'F', 'F'))
person <- data.frame(NAME=name, AGE=age, GENDER=gender)
person
str(person)
person[3,3] <- 'M'
person
colnames(person) <- c('이름', '나이', '성별')
person
person$Phone <- c(010, 011, 016)
person
rownames(person) <- c('하나', '둘', '셋')
person


## 예제 1-3. 통계청이 제공하는 마이크로데이터 받아오기


## 예제 1-4 파일확인 및 파일명 변경
setwd('D:/Heechul/R_Statistics/Lecture/chapter01')
data <- read.csv('data/2010년 인구사항.csv', header=F, na.strings=c('.'))
str(data)
data$V1 <- factor(data$V1, levels=c(1,2),
                  labels= c('남자', '여자'))
data$V3 <- factor(data$V3, levels=1:14, 
                  labels= c('가구주', '가구주의 배우자', '자녀', 
                            '자녀의 배우자', '가구주의 부모',
                            '배우자의 부모', '손자녀, 그 배우자',
                            '증손자녀, 그 배우자', '조부모',
                            '형제자매, 그 배우자', 
                            '형제자매의 자녀, 그 배우자',
                            '부모의 형제자매, 그 배우자', '기타 친인척', 
                            '그외같이사는사람'))
data$V4 <- factor(data$V4, levels=1:8,
                  labels=c('안 받았음', '초등학교', '중학교',
                           '고등학교', '대학-4년제 미만', '대학-4년제 이상',
                           '석사과정', '박사과정'))
str(data)
save.image('data.rda')


