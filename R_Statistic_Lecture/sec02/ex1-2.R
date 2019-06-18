### Section 2. R과 R Studio 
setwd('D:/Heechul/R_Statistics/R_Statistic_Lecture/sec02')

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

## factor
x <- 1:5
x

factor(x, levels = c(1:4))                                                # factor가 되었음
factor(x, levels = c(1:4), labels = c('a', 'b', 'c', 'd'))                # level이 a,b,c,d 가 되었음
factor(x, levels = c(1:4), labels = c('a', 'b', 'c', 'd'), ordered = T)   # 순서가 a,b,c,d, 가 되었음

# factor로 일월화수목금토 만들기
week <- factor(c(1:7), levels = c(1:7), labels = c('일', '월', '화', '수', '목', '금', '토'), ordered = T)
week

## data.frame
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
