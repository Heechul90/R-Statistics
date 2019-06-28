### section 02. 회귀분석

setwd('D:/Heechul/R_Statistics/Lecture/chapter09(상관과 회귀)')

## 예제 9-2. 아버지와 아들 키 자료로부터 회귀계수 추정
# 아버지 아들 키 평균
mean.x <- mean(hf.son$Father)
mean.x
mean.y <- mean(hf.son$Height)
mean.y

# 공분산
sxy <- sum((hf.son$Father - mean.x) * (hf.son$Height))
sxy

# 아버지 키 분산
sxx <- sum((hf.son$Father - mean.x)^2)
sxx

# 회귀계수 
b1 <- sxy / sxx
b1
b0 <- mean.y - b1 * mean.x
b0

# R로 회귀계수(lm()함수)
out <- lm(Height ~ Father, data=hf.son)

## 예제 9-3. 회귀모형의 유의성 검정
anova(out)

## 예제 9-4. 회귀계수의 유의성 검정
summary(out)

# 좋은 선형 모델
par(mfrow = c(2,2))
plot(out)
# 정규성 - 두번째 그림(점들이 선을 따라 있어야 함)
# 독립성
# 선형성 - 첫번째 그림(점들이 분산되어 있어야 함)
# 등분산성 - 세번째 그림(점들이 분산되어 있어야 함)

## 예제 9-5. R 내장함수를 이용한 정규성 검정
women

# 신장에 따른 몸무게 
par(mfrow= c(1,1))
plot(weight ~ height, data = women)
fit <- lm(weight ~ height, data=women)
abline(fit, col='red', lwd=2)

summary(fit)
cor.test(women$weight, women$height)


par(mfrow= c(2,2))
plot(fit)


fit2 <- lm(weight ~ height + I(height^2), data = women)
par(mfrow = c(1,1))
plot(weight~height, data = women)
lines(women$height, fitted(fit2), col = 'green', lwd=2)
summary(fit2)
par(mfrow = c(2,2))
plot(fit2)


fit3 <- lm(weight ~ height + I(height^2) + I(height^3), data = women)
par(mfrow = c(1,1))
plot(weight~height , data = women)
lines(women$height, fitted(fit3), col = 'orange', lwd = 2)

summary(fit3)
par(mfrow = c(2,2))
plot(fit3)


# 작은것이 좋다
AIC(fit2)
AIC(fit3)




