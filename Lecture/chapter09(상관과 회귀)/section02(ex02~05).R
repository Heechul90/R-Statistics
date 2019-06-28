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


# =======================================================
# 그림 9-4
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]

out <- lm(Height ~ Father, data=hf.son)

mean.y <- mean(hf.son$Height)

par(mar=c(4, 4, 1, 1))
plot(Height~Father, pch=21, data=hf.son, 
     xlim=c(75, 76), ylim=c(68, 75), 
     xlab="아버지의 키(인치)", ylab="아들의 키(인치)")
abline(h=mean.y, lwd=2)
abline(lm(Height~Father, data=hf.son), lty=3)

lines(c(75.5, 75.5), c(mean.y, out$fitted.values[hf.son$Father==75.5][1]))
lines(c(75.5, 75.5), 
      c(out$fitted.values[hf.son$Father==75.5][1], 
        min(hf.son$Height[hf.son$Father==75.5])), lty=3, col="red", lwd=2)

text(75.8, 70, expression(bar(y)))
arrows(75.8, 69.7, 75.8, 69.3, length=0.08)

text(75.8, 71, expression(hat(y)))
arrows(75.8, 71.3, 75.8, 72, length=0.08)

text(75.6, 70.5, expression(y-bar(y)))
arrows(75.55, mean.y, 75.55, min(hf.son$Height[hf.son$Father==75.5]), angle=90, code=3, length=0.05)


text(75.4, 72.5, expression(y-hat(y)), col="red")
arrows(75.45, 
       out$fitted.values[hf.son$Father==75.5][1], 75.45, 
       min(hf.son$Height[hf.son$Father==75.5]), 
       angle=90, code=3, length=0.05, col="red", lty=3)

text(75.4, 70, expression(hat(y)-bar(y)))
arrows(75.45, out$fitted.values[hf.son$Father==75.5][1], 
       75.45, mean.y, angle=90, code=3, length=0.05)

# 회귀분석
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]

out <- lm(Height ~ Father, data=hf.son)
anova(out)

summary(out)

no <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(out)
par(no)

# 그림 9-5
no <- par(no.readonly = TRUE)
par(mar=c(2,2,2,1))
plot(Height~Father, data=hf.son, main="", 
     xlab="아버지의 키(인치)", ylab="아들의 키(인치)", 
     ylim=c(65, 75))
abline(out, lwd=1.5)
ci <- predict(out, interval="confidence")
prd <- predict(out, interval="predict")
lines(hf.son$Father, ci[,2], lty=3, lwd=1.5, col="red")
lines(hf.son$Father, ci[,3], lty=3, lwd=1.5, col="red")
par(no)

# 잔차 분석
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]

out <- lm(Height ~ Father, data=hf.son)
out2 <- lm(dist ~ speed, data=cars)

# 그림 9-6
no <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), mar=c(2, 2, 2, 3))
plot( hf.son$Father, residuals(out), xlab="residuals", ylab="")
abline(h=0, col="red", lty=2)
plot( cars$speed, residuals(out2), xlab="residuals", ylab="" )
abline(h=0, col="red", lty=2)
par( no )

# 그림 9-7
dev.off()
no <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), mar=c(2, 2, 2, 3))
qqnorm(residuals(out), main="")
qqline(residuals(out), lty=2, col="red")
qqnorm(residuals(out2), main="")
qqline(residuals(out2), lty=2, col="red")
par( no )

# 정규성 검정
shapiro.test(residuals(out2))