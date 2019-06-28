### Polynomial Regression

setwd('D:/Heechul/R_Statistics/Lecture/chapter09(상관과 회귀)')

women
View(women) # height(inch), weight(lbs)

# 신장에 따른 몸무게
plot(weight~height, data=women)
fit <- lm(weight~height, data=women)
abline(fit, col="red", lwd=2)

summary(fit)
cor.test(women$weight, women$height)

par(mfrow=c(2,2))
plot(fit) # 정규성(2-x), 독립성, 선형성(1-x), 등분산성(3-o)
par(mfrow=c(1,1))

fit2 <- lm(weight~height + I(height^2), data=women)
plot(weight~height, data=women)
lines(women$height, fitted(fit2), col="green", lwd=2)

summary(fit2)
par(mfrow=c(2,2))
plot(fit2) # 정규성(2-o), 독립성, 선형성(1-o), 등분산성(3-o)
par(mfrow=c(1,1))

fit3 <- lm(weight~height + I(height^2) + I(height^3), data=women)
plot(weight~height, data=women)
lines(women$height, fitted(fit3), col="orange", lwd=2)

summary(fit3)
par(mfrow=c(2,2))
plot(fit3) # 정규성(2-x), 독립성, 선형성(1-o), 등분산성(3-o)
par(mfrow=c(1,1))

AIC(fit2)
AIC(fit3)