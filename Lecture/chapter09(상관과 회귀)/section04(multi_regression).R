### multi_regression(다중회귀분석)

setwd('D:/Heechul/R_Statistics/Lecture/chapter09(상관과 회귀)')

state.x77
head(state.x77)
states <- as.data.frame(state.x77[,c('Murder', 'Population',
                                     'Illiteracy', 'Income', 'Frost')])
states

fit <- lm(Murder ~ Population+Illiteracy+Income+Frost, data=states)
summary(fit)
par(mfrow = c(2,2))
plot(fit)

fit1 <- lm(Murder ~ . , data = states) # 나머지 변수 다
summary(fit1)


# Population, Illiteracy은 별 영향 없을까?
fit2 <- lm(Murder ~ Population + Illiteracy, data = states)
summary(fit2)

# AIC(Akaike Information Criterion)
AIC(fit1, fit2)

# Backward stepwise regression, Forward stepwise regression
# Backward 모든변수부터
# Forward 상수항부터
step(fit1, direction = 'backward')

fit3 <- lm(Murder ~ 1, data = states)
step(fit3, direction = 'forward',
     scope = ~Population + Illiteracy + Income + Frost)
step(fit3, direction = 'forward', scope = list(upper = fit1, lower = fit3))


# 모든 가능한 조합을 다 시도
install.packages('leaps')
library(leaps)
subsets <- regsubsets(Murder ~., data=states,
                      method = 'seqrep', nbest = 4)
subsets <- regsubsets(Murder~., data = states,
                      method = 'exhaustive', nbest = 1)

summary(subsets)
par(mfrow = c(1,1))
plot(subsets)
