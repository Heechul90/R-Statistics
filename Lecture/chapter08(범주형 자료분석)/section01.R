### section 01. 적합도 검정 : 범주의 각 수준별 비율 검정

setwd('D:/Heechul/R_Statistics/Lecture/chapter08(범주형 자료분석)')

## 필요 패키지


## 예제

x <- seq(0, 15, by=0.01)
dc <- dchisq(x, df=3)

alpha <- 0.05
tol <- qchisq(0.95, df=3)

par(mar=c(0,1,1,1))
plot(x, dc, type="l", axes=F, ylim=c(-0.03, 0.25), xlab="", ylab="")
abline(h=0)
tol.g <- round(tol, 2)
polygon(c(tol.g, x[x>tol.g], 15), c(0, dc[x>tol.g], 0), col="red")
text(0, -0.03, "0", cex=0.8)
text(tol, -0.03, expression(chi[0.05]^{2}==2.14), cex=0.8)

# 예제-1. 멘델의 법칙
x <- c(315, 101, 108, 32)
chisq.test(x, p=c(9, 3, 3, 1)/16)

# 그래프에 표시
x <- seq(0, 15, by=0.01)
dc <- dchisq(x, df=3)
par(mar=c(0,1,1,1))
plot(x, dc, type="l", axes=F, ylim=c(-0.03, 0.25), xlab="", ylab="")
abline(h=0)
tol.g <- round(tol, 2)
polygon(c(tol.g, x[x>tol.g], 15), c(0, dc[x>tol.g], 0), col="red")
text(0, -0.01, "0", cex=0.8)
text(tol, -0.03, expression(chi[0.05]^{2}==2.14), cex=0.8)

tol2 <- qchisq(1-0.9254, df=3)
tol2.g <- round(tol2, 2)
polygon(c(tol2.g, x[x>tol2.g], 15), c(0, dc[x>tol2.g], 0), col="blue", density=20, angle=305)
text(tol2, -0.03, expression(chi[0.9254]^{2}==0.47), cex=0.8)
arrows(7, 0.17, 4, 0.12, length=0.05)
text(7, 0.177, expression(plain(P)(F>0.47) == 0.9254), cex=0.8)
