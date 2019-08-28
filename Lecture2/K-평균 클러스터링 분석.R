setwd('D:/Heechul/R_Statistics/Lecture2')

head(iris)
iris2 <- iris[, 1:4]
head(iris2)

km.out.withness = c()
km.out.between = c()
ii = c()

for(i in 2:7){
  set.seed(1)
  km.out = kmeans(iris2, centers = i)
  km.out.withness[i - 1] = km.out$tot.withinss
  km.out.between[i - 1] = km.out$betweenss
  ii = c(ii, i)
}

kk =data.frame(ii, km.out.withness, km.out.between)

par(mfrow = c(1,2))
plot(kk$ii, kk$km.out.withness, type = 'o')
plot(kk$ii, kk$km.out.between, type = 'o')
par(mfrow = c(1,1))

out = kmeans(iris2, centers =3)
out$centers
out$cluster
out$size

plot(iris2)



