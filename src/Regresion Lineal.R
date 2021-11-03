alturahombre<-c(1.54,1.62,1.70,1.75,1.82,1.89,1.90)
alturamujer<-c(1.65,1.65,1.68,1.68,1.70,1.72,1.74)
plot(alturahombre,alturamujer,pch=11,xlim=c(1.5,2),ylim=c(1.6,1.8))
modelo<-lm(alturamujer~alturahombre)
modelo$coefficients
abline(modelo)
summary(modelo)

#7.1
peso.unitario <- c(99.0, 101.1, 102.7, 103.0, 105.4, 107.0, 108.7, 110.8, 112.1, 112.4, 113.6, 113.8, 115.1,115.4,120)
porosidad <- c(28.8, 27.9, 27.0, 25.2, 22.8, 21.5, 20.9, 19.6, 17.1, 18.9, 16.0, 16.7, 13.0, 13.6, 10.8)

num <- sum((peso.unitario-mean(peso.unitario))*(porosidad-mean(porosidad)))
den <- sum((peso.unitario-mean(peso.unitario))^2)
b1 <- num / den
b0 <- mean(porosidad) - b1 * mean(peso.unitario)
porosidad.estimada <- b0 + b1*peso.unitario
plot(peso.unitario, porosidad,pch=11)
# recta de la regresión lineal
points(peso.unitario, porosidad.estimada, type = "l")
# gráfica de cada residuo
i<-seq(length(peso.unitario))
arrows(peso.unitario[i],porosidad[i],peso.unitario[i],porosidad.estimada[i], col="green", length = 0.01)
modelo<-lm(porosidad~peso.unitario)
modelo$coefficients
abline(modelo)
summary(modelo)
xtest <- rnorm(peso.unitario,mean(peso.unitario),sd(peso.unitario))
shapiro.test(xtest)

SCE <- sum(porosidad^2) - b0*sum(porosidad) - b1*sum(peso.unitario*porosidad)
SCE
s.sq <- SCE / (length(porosidad)-2)
s.sq

#7.2
beta0 <- 1800
beta1 <- 1.3
x <- 2500
beta0 + beta1*x

#7.5
library(datasets)
#?airquality
View(airquality)
dim(airquality)

modelo<-lm(airquality$Ozone~airquality$Solar.R)
modelo$coefficients
summary(modelo)

library(datasets)
#?airquality
View(trees)
dim(trees)
modelo<-lm(trees$Volume~trees$Girth)
modelo$coefficients
summary(modelo)

#metodo de minimos cuadrados
numerador<-sum((trees$Volume-mean(trees$Volume))*(trees$Girth-mean(trees$Girth)))
denominador<-sum((trees$Girth-mean(trees$Girth))^2)
b1<-numerador/denominador
b0<-mean(trees$Volume)-b1*mean(trees$Girth)
b0
b1

X <- c(20, 40, 60, 80)
Y <- c(0.24, 1.20, 1.71, 2.22)
modelo<-lm(Y~X)
summary(modelo)

#7.17

x1 <- rnorm(10, mean = 80, sd = 10)
x2 <- rnorm(10, mean = 70, sd = 5)
x3 <- 2*x1+4*x2
y1 <- 3+x1+x2+x3+rnorm(10, mean=0, sd=0.1)

modelo1 <- lm(y1~x1+x2+x3)
summary(modelo1)


x1 <- rnorm(10, mean = 80, sd = 10)
x2 <- rnorm(10, mean = 70, sd = 5)
x3 <- 2*x1+4*x2+rnorm(10,0,0.1)

y1 <- 3+x1+x2+x3 + rnorm(10, mean=0, sd=0.1)

modelo2 = lm(y1~x1+x2+x3)
summary(modelo2)