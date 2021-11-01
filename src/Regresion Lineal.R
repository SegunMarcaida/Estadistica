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
xtest <- norm
