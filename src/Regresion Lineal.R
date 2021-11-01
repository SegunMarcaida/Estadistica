alturahombre<-c(1.54,1.62,1.70,1.75,1.82,1.89,1.90)
alturamujer<-c(1.65,1.65,1.68,1.68,1.70,1.72,1.74)
plot(alturahombre,alturamujer,pch=11,xlim=c(1.5,2),ylim=c(1.6,1.8))
modelo<-lm(alturamujer~alturahombre)
modelo$coefficients
abline(modelo)
summary(modelo)
