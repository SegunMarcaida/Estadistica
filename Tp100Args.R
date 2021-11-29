library(readr)
library(fitdistrplus)
Muestreo_100_Args <- read_csv("src/Muestreo 100 Args.csv",
                              col_types = cols(Indice = col_integer(), 
                                               Fecha = col_date(format = "%d/%m/%Y"), 
                                               PuntajeMasPopular = col_integer(), 
                                               mp2 = col_integer(), mp3 = col_integer(), 
                                               mp4 = col_integer(), mp5 = col_integer(), 
                                               PuntajeSegundaMasPopular = col_integer(), 
                                               sp2 = col_integer(), sp3 = col_integer(), 
                                               sp4 = col_integer(), sp5 = col_integer(), 
                                               NumeroDeMasPopulares = col_integer(), 
                                               NumeroDeSegundaMasPopulares = col_integer(), 
                                               NumeroDeRespuestasMalas = col_integer(), 
                                               PuntajePrimero = col_integer(), PuntajeSegundo = col_integer(), 
                                               Total = col_integer(), Ganadores = col_integer()))

#relacion entre cantidad de respuestas mas populares y putaje.


NrepMasPop <- Muestreo_100_Args$NumeroDeMasPopulares
total<- Muestreo_100_Args$Total
NrepSegMasPop<-Muestreo_100_Args$NumeroDeSegundaMasPopulares
Nrepmalas<-Muestreo_100_Args$NumeroDeRespuestasMalas



RespuestaSegundaaPopular<-c(18,22,21,25,25,24,23,20,17,19,27,15,25,25,22,27,26,19,18,25,24,19,23
                            ,22,25,25,22,23,21,23,18,22,17,25,25,24,20,20,21,26,25,26,22,21,24,20,20,19,23,25,21,20,26,27,19,24,22,20,18,
        22,26,23,20,24,22,24,24,23,19,21,24,21,17,26,18,22,21
        ,25,26,18,22,25,20,23,25,20,26,21,26,24,22,21,19,22,24,22,21,24,24,25,23,
        21,24,22,23,25,26,24,19,24,26,20,19,23,23,22,24,26,26,25,27)

RespuestaMasPopular<-c(29,31,27,31,27,27,27,40,28,33,32,31,36,28,38,31,35,34,38,32,31,42,36,36,29,27,34,29,28,36,25,28,27,34,
        29,27,33,27,30,30,32,31,33,39,31,31,31,32,37,36,42,29,31,27,31,34,34,27,26,33,31,31,34,29,38,36,30,28,36,
        36,25,27,34,37,30,29,31,32,33,35,37,37,27,27,38,30,37,26,26,37,26,36,36,36,30,35,37,33,39,33,37,33,33,34,29,34,33,34,34,
        37,36,27,32,28,32,31,36,31,33,33,32,30,36,28,31,27,36,37,36,32,28,29,26,31,37,
        29,32,36,37,38,39,40,29,37,29,32,36,37,38,39,40,32,23,29,34,33,32,33,34,36,33,37,36,36,35,27,35,
        31,29,30,29,36,36,34,28,38,34,27,28,34,25,40,26,35,34,26,33,43,29,23,30,31,28,34,35,33,33,37,29,37,
        31,26,27,32,36,30,28,30,31,26,33,26,31,32,39,32,32,37,38,41,29,33,32,32,34,24,33,32,34,27,29,28,27,29,31,
        30,38,37,28,32,33,27,29,29,31,36,33,34,36,27,36,37,33,37,34,28,34,27,27,36,32,30,32,31,36)

# la probabilidad de acertar una x cantidad de respuestas mas popular sigue una distribucion Binomial(5,p)
# estimo p por metodo de momentos
rmp0<-length(which(NrepMasPop ==0))
rmp1<-length(which(NrepMasPop ==1))
rmp2<-length(which(NrepMasPop ==2))
rmp3<-length(which(NrepMasPop ==3))
rmp4<-length(which(NrepMasPop ==4))
rmp5<-length(which(NrepMasPop ==5))
hist(NrepMasPop)
p.sombreroMP<-(rmp1*1+rmp2*2+rmp3*3+rmp4*4+rmp5*5)/(5*length(NrepMasPop))

#lo mismo para segunda mas populares
prsmp0<-length(which(NrepSegMasPop ==0))
rsmp1<-length(which(NrepSegMasPop ==1))
rsmp2<-length(which(NrepSegMasPop ==2))
rsmp3<-length(which(NrepSegMasPop ==3))
rsmp4<-length(which(NrepSegMasPop ==4))
rsmp5<-length(which(NrepSegMasPop ==5))
hist(NrepSegMasPop)
p.sombreroSMP<-(rsmp1*1+rsmp2*2+rsmp3*3+rsmp4*4+rsmp5*5)/(5*length(NrepSegMasPop))


rm0<-length(which(Nrepmalas ==0))
rm1<-length(which(Nrepmalas ==1))
rm2<-length(which(Nrepmalas ==2))
rm3<-length(which(Nrepmalas ==3))
rm4<-length(which(Nrepmalas ==4))
rm5<-length(which(Nrepmalas ==5))
rm6<-length(which(Nrepmalas ==6))
rm7<-length(which(Nrepmalas ==7))
rm8<-length(which(Nrepmalas ==8))
rm9<-length(which(Nrepmalas ==9))
rm10<-length(which(Nrepmalas ==10))
nrm <- rm1*1+rm2*2+rm3*3+rm4*4+rm5*5+rm6*6+rm7*7+rm8*8+rm9*9+rm10*10
hist(Nrepmalas)
p.sombreroRM<-(rm1*1+rm2*2+rm3*3+rm4*4+rm5*5+rm6*6+rm7*7+rm8*8+rm9*9+rm10*10)/(10*length(Nrepmalas))

#con estos valores puedo obtengo la probabilidad de adivinar una respuesta popular

# las respuestas populares siguen una distribucion normal?
qqnorm(RespuestaMasPopular)
qqline(RespuestaMasPopular)
boxplot(RespuestaMasPopular)
hist(RespuestaMasPopular, col='steelblue', main='Normal')
x1<-rnorm(length(RespuestaMasPopular),mean(RespuestaMasPopular),sd(RespuestaMasPopular))
plot(x1, dnorm(x1,mean(RespuestaMasPopular),sd(RespuestaMasPopular)), col = "green", type = "p", ylim = c(0, 0.5))

qqnorm(RespuestaSegundaaPopular)
qqline(RespuestaSegundaaPopular)
boxplot(RespuestaSegundaaPopular)
hist(RespuestaSegundaaPopular, col='steelblue', main='Normal')
x2<-rnorm(length(RespuestaSegundaaPopular),mean(RespuestaSegundaaPopular),sd(RespuestaSegundaaPopular))
plot(x2, dnorm(x2,mean(RespuestaSegundaaPopular),sd(RespuestaSegundaaPopular)), col = "green", type = "p", ylim = c(0, 0.5))
#se puede comprobar graficamente que se puede aseemejar las distribuciones a una normal(en realidad no pero bueno)

#asumimos distribucion uniforme para las respuestas malas puesto que al relizar el muestro no tomamos los valores exactos.Igualmente
#la probabilidad de que la respuesta tenga 0-7 deberia ser la misma

modelo<-lm(total~NrepMasPop+NrepSegMasPop+Nrepmalas)
summary(modelo)
xrmp<-rbinom(1000,5,p.sombreroMP)
xrsmp<-rbinom(1000,5,p.sombreroSMP)
xrm<-rbinom(1000,10,p.sombreroRM)
totalPuntos<-145.583 +15.010*xrmp+8.616*xrsmp-10.487*xrm
mean<-mean(totalPuntos)
sd<-sd(totalPuntos)
pnorm(200,mean,sd,lower.tail = FALSE)
mean
sd
boxplot(totalPuntos)
sd(RespuestaSegundaaPopular)