#6.1
control<-c(88.6, 73.2, 91.4, 68, 75.2)
gr1<-c(63.0, 53.9, 69.2, 50.1, 71.5)
gr2<-c(44.9, 59.5, 40.2, 56.3, 38.7)
gr3<-c(31.0, 39.6, 45.3, 25.2, 22.7)
grupos <- c(rep("red", length(control)), rep("green", length(gr1)), rep("blue", length(gr2)), rep("yellow",length(gr3)))
aov(c(control, gr1, gr2, gr3)~grupos)
summary(aov(c(control, gr1, gr2, gr3)~grupos))
I <- 4 # cantidad de grupos
J <- 5 # cantidad de observaciones en cada grupo
mediatotal <- mean(c(control, gr1, gr2, gr3))
CMTr <- (J/(I-1))*((mean(control) - mediatotal)^2 + (mean(gr1) - mediatotal)^2 + (mean(gr2) - mediatotal)^2 + (mean(gr3) - mediatotal)^2)
CME <- (sd(control)^2 + sd(gr1)^2 + sd(gr2)^2 + sd(gr3)^2) / I
F <- CMTr / CME #estadistico
f.critico <- qf(1-0.05,3,16)
F>f.critico
#6.2
InsectSprays
tipoA <- c(10, 7, 20, 14, 14, 12, 10, 23, 17, 20, 14, 13)
tipoB <- c(11, 17, 21, 11, 16, 14, 17, 17, 19, 21, 7, 13)
tipoC <- c(0, 1, 7, 2, 3, 1, 2, 1, 3, 0, 1, 4)
tipoD <- c(3, 5,12, 6, 4, 3, 5, 5, 5, 5, 2, 4)
tipoE <- c(3, 5, 3, 5, 3, 6, 1, 1, 3, 2, 6, 4)
tipoF <- c(11, 9, 15, 22, 15, 16, 13, 10, 26, 26, 24, 13)
qqnorm(tipoD,pch=11)
qqline(tipoD)
sd(tipoF)

I <- 6 # cantidad de grupos
J <- 12 # cantidad de observaciones en cada grupo
mediatotal <- mean(c(tipoA, tipoB, tipoC, tipoD, tipoE, tipoF))
CMTr <- (J/(I-1))*((mean(tipoA) - mediatotal)^2 + (mean(tipoB) - mediatotal)^2 + (mean(tipoC) - mediatotal)^2 + (mean(tipoD) - mediatotal)^2 + (mean(tipoE) - mediatotal)^2 + (mean(tipoF) - mediatotal)^2)
CME <- (sd(tipoA)^2 + sd(tipoB)^2 + sd(tipoC)^2 + sd(tipoD)^2 + sd(tipoE)^2 + sd(tipoF)^2) / I
F <- CMTr / CME #estadistico
F
#6.3
ing <- c(11, 14, 7, 15, 11, 13, 11, 16, 10, 15, 18, 12, 9, 9, 10, 10, 15, 10, 14, 10, 10, 12, 14, 12, 15, 7, 13, 6, 10, 15, 20, 10, 13, 10, 6, 14, 8, 10, 8, 11)
der <- c(13, 10, 12, 7, 5, 10, 10, 16, 9, 7, 7, 2, 6, 9, 9, 8, 8, 10, 3, 6, 5, 2, 9, 3, 4, 5, 10, 8, 5, 9, 10, 8, 13, 10, 0, 2, 1, 1, 0, 4)
med <- c(6, 7, 3, 5, 9, 6, 1, 6, 0, 2, 5, 6, 11, 6, 7, 0, 5, 7, 5, 4, 7, 4, 2, 8, 9, 6, 1, 4, 7, 7, 8, 9, 7, 5, 1, 6, 9, 4, 7, 6)

grupos <- c(rep("ing", length(ing)), rep("der", length(der)), rep("med", length(med)))
summary(aov((c(ing, der, med)~grupos)))
boxplot(c(ing, der, med) ~ grupos)
shapiro.test(ing) #si es mayor a 0.05 asumo normalidad
shapiro.test(med)
shapiro.test(der)
fligner.test(c(ing,med,der)~grupos)
# La H0 es que todas las varianzas son iguales:
# se espera que el p.value>0.05 para indicar que no hay diferencias significativas en las varianzas
# En nuestro caso p.value 0.04064. No es mayor pero se puede considerar "cercano a" y, por tanto aceptar H0.
I <- 3 # cantidad de grupos
J <- 40 # cantidad de observaciones en cada grupo
mediatotal <- mean(c(ing, der, med))
CMTr <- (J/(I-1))*((mean(ing) - mediatotal)^2 + (mean(der) - mediatotal)^2 + (mean(med) - mediatotal)^2)
CME <- (sd(ing)^2 + sd(der)^2 + sd(med)^2) / 3
F <- CMTr / CME #estadistico
p.value <- 1-pf(F,2,117)
p.value
CMTr
CME
#grupos = g_lib+1=3  I(obs-1)=117 ->obs =40
f.critico <- qf(1-0.05,2,117)
F > f.critico

#6.4
gr1<-c(299, 313, 300, 321, 308, 312, 300, 310, 281, 308, 309, 300, 303, 303, 311, 308, 291, 298, 276, 290, 310,
       308, 295, 310, 286, 295, 289, 293, 291, 297, 297, 287, 297, 302, 298, 301, 313, 290, 306, 313, 294, 308, 295,
       303, 316, 299, 313, 296, 290, 299)
gr2<-c(252, 248, 232, 229, 256, 233, 240, 237, 248, 232, 230, 246, 236, 250, 238, 243, 245, 241, 235, 249,
       238, 231, 230, 239, 261, 243, 242, 245, 249, 258, 245, 236, 244, 242, 229, 246, 244, 244, 255, 247, 236, 252,
       237, 259, 248, 237, 236, 252, 236, 239)
gr3<-c(272, 268, 285, 274, 278, 287, 297, 275, 269, 281, 270, 284, 282, 281, 280, 286, 265, 283, 281, 272,
       269, 286, 268, 288, 284, 282, 304, 280, 283, 281, 281, 286, 287, 288, 278, 272, 268, 287, 269, 272, 270, 271,
       291, 265, 280, 280, 275, 294, 269, 277)
grupos <- c(rep("gr1", length(gr1)), rep("gr2", length(gr2)), rep("gr3", length(gr3)))
summary(aov((c(gr1, gr2, gr3)~grupos)))
qf(1-0.05,2,147)
boxplot(c(gr1,gr2,gr3)~grupos)
pvalue <- 1-pf(558,2,147)
#p.value<0.05 -> rechazo HO las varianzas no son iguales
#6.5
# a partir de 0.0364
#6.6
resultados <- c( 1.4,2.1,3.0,2.1,3.2,4.7,3.5,4.5,5.4 )
grupos <- factor( c( 1,1,1,2,2,2,3,3,3 ))
summary(aov(resultados ~ grupos))
SCE <-7.936
SCI<-6.5
eta <-SCE/(SCI+SCE)

#6.7
I <- 5
J <- 4
CMTr <- 2673.3
CME <- 1094.2
F <- CMTr / CME #estadistico
f.critico<-qf(1-0.05,I-1,I*(J-1))
F>=f.critico
# no puedo revhazar HO
p.value<- 1 - pf(F,I-1,I*(J-1))
p.value>= 0.05
# Como el p-value es mayor al nivel de significancia, concluimos en que no contamos con suficiente evidencia
# estadistica para rechazar la hipotesis nula.
#6.11
Azul<-c(16, 11, 20, 21, 14, 7)
Verde<-c(37, 32, 15, 25, 39, 41)
Blanco<-c(21, 12, 14, 17, 13, 17)
Amarillo<-c(45, 59, 48, 46, 38, 47)
grupos <- c(rep("Azul", length(Azul)), rep("Verde", length(Verde)), rep("Blanco", length(Blanco)),rep("Amarillo",length(Amarillo)))
boxplot(c(Azul,Verde,Blanco,Amarillo)~grupos)
summary(aov(c(Azul,Verde,Blanco,Amarillo)~grupos))
30.55>=qf(1-0.05,3,20)
#rechazo HO hay colores mas atractivos para los insectos
#6.12
#el test b no es valido
# el c es valido
#las medias son parecidas
CME <- 23.39
#sigma^2*3/3=23.39
#6.14
I <- 4
J <- 6
SCE <- 12.548
SCI <- 1.147
CMTr <- SCE / (I-1)
CME <- SCI / (I * (J - 1))
F <- CMTr / CME
f.critico<- qf(1-0.05,I-1,J*(I-1))
F>=f.critico
#rechazo HO
p.value <- pf(1.403,2,18)
1-p.value

