x1<-63
m<-300
`p1^`<-x1/m
p1<-0.2

x2<-75
n<-180
`p2^`<-x2/n
p2<-0.4

`p^`<-(x1+x2)/(m+n)
`q^`<-(1-`p^`)
alpha<- 0.05/2

zp<-(`p1^`-`p2^`)/sqrt(`p^`*`q^`*(1/m+1/n))
zp
colaizqP<- qt(alpha,g_lib)
coladerP<-qt(alpha,g_lib,lower.tail = FALSE)



probizq<-pnorm((colaizqP*sqrt(`p^`*`q^`*(1/m+1/n))-(p1-p2))/sqrt(`p^`*`q^`*(1/m+1/n)))
probizq
probder<-pnorm((coladerP*sqrt(`p^`*`q^`*(1/m+1/n))-(p1-p2))/sqrt(`p^`*`q^`*(1/m+1/n)))
probder

probder-probizq

colader <- coladerP
colaizq<- colaizqP
x1 <- seq(-5, 5, length = 500)
y1 <- dnorm(x1, mean = 0, sd = 1)
plot(x1, y1, col = "red", type = "l", ylim = c(0, 0.5))
grid()
arrows(zp, 0.1, zp, 0)
text(zp, 0.12, "z real")
arrows(colaizq,0.1,colaizq,0)
text(colaizq,0.12,"cola izq")
arrows(colader,0.1,colader,0)
text(colader,0.12,"cola der")


delta0 <- -1
alpha <- 0.01

xraya <- 0.64
s1 <- 0.200
m <- 10

yraya <- 2.05
s2 <- 0.4
n <- 10


g_lib <- trunc((s1^2/m+s2^2/n)^2/(((s1^2/m)^2)/(m-1)+((s2^2/n)^2)/(n-1)))
g_lib
z <- (xraya-yraya-delta0)/sqrt(s1^2/m+s2^2/n)
z

p.value <- pt(abs(z),g_lib,lower.tail = FALSE)
colaizqT <- qt(alpha, g_lib)
colaizqT
coladerT <- qt(alpha, g_lib,lower.tail = FALSE)
coladerT

p.value <- pnorm(abs(z),lower.tail = FALSE)
colaizq <- qnorm(alpha)
colaizq
colader <- qnorm(alpha,lower.tail = FALSE)
colader


colader <- coladerT
colaizq<- colaizqT

x1 <- seq(-5, 5, length = 500)
y1 <- dnorm(x1, mean = 0, sd = 1)
plot(x1, y1, col = "red", type = "l", ylim = c(0, 0.5))
grid()
arrows(z, 0.1, z, 0)
text(z, 0.12, "z real")
arrows(colaizq,0.1,colaizq,0)
text(colaizq,0.12,"cola izq")
arrows(colader,0.1,colader,0)
text(colader,0.12,"cola der")

