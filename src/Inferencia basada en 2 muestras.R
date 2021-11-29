#inferecia de 2 proporciones
  #ingresar datos
x1<-8*4
m<-13*4
`p1^`<-x1/m
p1<-   #optional 

x2<-2*4
n<-8*4
`p2^`<-x2/n
p2<-   #optional

`p^`<-(x1+x2)/(m+n)
`p^`
`q^`<-(1-`p^`)
`q^`
delta0 <- 0 
alpha<- 0.05  #remember dividing by 2 if using double tail


zp<-((`p1^`-`p2^`)-delta0)/sqrt(`p^`*`q^`*(1/m+1/n))  # p obs estandarizada
zp

col
colaizqP<- qnorm(alpha)
colaizqP
coladerP<-qnorm(alpha,lower.tail = FALSE)
coladerP
p.value<-pnorm(abs(zp),lower.tail=FALSE)
p.value
#calcula de beta 
delta1<- -0.2   #delta prima 
probizq<-pnorm(colaizqP-(delta1-delta0)/sqrt(p1*(1-p1)/m+p2*(1-p2)/n))
probizq
probder<-pnorm((coladerP*sqrt(`p^`*`q^`*(1/m+1/n))-(p1-p2))/sqrt(`p^`*`q^`*(1/m+1/n)))
probder

probder-probizq

colader <- coladerP
colaizq<- colaizqP
  
#grafico
x1 <- seq(-5, 5, length = 500)
y1 <- dnorm(x1, mean = 0, sd = 1)
plot(x1, y1, col = "green", type = "l", ylim = c(0, 0.5))
grid()
arrows(zp, 0.1, zp, 0)
text(zp, 0.12, "z real")
arrows(colaizq,0.1,colaizq,0)
text(colaizq,0.12,"cola izq")
arrows(colader,0.1,colader,0)
text(colader,0.12,"cola der")


#data entry

denscoorr<-c(0.50,1,1.25,1.50,2,2.25,2.75,3.50,3.75)

presionefec<-c(74.7,86.1,92.3,132.9,202.4,223.6,365.3,382.3,447.2)

presionefec2<-c(85.4,90.3,105.1,148.5,205.7,225.1,406.7,435.2,447.2)


delta0 <-0
alpha <- 0.05/2

xraya <- 80.018
s1 <-0.0245
m <- 13

yraya <- 79.979
s2 <- 0.0314
n <- 8


g_lib <- trunc((s1^2/m+s2^2/n)^2/(((s1^2/m)^2)/(m-1)+((s2^2/n)^2)/(n-1))) #grados de libertad
g_lib
z <- (xraya-yraya-delta0)/sqrt(s1^2/m+ s2^2/n) #z observada estandarizada
z

# T de Student

p.value <- pt(abs(z),g_lib,lower.tail = FALSE)
p.value
colaizqT <- qt(alpha, g_lib)
colaizqT
coladerT <- qt(alpha, g_lib,lower.tail = FALSE)
coladerT

# normal

p.value <- pnorm(abs(z),lower.tail = FALSE)
p.value
colaizq <- qnorm(alpha)
colaizq
colader <- qnorm(alpha,lower.tail = FALSE)
colader


#calculo de beta
delta1<- -1.2
z.critico <- qnorm(alpha)
beta <- pnorm(z.critico - (delta1-delta0) / sqrt((s1^2 / m) + (s2^2 / n)))
beta

#graico

  # correr estas dos lineas si se utiliza T Student
colader <- coladerT
colaizq<- colaizqT

x1 <- seq(-5, 5, length = 500)
y1 <- dnorm(x1, mean = 0, sd = 1)
plot(x1, y1, col = "green", type = "l", ylim = c(0, 0.5))
grid()
arrows(z, 0.1, z, 0)
text(z, 0.12, "z real")
arrows(colaizq,0.1,colaizq,0)
text(colaizq,0.12,"cola izq")
arrows(colader,0.1,colader,0)
text(colader,0.12,"cola der")

# cociente de varianzas
s1<-0.0245
m<-13
s2<-0.0314
n<-8
f<- s1^2/s2^2
f
alpha =0.05/2
p.valueder <- pf(f,m-1,n-1,lower.tail = FALSE)
p.valueder
p.valueizq <-pf(f,m-1,n-1)
p.valueizq

fizq<-qf(alpha,m-1,n-1)
fizq

fder<-qf(1-alpha,m-1,n-1)
fder

colader <- fder
colaizq<- fizq

x1 <- seq(-5, 5, length = 500)
y1 <- dnorm(x1, mean = 0, sd = 1)
plot(x1, y1, col = "green", type = "l", ylim = c(0, 0.5))
grid()
arrows(f, 0.1, f, 0)
text(f, 0.12, "f real")
arrows(colaizq,0.1,colaizq,0)
text(colaizq,0.12,"cola izq")
arrows(colader,0.1,colader,0)
text(colader,0.12,"cola der")














