alphaIC <- 0.05
xraya <- 40.3
s1 <- 11.3
m <- 6
yraya <- 21.4
s2 <- 8.3
n <- 8



g_lib <- trunc((s1^2/m+s2^2/n)^2/(((s1^2/m)^2)/(m-1)+((s2^2/n)^2)/(n-1)))
g_lib
icizqT <- xraya-yraya+qt(alphaIC/2, g_lib)*sqrt(s1^2/m+s2^2/n)
icizqT
icderT <- xraya-yraya+ qt(alphaIC/2, g_lib,lower.tail = FALSE)*sqrt(s1^2/m +s2^2/n)
icderT

icizqZ <- xraya-yraya+qnorm(alphaIC/2)*sqrt(s1^2/m+s2^2/n)
icizqZ
icderZ <- xraya-yraya+ qnorm(alphaIC/2,lower.tail = FALSE)*sqrt(s1^2/m +s2^2/n)
icderZ

x1<-350
m<-1500
`p1^`<-x1/m
p1<-0.2

x2<-320
n<-2000
`p2^`<-x2/n
p2<-0.4

`p^`<-(x1+x2)/(m+n)
`q^`<-(1-`p^`)
delta0 <- p1-p2
alpha<- 0.1

colaizqP<- qnorm(alpha)
coladerP<-qnorm(alpha,lower.tail = FALSE)

intervaloizq <- `p1^`- `p2^` + colaizqP*sqrt(`p1^`*(1-`p1^`)/m +  `p2^` *(1-`p2^`)/n )
intervaloizq

intervaloizq <- `p1^`- `p2^` + coladerP*sqrt(`p1^`*(1-`p1^`)/m +  `p2^` *(1-`p2^`)/n )
intervaloizq


x <- c(57.9, 66.2, 65.4, 65.4, 65.2, 62.6, 67.6, 63.7, 67.2, 71)
y <- c(66.4, 71.7, 70.3, 69.3, 64.8, 69.6, 68.6, 69.4, 65.3, 68.8)
s1 <- sd(x)
s2 <-sd(y)
alpha <- 0.05
m<-10
n<-10
limiteizq<- s1^2/s2^2*qf(alpha/2,m-1,n-1)
limiteder<- s1^2/s2^2*qf(1-alpha/2,m-1,n-1)

