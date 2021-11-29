

#data entry

alphaIC <- 0.05
xraya <- 115.7
s1 <- 5.03
m <- 6

yraya <- 129.3
s2 <- 5.38
n <- 6

# intervalo T Student

g_lib <- trunc((s1^2/m+s2^2/n)^2/(((s1^2/m)^2)/(m-1)+((s2^2/n)^2)/(n-1)))
g_lib
icizqT <- xraya-yraya+qt(alphaIC/2, g_lib)*sqrt(s1^2/m+s2^2/n)
icizqT
icderT <- xraya-yraya+ qt(alphaIC/2, g_lib,lower.tail = FALSE)*sqrt(s1^2/m +s2^2/n)
icderT

# intervalo normal

icizqZ <- xraya-yraya+qnorm(alphaIC/2)*sqrt(s1^2/m+s2^2/n)
icizqZ
icderZ <- xraya-yraya+ qnorm(alphaIC/2,lower.tail = FALSE)*sqrt(s1^2/m +s2^2/n)
icderZ

#intervalo para proporciones

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

#cociente de varianzas

s1 <- sqrt(50)
s2 <-sqrt(24)
alpha <- 0.1
m<-31
n<-25
limiteizq<- s1^2/s2^2*(1/qf(1-alpha/2,m-1,n-1))

limiteder<- s1^2/s2^2*(1/qf(alpha/2,m-1,n-1))

limiteizq
limiteder

#intervalo parcail
ysombrero1<- 222.9822196
n1<-9
s1<-33.62
ysombrero2<-246.193
n2<-9
s2<-43.04
alpha<-0.05
  
  
limiteizq<-ysombrero1-ysombrero2+qt(alpha/2,n1+n2-2)*sqrt(s1^2/n1+s2^2/n2)
limiteder<-ysombrero1-ysombrero2+qt(1-alpha/2,n1+n2-2)*sqrt(s1^2/n1+s2^2/n2)
limiteizq
limiteder
A<-c(80.05,80.04,80.04,80.04,80.03,80.03,80.02,80.02,80.01,80,80,79.98,79.97)
B<-c(80.03,80.02,79.98,79.97,79.97,79.97,79.95,79.94)
grupos <- c(rep("A", length(A)), rep("A", length(B)))
aov(c(A,B)~grupos)

