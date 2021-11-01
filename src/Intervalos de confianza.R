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

