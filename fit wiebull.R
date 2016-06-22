library("fitdistrplus")
data("groundbeef")
str(groundbeef)
fw <- fitdist(groundbeef$serving, "weibull")
summary(fw)

