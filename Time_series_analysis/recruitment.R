# Modelling AR(2) process on rec data
library(astsa)
source('yule_walker.r')

# from PACF p=2
par(mfrow=c(3,1))
plot(rec)
acf(rec)
pacf(rec)

phi = yule_walker(rec, p=2)$phi
var = yule_walker(rec, p=2)$var
cons = yule_walker(rec, p=2)$cons
#> phi
#             [,1]
#  [1,]  1.3315874
#  [2,] -0.4445447
#> var
#[1] 94.17131
#> cons
#[1] 7.033036