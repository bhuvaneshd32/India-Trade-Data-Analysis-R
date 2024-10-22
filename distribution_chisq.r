library(dplyr)
#main data
export<-X2018_2010_export
import<-X2018_2010_import

export$value[is.na(export$value)] = 0
export$value[is.nan(export$value)] = 0

v = export$value
n = length(v)

#qqnorm(v)
#qqline(v)

exponential <- function(x, lambda) {
  fx = lambda * exp(-lambda * x)
  return(fx)
}

library(MASS)

fit = fitdistr(v,"exponential")
lambda = unname(fit$estimate)
paste("Ho : Data follows Exponential distribution with lambda = ",lambda)
paste("Ha : Data does not follow Exponential distribution with lambda = ",lambda)

breaks = seq(0, 10, by = 0.01)
data_g = cut(v, breaks = breaks)

observed = table(data_g)
observed = as.vector(observed)

expected = exponential(breaks, lambda) * n

chisq = 0
for (i in 1:1000) {
  chisq = chisq + (observed[i] - expected[i]) ** 2 / expected[i]
}

p_value = 1 - pchisq(chisq, df = 1000)

if(p_value < 0.05) {
  print("Reject Ho")
  print("Data does not follow Exponential distribution")
} else {
  print("Accept Ho")
  print("Data follows Exponential distribution")
}
