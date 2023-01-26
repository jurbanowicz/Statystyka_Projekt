library(smoof)
library("GA")


ga_result <- function(func, lbound, ubound){
  result <- ga(type = "real-valued", fitness =function(x) - func(x), maxiter = 100, monitor = FALSE, lower = c(th = lbound), upper = ubound)
  return(result@fitnessValue*(-1))
};


# Ackley
n <- 2
ackley <- makeAckleyFunction(c(n))
res <- replicate(50, ga_result(ackley, rep(0, n), rep(10,n)))
res
mean(res)
hist(res, main = paste("Ackley  histogram for ", n, " dimensions"))
boxplot(res, main = paste("Ackley boxplot for ", n, " dimensions"))


# Rastigin
n <- 2
rastigin <- makeRastriginFunction(c(n))
result <- replicate(50, ga_result(rastigin, rep(0, n), rep(10,n)))
result
mean(result)
hist(result, main = paste("Rastigin  histogram for ", n, " dimensions"))
boxplot(result, main = paste("Rastigin boxplot for ", n, " dimensions"))

