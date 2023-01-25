library(smoof)

prs <- function(func, lbound, upbound, dims) {
  best = Inf
  for (i in (1:1000)) {
    v <- runif(dims, lbound, upbound)
    current <- func(v)
    if (current < best) {
      best <- current
    }
  }
  return (best)
};


# Ackley
n <- 2
ackley <- makeAckleyFunction(c(n))
result <- replicate(100, prs(ackley, rep(1, n), rep(100, n), n))
result
mean(result)
hist(result, main = paste("Ackley  histogram for ", n, " dimensions"))
boxplot(res, main = paste("Ackley boxplot for ", n, " dimensions"))


# Rastrigin
n <- 20
rastigin <- makeRastriginFunction(c(n))
result <- replicate(100, prs(rastigin, rep(1, n), rep(100, n), n))
result
mean(result)
hist(result, main = paste("Rastigin  histogram for ", n, " dimensions"))
boxplot(result, main = paste("Rastigin boxplot for ", n, " dimensions"))

