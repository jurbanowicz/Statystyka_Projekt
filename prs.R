library(smoof)
library("GA")

ackley2d <- makeAckleyFunction(c(2))
ackley10d <- makeAckleyFunction(c(10))
ackley20d <- makeAckleyFunction(c(20))


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


result <- replicate(100, prs(ackley10d, rep(1, 10), rep(100, 10), 10))
result
mean(result)


