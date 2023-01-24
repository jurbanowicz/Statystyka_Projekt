prs2d <- function(func, lbound, upbound) {
  best = Inf
  for (i in (1:1000)) {
    v <- runif(2, lbound, upbound)
    current <- func(v)
    if (current < best) {
      best <- current
    }
  }
  return (best)
}

result <- replicate(100, prs2d(ackley2d, c(1, 1), c(100, 100)))
mean(result)