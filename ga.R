library(smoof)
library("GA")

ackley3d <- makeAckleyFunction(c(3))
ackley2d <- makeAckleyFunction(c(2))
ackley10d <- makeAckleyFunction(c(10))
ackley20d <- makeAckleyFunction(c(20))
ackley <- makeAckleyFunction(c(1))

michalewicz2d <- makeMichalewiczFunction(c(2));
rastrigin2d <- makeRastriginFunction(c(2));
rastrigin3d <- makeRastriginFunction(c(3));



ga_result <- function(func, lbound, ubound){
  result <- ga(type = "real-valued", fitness =function(x) - func(x), maxiter = 1000, monitor = FALSE, lower = c(th = lbound), upper = ubound)
  return(result@fitnessValue*(-1))
};

# 2 dims
res <- replicate(50, ga_result(ackley2d, rep(-10, 2), rep(10,2)))
res
mean(res)



# 10 dimensions
res <- replicate(50, ga_result(ackley10d, rep(1, 10), rep(10,10)))
res
mean(res)



