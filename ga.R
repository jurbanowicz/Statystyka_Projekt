library(smoof)
library("GA")

ackley3d <- makeAckleyFunction(c(3))
ackley2d <- makeAckleyFunction(c(2))
ackley10d <- makeAckleyFunction(c(10))
ackley20d <- makeAckleyFunction(c(20))
ackley <- makeAckleyFunction(c(1))

michalewicz2d <- makeMichalewiczFunction(c(2))
rastrigin2d <- makeRastriginFunction(c(2))
rastrigin3d <- makeRastriginFunction(c(3))


# 2 dimensions
lbound <- c(-100,-100)
ubound <- c(100,100)
result2d <- ga(type = "real-valued", fitness =function(x) - ackley2d(x), lower = c(th = lbound), upper = ubound)
summary(result)
result2d@fitnessValue

# 10 dimensions
lbound <- rep(-10, 10)
ubound <- rep(10, 10)
result10d <- ga(type = "real-valued", fitness =function(x) - ackley10d(x), maxiter = 1000, lower = c(th = lbound), upper = ubound)
summary(result10d)
result10d@fitnessValue

