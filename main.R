library(smoof)
library(GA)

# create the functions to use minimizing functions on
ackley2d <- makeAckleyFunction(c(2))
ackley10d <- makeAckleyFunction(c(10))
ackley20d <- makeAckleyFunction(c(20))

rastrigin2d <- makeRastriginFunction(c(2))
rastrigin10d <- makeRastriginFunction(c(10))
rastrigin20d <- makeRastriginFunction(c(20))

getParamSet(ackley2d)
getParamSet(rastrigin2d)

ack2d_prs <- replicate(50, prs(ackley2d, rep(-32, 2), rep(32, 2), 2))
ack10d_prs <- replicate(50, prs(ackley10d, rep(-32, 10), rep(32, 10), 10))
ack20d_prs <- replicate(50, prs(ackley20d, rep(-32, 20), rep(32, 20), 20))

ack2d_ga <- replicate(50, ga_result(ackley2d, rep(-32, 2), rep(32, 2)))
ack10d_ga <- replicate(50, ga_result(ackley10d, rep(-32, 10), rep(32, 10)))
ack20d_ga <- replicate(50, ga_result(ackley20d, rep(-32, 20), rep(32, 20)))

ras2d_prs <- replicate(50, prs(rastrigin2d, rep(-5, 2), rep(5, 2), 2))
ras10d_prs <- replicate(50, prs(rastrigin10d, rep(-5, 10), rep(5, 10), 10))
ras20d_prs <- replicate(50, prs(rastrigin20d, rep(-5, 20), rep(5, 20), 20))

ras2d_ga <- replicate(50, ga_result(rastrigin2d, rep(-5, 2), rep(5, 2)))
ras10d_ga <- replicate(50, ga_result(rastrigin10d, rep(-5, 10), rep(5, 10)))
ras20d_ga <- replciate(50, ga_result(rastrigin20d), rep(-5, 20), rep(5, 20))

# comparison between each of the optimalization method
t.test(ack2d_prs, ack2d_ga)
t.test(ack10d_prs, ack10d_ga)
t.test(ack20d_prs, ack20d_ga)

t.test(ras2d_prs, ras2d_ga)
t.test(ras10d_prs, ras10d_ga)
t.test(ras20d_prs, ras20d_ga)
