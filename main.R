library(smoof)
library(GA)

# create the functions to use minimizing functions on
ackley2d <- makeAckleyFunction(c(2))
ackley10d <- makeAckleyFunction(c(10))
ackley20d <- makeAckleyFunction(c(20))

michalewicz2d <- makeMichalewiczFunction(c(2))
michalewicz10d <- makeMichalewiczFunction(c(10))
michalewicz20d <- makeMichalewiczFunction(c(20))

ack2d_prs <- replicate(50, prs(ackley2d, rep(-32, 2), rep(32, 2), 2))
ack10d_prs <- replicate(50, prs(ackley10d, rep(-32, 10), rep(32, 10), 10))
ack20d_prs <- replicate(50, prs(ackley20d, rep(-32, 20), rep(32, 20), 20))

ack2d_ga <- replicate(50, ga_result(ackley2d, rep(-32, 2), rep(32, 2)))
ack10d_ga <- replicate(50, ga_result(ackley10d, rep(-32, 10), rep(32, 10)))
ack20d_ga <- replicate(50, ga_result(ackley20d, rep(-32, 20), rep(32, 20)))

mean(ack2d_prs)
mean(ack2d_ga)

