library(smoof)
library(GA)

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

ga_result <- function(func, lbound, ubound){
  result <- ga(type = "real-valued", fitness =function(x) -func(x), maxiter = 1000, monitor = FALSE, lower = c(th = lbound), upper = ubound)
  return(result@fitnessValue*(-1))
};

# create the functions to use minimizing functions on
ackley2d <- makeAckleyFunction(c(2))
ackley10d <- makeAckleyFunction(c(10))
ackley20d <- makeAckleyFunction(c(20))

rastrigin2d <- makeRastriginFunction(c(2))
rastrigin10d <- makeRastriginFunction(c(10))
rastrigin20d <- makeRastriginFunction(c(20))

getParamSet(ackley2d)
getParamSet(rastrigin2d)
getGlobalOptimum(ackley2d)
getGlobalOptimum(rastrigin2d)

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
ras20d_ga <- replicate(50, ga_result(rastrigin20d, rep(-5, 20), rep(5, 20)))

# Boxplots and histograms for each of teh results
run_comparison <- function() {
hist(ack2d_prs, main = paste("Ackley histogram for ", 2, " dimensions: prs"))
boxplot(ack2d_prs, main = paste("Ackley boxplot for ", 2, " dimensions: prs"))

hist(ack2d_ga, main = paste("Ackley histogram for ", 2, " dimensions: ga"))
boxplot(ack2d_ga, main = paste("Ackley boxplot for ", 2, " dimensions: ga"))

hist(ack10d_prs, main = paste("Ackley histogram for ", 10, " dimensions: prs"))
boxplot(ack10d_prs, main = paste("Ackley boxplot for ", 10, " dimensions: prs"))

hist(ack10d_ga, main = paste("Ackleyhistogram for ", 10, " dimensions: ga"))
boxplot(ack10d_ga, main = paste("Ackley boxplot for ", 10, " dimensions: ga"))

hist(ack20d_prs, main = paste("Ackley histogram for ", 20, " dimensions: prs"))
boxplot(ack20d_prs, main = paste("Ackley boxplot for ", 20, " dimensions: prs"))

hist(ack20d_ga, main = paste("Ackley histogram for ", 20, " dimensions: ga"))
boxplot(ack20d_ga, main = paste("Ackley boxplot for ", 20, " dimensions using ga"))

hist(ras2d_prs, main = paste("Rastrigin histogram for ", 2, " dimensions: prs"))
boxplot(ras2d_prs, main = paste("Rastrigin boxplot for ", 2, " dimensions: prs"))

hist(ras2d_ga, main = paste("Rastrigin histogram for ", 2, " dimensions: ga"))
boxplot(ras2d_ga, main = paste("Rastrigin boxplot for ", 2, " dimensions: ga"))

hist(ras10d_prs, main = paste("Rastrigin histogram for ", 10, " dimensions: prs"))
boxplot(ras10d_prs, main = paste("Rastrigin boxplot for ", 10, " dimensions: prs"))

hist(ras10d_ga, main = paste("Rastrigin histogram for ", 10, " dimensions: ga"))
boxplot(ras10d_ga, main = paste("Rastrigin boxplot for ", 10, " dimensions: ga"))

hist(ras20d_prs, main = paste("Rastrigin histogram for ", 20, " dimensions: prs"))
boxplot(ras20d_prs, main = paste("Rastrigin boxplot for ", 20, " dimensions: prs"))

hist(ras20d_ga, main = paste("Rastrigin histogram for ", 20, " dimensions: ga"))
boxplot(ras20d_ga, main = paste("Rastrigin boxplot for ", 20, " dimensions: ga"))
}


# comparison between each of the optimalization method
t.test(ack2d_prs, ack2d_ga)
t.test(ack10d_prs, ack10d_ga)
t.test(ack20d_prs, ack20d_ga)

t.test(ras2d_prs, ras2d_ga)
t.test(ras10d_prs, ras10d_ga)
t.test(ras20d_prs, ras20d_ga)

t.test(ras2d_prs)
t.test(ras2d_ga)

# should add for all possibilities
library(ggplot2)
library(tibble)

data <- data_frame(method = c(rep("PRS2D",length(ack2d_prs)),rep("PRS10D",length(ack10d_prs))),value = c(ack2d_prs,ack10d_prs))
ggplot(data=data, aes(x= method, y=value)) +
  geom_boxplot()+
  ggtitle("Ackley")

data <- data_frame(method = c(rep("PRS2D",length(ack2d_prs)),rep("GA2D",length(ack2d_ga))),value = c(ack2d_prs,ack2d_ga))
ggplot(data=data, aes(x= method, y=value)) +
  geom_boxplot()+
  ggtitle("Ackley")




