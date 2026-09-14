library(yuima)
## You always need the parameters alpha, beta and gamma
## Additionally e.g. time.points
data <- simCIR(alpha=3,beta=1,gamma=1,
               time.points = c(0,0.1,0.2,0.25,0.3))
plot(data[1,],data[2,], type="l",col=4)
## or n, number of observations, h, distance between observations,
## and equi.dist=TRUE
data <- simCIR(alpha=3,beta=1,gamma=1,n=1000,h=0.1,equi.dist=TRUE)
plot(data[1,],data[2,], type="l",col=4)

## If you input every value and equi.dist=TRUE, time.points are not
## used for the simulations.

data <- simCIR(alpha=3,beta=1,gamma=1,n=1000,h=0.1,
               time.points = c(0,0.1,0.2,0.25,0.3),
               equi.dist=TRUE)
plot(data[1,],data[2,], type="l",col=4)

## If you leave equi.dist=FALSE, the parameters n and h are not
## used for the simulation.
data <- simCIR(alpha=3,beta=1,gamma=1,n=1000,h=0.1,
               time.points = c(0,0.1,0.2,0.25,0.3))
plot(data[1,],data[2,], type="l",col=4)
