raw_data = read.csv("Thomas.csv",skip=4)
library(moveHMM)

data1 <- prepData(raw_data,type="LL", coordNames = c("X","Y"))

plot(data1)

mods <- list()
for (N in 2:5) {
  stepMean <- seq(10, 300, length = N)
  stepSD <- seq(10, 100, length = N) 
  zeroMass <- rep(0, N)
  
  stepPar <- c(stepMean, stepSD, zeroMass)
  mods[[N]] <- fitHMM(data1, nbStates = N, stepPar0 = stepPar, verbose = 2, 
                      angleDist = "none", stationary = TRUE)
}


mods <- list()
for (N in 2:5) {
  stepMean <- sort(runif(N, 80, 150))   # Updated smaller range for step means
  stepSD <- sort(runif(N, 5, 30))       # Updated smaller range for step SDs
  zeroMass <- rep(0.01, N)              # Small non-zero values for zero-mass
  stepPar <- c(stepMean, stepSD, zeroMass)

  mods[[N]] <- fitHMM(data1, nbStates = N, stepPar0 = stepPar, verbose = 2, 
                      angleDist = "none", stationary = TRUE)
}


