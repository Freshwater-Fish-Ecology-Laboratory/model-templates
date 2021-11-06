### Simulate river temperature in relation to discharge
source("header.R")

discharge <- runif(100, 0, 50)
bDischarge <- -0.2
bIntercept <- 25
bSigma <- 2
x <- bIntercept + bDischarge*discharge 
temperature <- rnorm(100, mean = x, sd = bSigma)

data <- data.frame(Discharge = discharge,
                   Temperature = temperature)

ggplot(data = data, aes(x = Discharge, y = Temperature)) +
  geom_point()

code <- nimble::nimbleCode({
  bIntercept ~ dnorm(0, 1000)
  bDischarge ~ dnorm(0, 1000)
  bSigma ~ dunif(0, 100)
  
  for(i in 1:nObs) {
    eTemperature[i] <- bIntercept + bDischarge * Discharge[i]  
    Temperature[i] ~ dnorm(eTemperature[i], bSigma)
  }
})

cmodel <- nimbleModel(code, 
                      constants = list(nObs = 100), 
                      # inits = list(bIntercept = 25,
                      #              bDischarge = -0.2,
                      #              bSigma = 0.5),
                      data = list(Discharge = runif(100, 0, 50),
                                  Temperature = data$Temperature)) %>%
  compileNimble()

nodes <- cmodel$getDependencies(c("bIntercept", "bDischarge", "bSigma"), 
                                   self = FALSE, downstream = TRUE)
cmodel$simulate(nodes)

### plot simulated temperatures
data <- data.frame(Discharge = cmodel$Discharge, 
                   Temperature = cmodel$Temperature,
                   eTemperature = cmodel$eTemperature)

ggplot(data = data, aes(x = Discharge, y = Temperature)) +
  geom_point()

cmodel$setData(list(Temperature = cmodel$Temperature,
                    Discharge = cmodel$Discharge))

sim_mcmc <- buildMCMC(cmodel) %>% compileNimble(project = cmodel)
samples <- runMCMC(sim_mcmc, niter = 50000, nburnin = 5000)
plot(samples[ , 'bIntercept'], type = 'l', xlab = 'iteration',  ylab = "bIntercept")
