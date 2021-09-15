### Simulate river temperature in relation to discharge
# River is random effect with correlated random intercept and slope 

rm(list = ls())
library(jmbr)
library(ggplot2)
library(MASS)

set.seed(100)

nriver <- 10
# generate samples per river
n <- sample(x = 50:70, size = nriver, replace = TRUE)

sTemperature <- 1 # sd of overall mu
bSlope <- -0.1 # typical effect of discharge
bIntercept <- 25 # typical intercept

# generate (negatively) correlated random slopes + intercepts drawn from bivariate normal distribution
# we are saying that as the intercept goes up the effect gets more extreme
sSlopeRiver <- 0.05 # sd of random slope
sInterceptRiver <- 1.5 # sd of random intercept
rho <- -0.9
covMat <- matrix(c(sInterceptRiver^2, rho*sInterceptRiver*sSlopeRiver,
                   rho*sInterceptRiver*sSlopeRiver, sSlopeRiver^2), 2, 2)
bMat <- mvrnorm(n = nriver, mu = c(bIntercept, bSlope), Sigma = covMat)
bInterceptRiver <- bMat[,1]
bSlopeRiver <- bMat[,2]

# show correlation of slope + intercept
bdf <- as.data.frame(bMat) %>% setNames(c("Intercept", "Slope"))
ggplot(data = bdf, aes(x = Intercept, y = Slope)) +
  geom_point()

library(purrr)
data <- map_df(seq_len(nriver), function(i){
  discharge <- runif(n[i], 0, 50)
  mu <- bInterceptRiver[i] + bSlopeRiver[i]*discharge
  temp <- rnorm(n = n[i], mean = mu, sd = sTemperature)
  data.frame(River = factor(i), 
             Temperature = temp,
             Discharge = discharge)
})

ggplot(data = data, aes(x = Discharge, y = Temperature, color = River)) +
  geom_point()

model <- model("model {

  bIntercept ~ dnorm(20, 5^-2)
  bSlope ~ dnorm(0, 5^-2)
  bSigma ~ dnorm(1, 5^-2) T(0,)
  
  sInterceptRiver ~ dnorm(0, 5^-2) T(0,)
  sSlopeRiver ~ dnorm(0, 5^-2) T(0,)
  rho ~ dnorm(0, 0.5^-2) T(-1,1)
  
  eCovMat[1,1] <- sInterceptRiver^2
  eCovMat[2,2] <- sSlopeRiver^2
  eCovMat[1,2] <- rho*sInterceptRiver*sSlopeRiver
  eCovMat[2,1] <- eCovMat[1,2]
  eInvCovMat[1:2,1:2] <- inverse(eCovMat[,])
  
  for(i in 1:nRiver) {
    eBhat[i,1] <- 0
    eBhat[i,2] <- 0
    eB[i, 1:2] ~ dmnorm(eBhat[i,], eInvCovMat[,])
    bInterceptRiver[i] <- eB[i,1]
    bSlopeRiver[i] <- eB[i,2]
  }
  
  for(i in 1:nObs) {
    eIntercept[i] <- bIntercept + bInterceptRiver[River[i]]
    eSlope[i] <- bSlope + bSlopeRiver[River[i]]
    eTemperature[i] <- eIntercept[i] + eSlope[i] * Discharge[i]  
    Temperature[i] ~ dnorm(eTemperature[i], bSigma^-2)
  }
}", new_expr = "
  for(i in 1:nObs) {
    eIntercept[i] <- bIntercept + bInterceptRiver[River[i]]
    eSlope[i] <- bSlope + bSlopeRiver[River[i]]
    eTemperature[i] <- eIntercept[i] + eSlope[i] * Discharge[i]  
    prediction[i] <- eTemperature[i]
  }
", nthin = 10L, select_data = list(Discharge = c(0, 1000),
                                   Temperature = c(-10, 50),
                                   River = factor()), 
               random_effects = list(bInterceptRiver = "River",
                                     bSlopeRiver = "River"))

analysis <- analyse(model, data = data)
plot(analysis)
print(coef(analysis, simplify = TRUE), n = 100)
prediction <- predict(analysis, new_data = c("Discharge", "River")) 

gp <- ggplot(data = prediction, aes(x = Discharge, y = estimate, 
                                    group = River, color = River)) +
  geom_point(data = data, aes(y = Temperature)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL
print(gp)

### how does model without bivariate normal perform?
model <- model("model {

  bIntercept ~ dnorm(20, 5^-2)
  bSlope ~ dnorm(0, 5^-2)
  bSigma ~ dnorm(1, 5^-2) T(0,)
  
  sInterceptRiver ~ dnorm(1, 5^-2) T(0,)
  sSlopeRiver ~ dnorm(1, 5^-2) T(0,)
  
  for(i in 1:nRiver){
    bInterceptRiver[i] ~ dnorm(0, sInterceptRiver^-2)
    bSlopeRiver[i] ~ dnorm(0, sSlopeRiver^-2)
  }
  
  for(i in 1:nObs) {
    eIntercept[i] <- bIntercept + bInterceptRiver[River[i]]
    eSlope[i] <- bSlope + bSlopeRiver[River[i]]
    eTemperature[i] <- eIntercept[i] + eSlope[i] * Discharge[i]  
    Temperature[i] ~ dnorm(eTemperature[i], bSigma^-2)
  }
}", new_expr = "
  for(i in 1:nObs) {
    eIntercept[i] <- bIntercept + bInterceptRiver[River[i]]
    eSlope[i] <- bSlope + bSlopeRiver[River[i]]
    eTemperature[i] <- eIntercept[i] + eSlope[i] * Discharge[i]  
    prediction[i] <- eTemperature[i]
  }
", nthin = 10L, select_data = list(Discharge = c(0, 1000),
                                   Temperature = c(-10, 50),
                                   River = factor()), 
               random_effects = list(bInterceptRiver = "River",
                                     bSlopeRiver = "River"))

analysis <- analyse(model, data = data)
print(coef(analysis, simplify = TRUE), n = 100)
prediction <- predict(analysis, new_data = c("Discharge", "River")) 

gp <- ggplot(data = prediction, aes(x = Discharge, y = estimate, 
                                    group = River, color = River)) +
  geom_point(data = data, aes(y = Temperature)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL
print(gp)

### questions
# why is it that model without bivariate normal does pretty much equivalent job at estimating?
# naming conventions for variance/covariance matrix etc.?
# why not very good at estimating rho?



