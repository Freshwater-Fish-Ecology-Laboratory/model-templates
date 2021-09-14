### Simulate temperature by discharge and substrate
library(jmbr)
library(ggplot2)

discharge <- runif(100, 0, 50)
bDischarge <- -0.3
bIntercept <- 30
bSigma <- 2
bSubstrate <- c("rock" = 3, "sand" = -2, "veg" = 8)
substrate <- sample(names(bSubstrate), 100, replace = TRUE, prob = c(0.3, 0.6, 0.1))
x <- bIntercept + bDischarge*discharge + bSubstrate[substrate]
temperature <- rnorm(100, mean = x, sd = bSigma)

data <- data.frame(Discharge = discharge,
                   Temperature = temperature, 
                   Substrate = factor(substrate))

ggplot(data = data, aes(x = Discharge, y = Temperature, color = Substrate)) +
  geom_point()

model <- model("model {

  bIntercept ~ dnorm(0, 5^-2)
  bDischarge ~ dnorm(0, 5^-2)
  bSigma ~ dnorm(4, 10^-2) T(0,)
  
  bSubstrate[1] <- 0
  for(i in 2:nSubstrate){
    bSubstrate[i] ~ dnorm(0, 5^-2)
  }
  for(i in 1:nObs) {
    eTemperature[i] <- bIntercept + bDischarge * Discharge[i] + bSubstrate[Substrate[i]]
    Temperature[i] ~ dnorm(eTemperature[i], bSigma^-2)
  }
}", new_expr = "
  for(i in 1:nObs) {
    eTemperature[i] <- bIntercept + bDischarge * Discharge[i] + bSubstrate[Substrate[i]]
    prediction[i] <- eTemperature[i]
  }
", nthin = 10L, select_data = list(Discharge = c(0, 1000),
                                   Temperature = c(-10, 50),
                                   Substrate = factor()))

analysis <- analyse(model, data = data)
coef(analysis, simplify = TRUE)
prediction <- predict(analysis, new_data = c("Discharge")) 

gp <- ggplot(data = prediction, aes(x = Discharge, y = estimate, color = Substrate)) +
  geom_point(data = data, aes(y = Temperature)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL
print(gp)

