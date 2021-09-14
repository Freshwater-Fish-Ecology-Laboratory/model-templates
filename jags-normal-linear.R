### Simulate river temperature in relation to discharge
library(jmbr)
library(ggplot2)

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

model <- model("model {

  bIntercept ~ dnorm(0, 5^-2)
  bDischarge ~ dnorm(0, 5^-2)
  bSigma ~ dnorm(4, 10^-2) T(0,)
  
  for(i in 1:nObs) {
    eTemperature[i] <- bIntercept + bDischarge * Discharge[i]  
    Temperature[i] ~ dnorm(eTemperature[i], bSigma^-2)
  }
}", new_expr = "
  for(i in 1:nObs) {
    eTemperature[i] <- bIntercept + bDischarge * Discharge[i]  
    prediction[i] <- eTemperature[i]
  }
", nthin = 10L, select_data = list(Discharge = c(0, 1000),
                                   Temperature = c(-10, 40)))

analysis <- analyse(model, data = data)
coef(analysis, simplify = TRUE)
prediction <- predict(analysis, new_data = c("Discharge")) 

gp <- ggplot(data = prediction, aes(x = Discharge, y = estimate)) +
  geom_point(data = data, aes(y = Temperature)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL
print(gp)

