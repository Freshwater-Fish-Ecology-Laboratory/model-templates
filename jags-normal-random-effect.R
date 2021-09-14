### Simulate river temperature in relation to discharge with random intercept and slope for each river
rm(list = ls())
library(jmbr)
library(ggplot2)

nriver <- 9

# assign obs to rivers
n <- sample(x = 50:70, size = nriver, replace = TRUE)

discharge <- runif(200, 0, 50)

bSigma <- 1 # sd of overal mu
bSlope <- -0.12 # typical effect of discharge
bIntercept <- 25 # typical intercept
sSlope <- 0.04 # sd of random effect
sIntercept <- 1.4 # sd of random intercept

betas <- rnorm(nriver, bSlope, sSlope)
alphas <- rnorm(nriver, bIntercept, sIntercept)

library(purrr)
data <- map_df(seq_len(nriver), function(x){
  alpha <- alphas[x]
  beta <- betas[x]
  disch <- sample(discharge, size = n[x])
  mu <- alpha + beta*disch
  temp <- rnorm(n = n[x], mean = mu, sd = bSigma)
  data.frame(River = factor(x), 
             bIntercept = alpha,
             bSlope = beta,
             Temperature = temp,
             Discharge = disch)
})

ggplot(data = data, aes(x = Discharge, y = Temperature, color = River)) +
  geom_point()

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
# Is it normal to have very low svalues for random slopes/intercepts?
# the by-river slope and intercept coefficient values are differences from the mean, correct?

