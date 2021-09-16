### Simulate river temperature in relation to discharge with random intercept and slope for each river
source("header.R")

nriver <- 9
# generate samples per river
n <- sample(x = 50:70, size = nriver, replace = TRUE)

sTemperature <- 1 # sd of overall mu
bSlope <- -0.1 # typical effect of discharge
bIntercept <- 25 # typical intercept

# generate random slopes + intercepts
sSlopeRiver <- 0.05 # sd of random effect
sInterceptRiver <- 1.5 # sd of random intercept
bSlopeRiver <- rnorm(nriver, bSlope, sSlopeRiver)
bInterceptRiver <- rnorm(nriver, bIntercept, sInterceptRiver)

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
# naming convention for overall sigma?

