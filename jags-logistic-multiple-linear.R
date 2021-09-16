### simulate detection efficiency wth negative association to distance and positive association with discharge
source("header.R")

distance <- runif(100, 0, 1000)
discharge <- runif(100, 100, 300)
bDistance <- -0.02
bDischarge <- 0.04
bIntercept <- 4
x <- bIntercept + bDistance*distance + bDischarge*discharge
p <- 1/(1 + exp(-x))
        
summary(p)
pings <- round(runif(100, 30, 70))
detects <- rbinom(100, size = pings, p = p)

data <- data.frame(Distance = distance, 
                   Discharge = discharge,
                   Pings = as.integer(pings), 
                   Detects = as.integer(detects))

ggplot(data = data, aes(x = Distance, y = Detects/Pings)) +
  geom_point()
ggplot(data = data, aes(x = Discharge, y = Detects/Pings)) +
  geom_point()

model <- model("model {

  bIntercept ~ dnorm(4, 10^-2)
  bDistance ~ dnorm(0, 1^-2)
  bDischarge ~ dnorm(0, 1^-2)
  
  for(i in 1:nObs) {
    logit(eDetects[i]) <- bIntercept + bDistance * Distance[i] + bDischarge * Discharge[i]
    Detects[i] ~ dbin(eDetects[i], Pings[i])
  }
}", new_expr = "
  for(i in 1:nObs) {
    logit(eDetects[i]) <- bIntercept + bDistance * Distance[i] + bDischarge * Discharge[i]
    prediction[i] <- eDetects[i]
  }
", nthin = 10L, select_data = list(Detects = c(0L, 10000L), 
                                   Discharge = c(0, 500),
                                   Distance = c(0, 1000),
                                   Pings = c(1L, 10000L)))

analysis <- analyse(model, data = data)
coef(analysis, simplify = TRUE)
prediction <- predict(analysis, new_data = c("Distance")) 

gp <- ggplot(data = prediction, aes(x = Distance, y = estimate)) +
  geom_point(data = data, aes(y = Detects/Pings)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL
print(gp)

