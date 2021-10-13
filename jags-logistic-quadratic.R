### Simulate detection efficiency change over distance of telemetry receiver
source("header.R")

distance <- runif(100, 0, 1000)
bDistance <- 0.03
bDistance2 <- -0.00009
bIntercept <- -2
x <- bIntercept + bDistance*distance + bDistance2*distance^2

p <- 1/(1 + exp(-x))
pings <- round(runif(100, 50, 60))
detects <- rbinom(100, size = pings, p = p)

data <- data.frame(Distance = distance, 
                   Pings = as.integer(pings), 
                   Detects = as.integer(detects))
ggplot(data = data, aes(x = Distance, y = Detects/Pings)) +
  geom_point()

model <- model("model {

  bIntercept ~ dnorm(0, 5^-2)
  bDistance ~ dnorm(0, 5^-2)
  bDistance2 ~ dnorm(0, 5^-2)

  for(i in 1:nObs) {
    logit(eDetects[i]) <- bIntercept + bDistance * Distance[i] + bDistance2 * Distance[i]^2  
    Detects[i] ~ dbin(eDetects[i], Pings[i])
  }
}", new_expr = "
  for(i in 1:nObs) {
    logit(eDetects[i]) <- bIntercept + bDistance * Distance[i] + bDistance2 * Distance[i]^2  
    prediction[i] <- eDetects[i]
  }
", nthin = 5L, select_data = list(Detects = c(0L, 10000L), 
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


