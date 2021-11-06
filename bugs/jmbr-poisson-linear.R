### Simulate count of fish by lake size 
source("header.R")

area <- runif(100, 0, 20)
bArea <- 0.15
bIntercept <- exp(0)
x <- exp(bIntercept + bArea*area)
count <- rpois(n = 100, lambda = x)

data <- data.frame(Area = area,
                   Count = count)
ggplot(data = data, aes(x = Area, y = Count)) +
  geom_point()

model <- model("model {

  bIntercept ~ dnorm(1, 5^-2)
  bArea ~ dnorm(1, 5^-2)
  
  for(i in 1:nObs) {
    log(eCount[i]) <- bIntercept + bArea * Area[i]  
    Count[i] ~ dpois(eCount[i])
  }
}", new_expr = "
  for(i in 1:nObs) {
    log(eCount[i]) <- bIntercept + bArea * Area[i]  
    prediction[i] <- eCount[i]
  }
", nthin = 10L, select_data = list(Area = c(0, 1000),
                                   Count = c(0L, 100000L)))

analysis <- analyse(model, data = data)
coef(analysis, simplify = TRUE)
prediction <- predict(analysis, new_data = c("Area")) 

gp <- ggplot(data = prediction, aes(x = Area, y = estimate)) +
  geom_point(data = data, aes(y = Count)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL
print(gp)

