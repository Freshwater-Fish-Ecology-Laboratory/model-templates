### this example is from Mcelreath chapter 6
# when trying to predict height from both left leg + right leg height (correlated) we run into problems
source("header.R")

set.seed(909)
n <- 100
# average height 170
height <- rnorm(n, 10, 2)
# leg proportion somewhere between 0.4 and 0.5
leg_prop <- runif(n, 0.4, 0.5)
# generate leg heights with some error
leg_left <- height*leg_prop + rnorm(n, 0, 0.02)
leg_right <- height*leg_prop + rnorm(n, 0, 0.02)

data <- data.frame(Height = height, Left = leg_left, Right = leg_right)

ggplot(data = data, aes(x = Right, y = Height)) + 
  geom_point()

ggplot(data = data, aes(x = Right, y = Left)) + 
  geom_point()

model <- model("model {

  bIntercept ~ dnorm(10, 100^-2)
  bRight~ dnorm(2, 10^-2)
  bSigma ~ dnorm(0, 10^-2) T(0,)
  
  for(i in 1:nObs) {
    eHeight[i] <- bIntercept + bRight * Right[i] 
    Height[i] ~ dnorm(eHeight[i], bSigma^-2)
  }
}", new_expr = "
  for(i in 1:nObs) {
    eHeight[i] <- bIntercept + bRight * Right[i] 
    prediction[i] <- eHeight[i]
  }
", nthin = 10L, select_data = list(Height = c(0, 20),
                                   Right = c(0, 20)))

analysis <- analyse(model, data = data)
coef <- coef(analysis, simplify = TRUE)
coef
plot_coef(coef)
prediction <- predict(analysis, new_data = c("Right")) 

gp <- ggplot(data = prediction, aes(x = Right, y = estimate)) +
  geom_point(data = data, aes(y = Height)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL
print(gp)

model <- model("model {

  bIntercept ~ dnorm(10, 100^-2)
  bRight ~ dnorm(2, 10^-2)
  bLeft ~  dnorm(2, 10^-2)
  bSigma ~ dnorm(0, 10^-2) T(0,)
  
  for(i in 1:nObs) {
    eHeight[i] <- bIntercept + bRight * Right[i] + bLeft * Left[i]
    Height[i] ~ dnorm(eHeight[i], bSigma^-2)
  }
}", new_expr = "
  for(i in 1:nObs) {
    eHeight[i] <- bIntercept + bRight * Right[i] + bLeft * Left[i]
    prediction[i] <- eHeight[i]
  }
", nthin = 10L, select_data = list(Height = c(0, 20),
                                   Right = c(0, 20),
                                   Left = c(0, 20)))

analysis <- analyse(model, data = data)
coef <- coef(analysis, simplify = TRUE)
plot_coef(coef)

prediction <- predict(analysis, new_data = c("Right")) 

gp <- ggplot(data = prediction, aes(x = Right, y = estimate)) +
  geom_point(data = data, aes(y = Height)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL
print(gp)
