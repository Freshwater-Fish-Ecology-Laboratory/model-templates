##### incomplete - may not run #######
# this is an example from Chapter 6 Mcelreath to demonstrate collider bias
# simulate 1300 people of all ages up to 65
source("header.R")

N_years <- 1000
max_age = 65
N_births = 20
aom = 18

H <- M <- A <- c()
for (t in 1:N_years) {
  A <- A + 1
  A <- c(A, rep(1, N_births))
  H <- c(H, seq(from = -2, to = 2, length.out = N_births))
  M <- c(M, rep(0, N_births))
  for (i in 1:length(A)) {
    if (A[i] >= aom & M[i] == 0) {
      M[i] <- rethinking::rbern(1, rethinking::inv_logit(H[i] - 4))
    }
  }
  deaths <- which(A > max_age)
  if (length(deaths) > 0) {
    A <- A[-deaths]
    H <- H[-deaths]
    M <- M[-deaths]
  }
}
d <- data.frame(Age = A, Married = M, Happiness = H)

# only include age 18+ in the model
d2 <- d[d$Age > 17,]
d2$A <- (d2$Age - 18)/ (65 - 18) 
d2$Mid <- d2$Married + 1

ggplot(data = d, aes(x = Age, y = Happiness, color = factor(Married))) +
  geom_point()

model <- model("model {

  bAge ~ dnorm(0, 2^-2)
  bSigma ~ dnorm(0, 2^-2) T(0,)
  for(i in 1:2){
    bMid[i] ~ dnorm(0, 1)
  }
  
  for(i in 1:nObs) {
    eHappiness[i] <- bMid[Mid[i]] + bAge * Age[i]  
    Happiness[i] ~ dnorm(eHappiness[i], bSigma^-2)
  }
}", new_expr = "
  for(i in 1:nObs) {
    eHappiness[i] <- bMid[Mid[i]] + bAge * Age[i]  
    prediction[i] <- eHappiness[i]
  }
", nthin = 10L)

analysis <- analyse(model, data = d2)
# here we see a negative relatinoship between age and happiness conditioned on marriage when that was not simulated
print(coef(analysis, simplify = TRUE), n = 100)
prediction <- predict(analysis, new_data = c("Age", "Mid")) 

## model without married index
model <- model("model {

  bAge ~ dnorm(0, 2^-2)
  bSigma ~ dnorm(0, 2^-2) T(0,)
  bIntercept ~ dnorm(0, 1)
  
  for(i in 1:nObs) {
    eHappiness[i] <- bIntercept + bAge * Age[i]  
    Happiness[i] ~ dnorm(eHappiness[i], bSigma^-2)
  }
}", new_expr = "
  for(i in 1:nObs) {
    eHappiness[i] <- bIntercept + bAge * Age[i]  
    prediction[i] <- eHappiness[i]
  }
", nthin = 10L)

analysis <- analyse(model, data = d2)
# here we see a correct estimate no relationship between age and happiness
print(coef(analysis, simplify = TRUE), n = 100)

  

