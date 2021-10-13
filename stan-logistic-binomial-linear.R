### this simulates proportion of sample areas with presence/absence on islands of different area
# presence more likely on larger islands

source("header.R")

area <- runif(100, 0, 1000)
bArea <- 0.01
bIntercept <- -3
x <- bIntercept + bArea*area

p <- 1/(1 + exp(-x))
transects <- round(runif(100, 50, 60))
presence <- rbinom(100, size = transects, p = p)

data <- data.frame(Area = area, 
                   Transects = as.integer(transects),
                   Presence = as.integer(presence))
ggplot(data = data, aes(x = Area, y = Presence/Transects)) +
  geom_point()

message("why does this model run even when remove the priors?")

model <- model("

data {
  int<lower=0> nObs;
  vector[nObs] Area;
  int Presence[nObs];
  int Transects[nObs];
}
parameters {
  real bIntercept;
  real bArea;
}
model {

  vector[nObs] ePresence;

  bIntercept ~ normal(0, 10);
  bArea ~ normal(0, 10);
  
  for(i in 1:nObs){
    ePresence[i] = bIntercept + bArea * Area[i];
    Presence[i] ~ binomial(Transects[i], inv_logit(ePresence[i]));
  }
}
", new_expr = "
  for(i in 1:length(Area)) {
    logit(ePresence[i]) = bIntercept + bArea * Area[i];
    prediction[i] <- ePresence[i]
  }
", nthin = 2L, select_data = list(Presence = 1L,
                                  Transects = 1L,
                                  `Area` = c(0, 10000)))

analysis <- analyse(model, data = data)
coef(analysis, simplify = TRUE)
prediction <- predict(analysis, new_data = c("Area")) 

gp <- ggplot(data = prediction, aes(x = Area, y = estimate)) +
  geom_point(data = data, aes(y = Presence/Transects)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL
print(gp)

