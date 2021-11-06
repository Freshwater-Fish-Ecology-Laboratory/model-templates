########## not working yet ########
source("header.R")

set.seed(10)
### set up data
ngroup = 6
n = 20
nobs = ngroup * n
b0 = 5
bdist = -0.02
btarget = 250
sigma_re = 100
distance <- runif(nobs, 0, 800)
bstation <- rnorm(ngroup, mean = 0, sd = sigma_re)

de_logit <- logit(0.5)
mu <- b0 + (de_logit - b0)/(btarget + bstation) * distance 

p <- 1/(1 + exp(-mu))
pings <- round(runif(nobs, 50, 60))
detects <- rbinom(nobs, size = pings, p = p)

data <- data.frame(Distance = distance, 
             Pings = as.integer(pings), 
             Detects = as.integer(detects),
             Station = factor(rep(1:ngroup, n)))

ggplot(data = data, aes(x = Distance, y = Detects/Pings)) +
  geom_point() +
  facet_wrap(~Station)

model <- mbr::model("

data {
  int<lower=0> nObs;
  int<lower=0> nStation;
  vector[nObs] Distance;
  int Pings[nObs];
  int Detects[nObs];
  int Station[nObs];
}
parameters {
  real b0;
  real bDist;
  real sDist;
  real bDistStation[nStation];
}
model {

  vector[nObs] eDetects;

  b0 ~ normal(0, 10);
  bDist ~ normal(0, 10);
  sDist ~ uniform(0, 1000);
  
  for(i in 1:nStation){
    bDistStation[i] ~ normal(0, sDist);
  };
  
  for(i in 1:nObs){
    eDetects[i] = b0 + (bDist + bDistStation[Station[i]]) * Distance[i];
    Detects[i] ~ binomial(Pings[i], inv_logit(eDetects[i]));
  }
}
", new_expr = "
  for(i in 1:length(Distance)) {
    logit(eDetects[i]) = b0 + bDist * Distance[i];
    prediction[i] <- eDetects[i]
  }
", nthin = 2L, select_data = list(Detects = 1L,
                                  Pings = 1L,
                                  `Distance` = c(0, 10000),
                                  Station = factor()))

analysis <- mbr::analyse(model, data = data)
coef(analysis, simplify = TRUE)
prediction <- predict(analysis, new_data = c("Area")) 

gp <- ggplot(data = prediction, aes(x = Area, y = estimate)) +
  geom_point(data = data, aes(y = Presence/Transects)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL
print(gp)

m <- brms::brm(formula = Detects | trials(Pings) ~ 1 + Distance + (1 + Distance|Station),
               data = data,
               family = "binomial",
               cores = 4L)
summary(m)

