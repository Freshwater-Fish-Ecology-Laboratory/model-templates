### Simulate river temperature in relation to discharge
source("header.R")

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

model <- mbr::model("
  data {
    int nObs;
    
    vector[nObs] Discharge;
    vector[nObs] Temperature;
  }
  
  parameters {
    real bIntercept;
    real bDischarge;
    real sTemperature;
  }
  
  model {
  
  vector[nObs] eTemperature;

  bIntercept ~ normal(0, 5);
  bDischarge ~ normal(0, 2);

  sTemperature ~ normal(0, 5);
  
  for(i in 1:nObs) {
    eTemperature[i] = bIntercept + bDischarge * Discharge[i];
    Temperature[i] ~ normal(eTemperature[i], sTemperature);
  }
}", new_expr = "
  for(i in 1:length(Discharge)) {
   prediction[i] <- bIntercept + bDischarge * Discharge[i] 

  fit[i] <- prediction[i]

  residual[i] <- (Temperature[i] - fit[i]) / sTemperature
  }
", nthin = 2L, select_data = list(Discharge = c(0, 1000),
                                  Temperature = c(-10, 40)))


m <- brms::brm(formula = Temperature ~ Discharge,
               data = data)
summary(m)