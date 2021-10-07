### Simulate fish length-weight lognormal relationship 
# (borrowed from Joes lar-ldr-rb-juv condition model)
source("header.R")

length <- runif(100, 30, 50)
bLength <- 3
bIntercept <- -5
sWeight <- -2
x <- bIntercept + bLength * log(length)
weight <- rlnorm(100, mean = x, sd = exp(sWeight))

data <- data.frame(Weight = weight,
                   Length = length)

ggplot(data = data, aes(x = Length, y = Weight)) +
  geom_point()

model <- model("
 data {
  int nObs;
  vector[nObs] Length;
  vector[nObs] Weight;
}

parameters {
  real bIntercept;
  real bLength;
  real sWeight;
}

model {

  vector[nObs] eWeight;

  bIntercept ~ normal(-10, 5);
  bLength ~ normal(0, 2);

  sWeight ~ normal(-10, 5);
  
  for(i in 1:nObs) {
    eWeight[i] = bIntercept + bLength * log(Length[i]);
    Weight[i] ~ lognormal(eWeight[i], exp(sWeight));
  }
}",
new_expr = "
  for(i in 1:length(Length)) {

  prediction[i] <- exp(bIntercept + bLength * log(Length[i]))

  fit[i] <- prediction[i]

  residual[i] <- (log(Weight[i]) - log(fit[i])) / exp(sWeight)
  }
",
select_data = list(Weight = 1, "Length" = 1),
nthin = 2L
)

analysis <- analyse(model, data = data)
coef(analysis, simplify = TRUE)
prediction <- predict(analysis, new_data = c("Length")) 

gp <- ggplot(data = prediction, aes(x = Length, y = estimate)) +
  geom_point(data = data, aes(y = Weight)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL
print(gp)
