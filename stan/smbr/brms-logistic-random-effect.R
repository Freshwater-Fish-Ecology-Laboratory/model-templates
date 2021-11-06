### Simulate detection efficiency by Velocity
source("header.R")

nstation <- 9
# generate samples per river
n <- sample(x = 50:70, size = nstation, replace = TRUE)

velocity <- runif(100, 0, 4)
bVelocity <- -3.5
bIntercept <- 9

# generate random slopes + intercepts
sVelocityStation <- 0.5 # sd of random effect
sInterceptStation <- 1.5 # sd of random intercept
bVelocityStation <- rnorm(nstation, bVelocity, sVelocityStation)
bInterceptStation <- rnorm(nstation, bIntercept, sInterceptStation)

library(purrr)
data <- map_df(seq_len(nstation), function(i){
  velocity <- runif(n[i], 0, 4)
  mu <- bInterceptStation[i] + bVelocityStation[i]*velocity
  p <- 1/(1 + exp(-mu))
  pings <- round(runif(n[i], 100, 150))
  detects <- rbinom(n[i], size = pings, p = p)
  data.frame(Station = factor(i), 
             Velocity = velocity,
             Detects = detects,
             Pings = pings)
})

ggplot(data = data, aes(x = Velocity, y = Detects/Pings, color = Station)) +
  geom_point()

m <- brms::brm(formula = Detects | trials(Pings) ~ 1 + Velocity + (1 + Velocity|Station),
               data = data,
               family = "binomial",
               cores = 4L,
               thin = 1L, iter = 1000)

brms::waic(m)
summary(m)


