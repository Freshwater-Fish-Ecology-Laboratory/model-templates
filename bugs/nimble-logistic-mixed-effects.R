source("header.R")

set.seed(10)
### set up data
ngroup = 6
n = 20
nobs = ngroup * n
b0 = 5
bdist = -0.02
sigma_re = 5
distance <- runif(nobs, 0, 800)
bstation <- rnorm(ngroup, mean = 0, sd = sigma_re)

mu <- b0 + bdist * distance + bstation

p <- 1/(1 + exp(-mu))
pings <- round(runif(nobs, 50, 60))
detects <- rbinom(nobs, size = pings, p = p)

data <- list(distance = distance, 
                   pings = as.integer(pings), 
                   detects = as.integer(detects),
             station = rep(1:ngroup, n))

ggplot(data = as.data.frame(data), aes(x = distance, y = detects/pings)) +
  geom_point() +
  facet_wrap(~station)

### logistic rnadom effects model
code <- nimbleCode({
  # priors
  # beta0 ~ dnorm(0, sd = 10000)
  bdist ~ dnorm(0, sd = 10000)
  # hyperpriors
  sigma_re ~ T(dnorm(0, sd = 1000), 0, )
  
  for(j in 1:nGroup){
    bstation[j] ~ dnorm(0, sd = sigma_re)
  }

  for (i in 1:nObs) {
    logit(p[i]) <- b0 + bdist * distance[i] + bstation[station[i]]
    detects[i] ~ dbin(p[i], pings[i])
  }
})

data <- list(distance = distance, 
             pings = as.integer(pings), 
             detects = as.integer(detects))

constants <- list(nObs = nobs,
                  nGroup = ngroup,
                  b0 = b0,
                  station = rep(1:ngroup, n))

inits <- list(bdist = -0.02, sigma_re = 5, bstation = rep(-0.02, ngroup), p = rep(0.5, 200))

monitor <- c("bdist", "bstation", "sigma_re")

model <- nimbleModel(code = code, constants = constants, 
                   inits = inits, data = data)

mcmc <- nimbleMCMC(model = model, nburnin = 10000, 
                   niter = 20000, thin = 10, 
                   nchains = 3, summary = TRUE,
                   monitor = monitor, 
                   samplesAsCodaMCMC = TRUE)


samples <- mcmc$samples
plot(samples)

coda_samples <- coda::mcmc(samples)
plot(coda_samples)
plot_sample_trace(samples)
plot_sample_density(samples)
plot_estimates(mcmc$summary)

