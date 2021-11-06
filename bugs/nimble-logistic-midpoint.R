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

data <- list(distance = as.vector(scale(distance, scale = TRUE)), 
             pings = as.integer(pings), 
             detects = as.integer(detects),
             station = rep(1:ngroup, n))

ggplot(data = as.data.frame(data), aes(x = distance, y = detects/pings)) +
  geom_point() +
  facet_wrap(~station)

### logistic rnadom effects model
code <- nimbleCode({
  # priors
  b0 ~ dflat()
  btarget ~ dflat()
  # hyperpriors
  sigma_re ~ dhalfflat()
  
  for(j in 1:nGroup){
    bstation[j] ~ dnorm(0, sd = sigma_re)
  }
  
  for (i in 1:nObs) {
    etarget[i] <- btarget + bstation[station[i]]
    logit(p[i]) <- b0 + (de_logit - b0)/etarget[i] * distance[i]  
    detects[i] ~ dbin(p[i], pings[i])
  }
})

scale2 <- function(x) as.vector(scale(x, scale = TRUE))
data <- list(detects = as.integer(detects),
             distance = distance, 
             pings = as.integer(pings))

constants <- list(nObs = nobs,
                  nGroup = ngroup,
                  # b0 = b0,
                  station = rep(1:ngroup, n),
                  de_logit = logit(0.5)
                  )

inits <- list(btarget = 10, b0 = 10, sigma_re = 10, bstation = rep(10, ngroup), p = rep(0.5, 200))

monitor <- c("btarget", "b0", "bstation", "sigma_re")

model <- nimbleModel(code = code, constants = constants, 
                     inits = inits, data = data)

spec <- configureMCMC(model, 
                      # nodes = NULL,
                      monitors = monitor)

# spec$addSampler(type = 'RW_block', target = c('btarget', 'b0'))
# spec$addSampler(type = 'RW', target = 'sigma_re')
# spec$addSampler(type = 'RW_block', target = 'bstation[1:6]')

mcmc <- buildMCMC(spec)
cmodel <- compileNimble(model)
cmcmc <- compileNimble(mcmc, project = model)

run_mcmc <- function(model){
  nimble::runMCMC(model, niter = 110000, 
          nchains = 1, thin = 10, nburnin = 10000,
          samplesAsCodaMCMC = TRUE, summary = TRUE)
}

result <- mclapply(1:4, FUN = run_mcmc, 
                   model = cmcmc)

samples <- coda::mcmc.list(map(result, ~ .$samples))
plot(samples)
coda::gelman.diag(samples)



