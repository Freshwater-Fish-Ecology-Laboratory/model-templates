source("header.R")

### simulate range test data for demo and testing ----
set.seed(155)
nstation <- 6
# generate samples per river
n <- sample(x = 5:7, size = nstation, replace = TRUE)

bSlope <- -0.015 # typical effect of distance
bIntercept <- 5 # typical intercept

# generate random slopes + intercepts
sSlopeStation <- 0.005 # sd of random effect
sInterceptStation <- 1 # sd of random intercept
bSlopeStation <- rnorm(nstation, bSlope, sSlopeStation)
bInterceptStation <- rnorm(nstation, bIntercept, sInterceptStation)

generate_distance <- function(n){
  sapply(1:n, function(x) {
    xmin <- (x-1)*100
    round(runif(1, xmin, xmin+50))
  })
}
data <- purrr::map_df(seq_len(nstation), function(i){
  distance <- generate_distance(n[i])
  mu <- bInterceptStation[i] + bSlopeStation[i]*distance
  p <- 1/(1 + exp(-mu))
  pings <- round(runif(n[i], 75, 150))
  detects <- rbinom(n[i], size = pings, p = p)
  tibble::tibble(Station = factor(glue::glue("Station{i}")),
                 Distance = distance,
                 Detects = as.integer(detects),
                 Pings = as.integer(pings))
})

ggplot(data = data, aes(x = Distance, y = Detects/Pings)) +
  geom_point() + 
  facet_wrap(~Station)

model <- mbr::model("model {

  bIntercept ~ dnorm(5, 10^-2)
  bMidpoint ~ dunif(0, 1000)

  sInterceptStation ~ dnorm(0, 5^-2) T(0,)
  sMidpointStation ~ dnorm(0, 10^-2) T(0,)

  for(i in 1:nStation) {
    bInterceptStation[i] ~ dnorm(0, sInterceptStation^-2)
    bMidpointStation[i] ~ dnorm(0, sMidpointStation^-2)
  }

  for(i in 1:nObs) {
    eIntercept[i] <- bIntercept + bInterceptStation[Station[i]]
    eMidpoint[i] <- bMidpoint + bMidpointStation[Station[i]]
    logit(eDetects[i]) <- eIntercept[i] + -eIntercept[i]/eMidpoint[i] * Distance[i]
    Detects[i] ~ dbin(eDetects[i], Pings[i])
  }
}", new_expr = "
  for(i in 1:nObs) {
    eIntercept[i] <- bIntercept + bInterceptStation[Station[i]]
    eMidpoint[i] <- bMidpoint + bMidpointStation[Station[i]]
    logit(eDetects[i]) <- eIntercept[i] + -eIntercept[i]/eMidpoint[i] * Distance[i]
    prediction[i] <- eDetects[i]
  }
", nthin = 2L,
           random_effects = list(bMidpointStation = "Station",
                                 bInterceptStation = "Station"))

analysis <- analyse(model, data = data)
print(coef(analysis, simplify = TRUE), n = 100)
coef_rndm <- coef(analysis, simplify = TRUE, param_type = "random")
coefs <- coef(analysis, simplify = TRUE)

bmidpoint <- coefs$estimate[coefs$term == "bMidpoint"]
midpoint <- coef_rndm %>% 
  dplyr::filter(stringr::str_detect(term, "Midpoint")) %>%
  dplyr::mutate(midpoint = bmidpoint + estimate,
                midpoint_lower = bmidpoint + lower,
                midpoint_upper = bmidpoint + upper,
                Station = paste0("Station", 1:nrow(.))) 
  

prediction <- predict(analysis, new_data = c("Distance", "Station")) 

gp <- ggplot() +
  geom_point(data = data, aes(y = Detects/Pings, x = Distance)) +
  geom_errorbarh(data = midpoint, aes(xmin = midpoint_lower, 
                                           xmax = midpoint_upper,
                                           y = 0.5), 
                 height = .05, color = "red") +
  geom_point(data = midpoint, aes(x = midpoint, y = 0.5), color = "red") + 
  geom_vline(data = midpoint, aes(xintercept = midpoint), 
             linetype = "longdash", size = 0.3) +
  geom_line(data = prediction, aes(x = Distance, y = estimate)) +
  geom_line(data = prediction, aes(x = Distance, y = lower), linetype = "dotted") +
  geom_line(data = prediction, aes(x = Distance, y = upper), linetype = "dotted") +
  facet_wrap(~Station) +
  labs(x = "Distance", y = "Proportion of Pings Detected") +
  NULL
print(gp)
