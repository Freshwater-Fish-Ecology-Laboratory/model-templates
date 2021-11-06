source("header.R")

logit <- function(x) log(x / (1 - x))

### simulate range test data for demo and testing ----
set.seed(153)
nstation <- 6
# generate samples per river
n <- sample(x = 5:7, size = nstation, replace = TRUE)

bMidpoint <- 300 # typical midpoint
bIntercept <- 4 # typical intercept

# generate random slopes + intercepts
sMidpointStation <- 100 # sd of random effect
sInterceptStation <- 1 # sd of random intercept
bMidpointStation <- rnorm(nstation, bMidpoint, sMidpointStation)
bInterceptStation <- rnorm(nstation, bIntercept, sInterceptStation)

generate_distance <- function(n){
  sapply(1:n, function(x) {
    xmin <- (x-1)*100
    round(runif(1, xmin, xmin+50))
  })
}

de <- 0.5
de_logit <- logit(de)

data <- purrr::map_df(seq_len(nstation), function(i){
  distance <- generate_distance(n[i])
  intercept <- bInterceptStation[i]
  mu <- intercept + (de_logit - intercept)/bMidpointStation[i]*distance
  p <- 1/(1 + exp(-mu))
  pings <- round(runif(n[i], 50, 200))
  detects <- rbinom(n[i], size = pings, p = p)
  tibble::tibble(Station = factor(glue::glue("Station{i}")),
                 Distance = distance,
                 Detects = as.integer(detects),
                 Pings = as.integer(pings))
})

ggplot(data = data, aes(x = Distance, y = Detects/Pings)) +
  geom_point() + 
  facet_wrap(~Station)

message('dunif prior performs very poorly here')

model <- mbr::model(as.character(glue::glue(
  "model {

  bIntercept ~ dnorm(5, 5^-2)
  bMidpoint ~ dnorm(300, 50^-2) T(0,)

  sInterceptStation ~ dnorm(0, 5^-2) T(0,)
  sMidpointStation ~ dnorm(0, 200^-2) T(0,)

  for(i in 1:nStation) {
    bInterceptStation[i] ~ dnorm(0, sInterceptStation^-2)
    bMidpointStation[i] ~ dnorm(0, sMidpointStation^-2)
  }

  for(i in 1:nObs) {
    eIntercept[i] <- bIntercept + bInterceptStation[Station[i]]
    eMidpoint[i] <- bMidpoint + bMidpointStation[Station[i]]
    logit(eDetects[i]) <- eIntercept[i] + (_{de_logit}_ - eIntercept[i])/eMidpoint[i] * Distance[i]
    Detects[i] ~ dbin(eDetects[i], Pings[i])
  }
}", .open = "_{", .close = "}_")), new_expr = as.character(glue::glue(
  "
  for(i in 1:nObs) {
    eIntercept[i] <- bIntercept + bInterceptStation[Station[i]]
    eMidpoint[i] <- bMidpoint + bMidpointStation[Station[i]]
    logit(eDetects[i]) <- eIntercept[i] + (_{de_logit}_ - eIntercept[i])/eMidpoint[i] * Distance[i]
    prediction[i] <- eDetects[i]
  }
", .open = "_{", .close = "}_")), nthin = 2L,
  select_data = list(Station = factor(),
                     Distance = c(0, Inf),
                     Detects = c(0L, 1000L),
                     Pings = c(1L, 1000L)),
  random_effects = list(bMidpointStation = "Station",
                        bInterceptStation = "Station"))

analysis <- analyse(model, data = data, nthin = 20L)
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
                                      y = de), 
                 height = .05, color = "red") +
  geom_point(data = midpoint, aes(x = midpoint, y = de), color = "red") + 
  geom_vline(data = midpoint, aes(xintercept = midpoint), 
             linetype = "longdash", size = 0.3) +
  geom_line(data = prediction, aes(x = Distance, y = estimate)) +
  geom_line(data = prediction, aes(x = Distance, y = lower), linetype = "dotted") +
  geom_line(data = prediction, aes(x = Distance, y = upper), linetype = "dotted") +
  facet_wrap(~Station) +
  labs(x = "Distance", y = "Proportion of Pings Detected") +
  NULL
print(gp)
