# Compute the posterior distribution of the parameters: π(θ|X)
# The idea of MCMC is to “sample” from parameter values θi 
# in such a way that the resulting distribution approximates 
# the posterior distribution.

# Recall that Markov Chain is a random process that 
# depends only on its previous state, and that (if ergodic), 
# leads to a stationary distributoin.
# 
# The “trick” is to find sampling rules (MCMC algorithms) 
# that asymptotically approach the correct distribution.
# 
# There are several such (related) algorithms: 
# - Metropolis-Hastings 
# - Gibbs Sampling 
# - No U-Turn Sampling (NUTS) 
# - Reversible Jump

## ---- Loading Packages -----------
library(tidyverse)

## ---- Creating Data ----------------
true_beta <- 5
true_intercept <- 0
true_std <- 10
nobs <- 31

dta <- tibble(
  # create independent x-values
  x = (-(nobs - 1)/2):((nobs - 1)/2),
  # create dependent values according to ax + b + N(0,sd)
  y = true_beta * x + true_intercept + rnorm(n = nobs, mean = 0, sd = true_std)
)

## ---- Likelihood --------------
likelihood <- function(param, data) {
  beta <- param[1]
  intercept <- param[2]
  std <- param[3]
  
  pred <- beta * data$x + intercept
  single_likelihoods <- dnorm(data$y, mean = pred, sd = std, log = T)
  total_loglikelihoods <- sum(single_likelihoods)
  return(total_loglikelihoods)
}

## ---- Plotting Likelihood Profiles ------------
# Example: plot the likelihood profile of the slope a
# slopevalues <- function(x) {
#   return(likelihood(c(x, true_intercept, true_std)))
# }
# slopelikelihoods <- lapply(seq(3, 7, by = 0.05), slopevalues)
# 
# plot(seq(3, 7, by = 0.05), slopelikelihoods, type = "l", 
#      xlab = "values of slope parameter a", 
#      ylab = "Log likelihood")

## ---- Prior distribution ---------------
prior <- function(param) {
  a <- param[1]
  b <- param[2]
  sd <- param[3]
  aprior <- dunif(a, min = 0, max = 10, log = T)
  bprior <- dnorm(b, sd = 5, log = T)
  sdprior <- dunif(sd, min = 0, max = 30, log = T)
  return(aprior + bprior + sdprior)
}

## ---- Posterior distribution ---------------
posterior <- function(param, data){
  return(likelihood(param, data) + prior(param))
}

## ---- Metropolis algorithm ------------
proposalfunction <- function(param) {
  return(rnorm(3, mean = param, sd = c(0.1, 0.5, 0.3)))
}

run_metropolis_MCMC <- function(startvalue, iterations, data) {
  chain <- array(dim = c(iterations + 1, 3))
  chain[1, ] <- startvalue
  for (i in 1:iterations) {
    proposal <- proposalfunction(chain[i, ])
    
    probab <- exp(posterior(proposal, data) - posterior(chain[i, ], data))
    if (runif(1) < probab) {
      chain[i + 1, ] <- proposal
    } else {
      chain[i + 1, ] <- chain[i, ]
    }
  }
  return(chain)
}

## ---- Running Metropolis and collect results --------------
startvalue <- c(beta = 4, intercept = 0, std = 10)
chain <- run_metropolis_MCMC(startvalue, 2000, data = dta)

burnIn <- 1500
acceptance <- 1 - mean(duplicated(chain[-(1:burnIn), ]))

## ---- Summary and Plots ---------------
chain_df <- data.frame(
  BetaCoef = chain[-(1:burnIn), 1],
  Intercept = chain[-(1:burnIn), 2],
  Sigma = chain[-(1:burnIn), 3]
)
true_params <- data.frame(
  id = 1:3,
  key = c("BetaCoef", "Intercept", "Sigma"),
  value = c(true_beta, true_intercept, true_std)
)

pdf(file = "Bayesian-Trace.pdf", width = 8, height = 5, onefile = TRUE)
for (i in 3:nrow(chain_df)) {
  df <- chain_df[1:i,]
  df_ <- df %>% rownames_to_column("id") %>% 
    mutate_at("id", parse_integer) %>% 
    gather(key, value, -id)

  est_params <- df_ %>% 
    group_by(key) %>% 
    summarize(value = mean(value)) %>% 
    bind_cols(id = 1:3)
  
  plt1 <- df_ %>% 
    ggplot(aes(id, value)) +
    geom_line(group = 1, size = 0.25) +
    facet_wrap( ~ key, scales = "free_y", nrow = 1) +
    labs(x = "Iteration", y = "Trace")
  plt2 <- df_ %>% 
    ggplot(aes(value)) +
    geom_histogram(aes(y = ..density..), bins = 30, size = 0.1,
                   fill = "#ababab", color = "#efefef") +
    stat_density(geom = "line", n = 200, col = "royalblue") +
    facet_wrap(~ key, nrow = 1, scales = "free_y") +
    geom_vline(color = "navyblue", aes(xintercept = value),
               data = true_params, linetype = "dashed") +
    geom_vline(color = "firebrick", aes(xintercept = value),
               data = est_params, linetype = "dashed") +
    labs(x = NULL, y = "Density")
  
  gridExtra::grid.arrange(plt1, plt2, ncol = 1)
  if (i %% 100 == 0) cat("Iteration", i, "of", nrow(chain_df) - 3, "is completed.\n")
}
dev.off()


## ---- Result from Linear Model Fit ----------
mdl <- lm(y ~ x, data = dta)
summary(mdl)