library(tidyverse)
target_fn <- function(x) {
  ret <- sapply(x, function(k) {
    if (k < 0) return(0)
    else return(exp(-k))
    
  })
  return(ret)
}

mcmc <- function(niter, start_value, purposal_sd) {
  x <- vector(length = niter)
  x[1] <- start_value
  for (i in 2:niter) {
    current_value <- x[i-1]
    purposed_value <- rnorm(1, current_value, purposal_sd)
    ratio <- target_fn(purposed_value) / target_fn(current_value)
    if (runif(1) < ratio) {
      x[i] <- purposed_value
    } else {
      x[i] <- current_value
    }
  }
  return(x)
}

niter <- 10000
start <- 0.5
pvar <- 0.25
df <- tibble(
  x = 1:niter,
  y = mcmc(niter, start, pvar)
)
ggplot(df, aes(y)) +
  geom_histogram(bins = 30, aes(y = ..density..)) +
  stat_density(geom = "path") +
  stat_function(fun = target_fn, col = "red", linetype = "dashed")

ggplot(df, aes(x, y)) +
  geom_line()

## Effect of Starting Value
sv <- `names<-`(seq(1, 60, 10), seq(1, 60, 10))
samps <- map(sv, ~mcmc(1000, ..1, 1))
df <- bind_rows(samps) %>% 
  gather(sv, x) %>% 
  group_by(sv) %>% 
  mutate(id = row_number())

ggplot(df, aes(id, x)) +
  geom_line() +
  facet_wrap(.~sv, ncol = 3)

ggplot(df, aes(x)) +
  geom_histogram(bins = 50, fill = "grey",
                 aes(y = ..density..), color = "darkgrey") +
  stat_density(geom = "path") +
  stat_function(fun = target_fn, col = "red", linetype = "dashed") +
  facet_wrap(. ~ sv, ncol = 3)

df %>% 
  group_by(sv) %>% 
  summarize(avg = mean(x),
            varx = var(x)) %>% 
  ggplot(aes(sv, avg, size = varx)) +
  geom_point(shape = 21)

