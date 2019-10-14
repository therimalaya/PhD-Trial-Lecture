library(tidyverse)

fn <- function(x) {
  dnorm(x^2)
}

nobs <- 1e4

df <- tibble(
  x = runif(nobs, -2, 2),
  y = fn(x),
  z = runif(nobs, 0, max(y) + 0.1)
)

plt <- df %>%
  ggplot(aes(x, z)) +
  geom_point(color = ifelse(df$y <= df$z, "lightgrey", "darkgrey")) +
  stat_function(fun = fn, col = "#ababab") +
  geom_histogram(data = df %>% filter(z <= y),
                 aes(y = ..density.., x = x),
                 inherit.aes = FALSE, bins = 60,
                 color = "grey30", fill = NA) +
  labs(x = NULL, y = "Density",
       title = expression(paste("Random Samples from unknown distribution:",N(x^2))),
       subtitle = "Using Rejection Sampling with uniform envelope distribution")

ggsave(plt, filename = "~/Dropbox/Talks/Trial-Lecture/_images/rejection-sampling-1.svg",
       width = 5, height = 5)