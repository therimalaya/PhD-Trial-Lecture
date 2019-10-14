library(tidyverse)
library(gganimate)

fnc <- function(x, r = 1) {
  sqrt(r^2 - x^2)
}

r <- 1
df <- tibble(n = as.integer(seq(50, 5000, length.out = 30))) %>% 
  group_by(n) %>% 
  mutate(x = list(runif(n)),
         y = list(runif(n))) %>% 
  unnest(x, y) %>% 
  mutate(col = x^2 + y^2 <= r^2)

pi_df <- df %>% select(n, col) %>% 
  group_by(n) %>% 
  mutate(pi_ = sum(col)/n * 4) %>% 
  select(n, pi_) %>% unique()

plt <- df %>% 
  ggplot(aes(x, y)) +
  stat_function(fun = fnc, n = 100, xlim = c(0, r)) +
  geom_point(aes(color = col, group = factor(n)),
             shape = 16, show.legend = FALSE) +
  coord_fixed() +
  geom_hline(yintercept = c(0, r)) +
  geom_vline(xintercept = c(0, r)) +
  geom_text(x = 0, y = 1, data = pi_df, hjust = 0, vjust = -1,
            aes(label = paste("Total Sample:", n, ", Pi:", pi_), 
                group = n)) +
  labs(x = NULL, y = NULL) +
  transition_states(n) +
  ease_aes("elastic-in-out") +
  ggtitle("Monte Carlo Simulation",
          "Determining the value of Pi")

anim_save("MC-Pi.mp4", animation = plt, path = "_images")
