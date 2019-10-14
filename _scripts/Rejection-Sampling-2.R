library(tidyverse)

set.seed(777)

fn <- function(x) dgamma(x, shape = 1.8)

df <- tibble(x = runif(1e4, -0.5, 10),
             y = fn(x),
             z = runif(length(x))/2.4,
             c = z <= y)

plt <- df %>% 
  ggplot(aes(x, y )) +
  geom_histogram(data = df %>% filter(c), aes(y = ..density.., x = x),
                 inherit.aes = FALSE, bins = 50, 
                 color = "grey30", fill = NA) +
  geom_point(aes(y = z, color = c, fill = c), alpha = 0.5,
             shape = 22, size = 2, show.legend = FALSE) +
  geom_line() +
  scale_fill_manual(values = c("lightgrey", "darkgrey")) +
  scale_color_manual(values = c("lightgrey", "darkgrey")) +
  labs(x = NULL, y = "Densities",
       title = expression(paste("Random sample from unknown ", 
                                gamma(1.8), " distribution")),
       subtitle = "Using acceptance-rejection sampling")

ggsave(plt, filename = "~/Dropbox/Talks/Trial-Lecture/_images/rejection-sampling.svg",
       width = 5, height = 5)