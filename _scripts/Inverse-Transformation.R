library(tidyverse)
library(gganimate)

df <- tibble(
  ux = runif(1000),
  nx = qnorm(ux),
  px = pnorm(ux)
)
sub_df <- df %>% sample_n(10) %>% 
  filter(nx >-1 & nx < 1)
plt <- df %>% 
  ggplot(aes(nx, ux)) +
  geom_histogram(aes(x = nx, y = ..density..), inherit.aes = FALSE,
                 bins = 30, color = "#cdcdcd", fill = "#dedede") +
  stat_density(aes(x = nx), inherit.aes = FALSE,
                 color = "#dedede", geom = "line") +
  geom_rug(length = unit(0.01, "npc"), alpha = 0.5, color = "royalblue") +
  geom_segment(aes(x = nx, xend = nx, yend = 0), 
               alpha = 0.5, data = sub_df, color = "forestgreen") +
  geom_segment(aes(x = nx, xend = min(df$nx), y = ux, yend = ux), 
               alpha = 0.5, data = sub_df, color = "forestgreen") +
  stat_function(fun = pnorm) +
  labs(x = "Random normal from inverse function",
       y = "Random uniform distribution between 0 and 1",
       title = "Generation of random Chi-square variates",
       subtitle = "From inverse transform using U(0,1)") +
  theme(axis.title.x = element_text(margin = margin(t = 8, 0, 0, 0, "mm")),
        axis.title.y = element_text(margin = margin(r = 8, 0, 0, 0, "mm"))) +
  annotate(geom = "text", x = 2.5, y = 0.95, label = "Cummulative\nNormal\nDistribution") +
  geom_point(aes(y = 0), data = sub_df, shape = 21, fill = "forestgreen") +
  geom_point(aes(x = min(df$nx)), data = sub_df, shape = 21, fill = "forestgreen") +
  geom_rug(data = sub_df, color = "forestgreen", length = unit(0.015, "npc"))

# plot(plt)
ggsave(plt, filename = "_images/inverse-transformation.svg",
       width = 5, height = 5)
