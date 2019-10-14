library(tidyverse)

nsamp <- 1000
plot_dist <- function(dist_name, pdf_function, samples) {
  df <- tibble(x = samples)
  ggplot(df, aes(x)) +
    geom_histogram(
      bins = 30,
      aes(y = ..density..),
      color = "lightgrey",
      fill = "grey"
    ) +
    stat_density(geom = "path") +
    stat_function(fun = pdf_function, color = "blue", linetype = "dashed") +
    labs(x = NULL, y = NULL) +
    ggtitle(paste(dist_name, "Distribution"))
}

df <- tibble(
  Distribution = c("Uniform", "Normal", "Gamma", "Chi-Sq", "Beta", "Cauchy"),
  SampFun = list(
    function(x, ...) runif(x, ...),
    function(x, ...) rnorm(x, ...),
    function(x, ...) rgamma(x, shape = 2, ...),
    function(x, ...) rchisq(x, df = 15, ...),
    function(x, ...) rbeta(x, shape1 = 1.3, shape2 = 2.4, ...),
    function(x, ...) rcauchy(x, scale = 1.5, ...)
  ),
  DistFun = list(
    function(x, ...) dunif(x, ...),
    function(x, ...) dnorm(x, ...),
    function(x, ...) dgamma(x, shape = 2, ...),
    function(x, ...) dchisq(x, df = 15, ...),
    function(x, ...) dbeta(x, shape1 = 1.3, shape2 = 2.4, ...),
    function(x, ...) dcauchy(x, scale = 1.5, ...)
  )) %>% 
  mutate(Samples = map(SampFun, ~.x(nsamp))) %>% 
  mutate(Plot = pmap(., ~plot_dist(..1, ..3, ..4)))

# do.call(gridExtra::grid.arrange, df$Plot)

plots <- `names<-`(df$Plot, df$Distribution)
for(x in names(plots))
  ggsave(plots[[x]], filename = paste0("_images/", paste0(x, ".svg")), 
         width = 5, height = 4)