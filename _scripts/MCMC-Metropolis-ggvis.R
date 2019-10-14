# The Metropolis–Hastings algorithm thus consists in the following:
#   
# 0. Initialise
#   - Pick an initial state x0;
#   - Set t=0;
# Iterate:
# 1. Generate: randomly generate a candidate state x_c according to g(x_c|x_t)
# 2. Calculate: calculate the acceptance probability A(x_c, x_t) = min(1, p(x_c)/p(x_t)*g(x_c|x_t)/g(x_t|x_c))
# 3. Accept or Reject:
#   - generate a uniform random number u = U(0, 1)
#   - if u <= A(x_c, x_t), accept the new state and set x_(t+1) = x_c
#   - if u > A, reject the new state, and copy the old state forward x_(t+1) = x_t
# 4. Increment: set t = t + 1
library(ggplot2)

iter <- 1000
target_mu <- 0
target_sd <- 1

xc <- 1
proposal_sd <- 1
xc_vec <- c()

for (i in 1:iter) {
  xi <- rnorm(1, mean = xc, sd = proposal_sd)
  pxc <- dnorm(xc, mean = target_mu, sd = target_sd)
  pxi <- dnorm(xi, mean = target_mu, sd = target_sd)
  gxc_t <- dnorm(xc, mean = xi, sd = proposal_sd)
  gxt_c <- dnorm(xi, mean = xc, sd = proposal_sd)
  accept <- min(1, pxi/pxc*gxc_t/gxt_c)
  u <- runif(1)
  xc <- if (u <= accept) xi else xc
  xc_vec[i] <- xc
}

df <- data.frame(x = 1:iter, y = xc_vec)
for (i in 2:iter) {
  png(filename = "test/MHplot%03d.png", res = 150, width = 800, height = 1000, units = "px")
  p1 <- ggplot(df[1:i, ], aes(x, y)) +
    geom_line(group = 1, size = 0.3) +
    labs(x = "Iterations", y = "Trace")
  p2 <- ggplot(df[1:i,], aes(y)) +
    stat_density(geom = "line", color = "forestgreen") +
    stat_function(fun = "dnorm", args = list(mean = target_mu, sd = target_sd),
                  xlim = c(-3, 3), color = "royalblue", linetype = "dashed") +
    labs(x = NULL, y = "Density")
  df_ <- df[i, ]
  
  p3 <- ggplot(data.frame(x = seq(-3, 3, length.out = 10)), aes(x = x)) +
    layer(stat = "function",
          geom = "line",
          position = "identity",
          params = list(
            fun = dnorm, 
            args = list(mean = target_mu, sd = target_sd),
            linetype = "dashed",
            xlim = c(-3, 3)),
          mapping = aes(color = "Target Distribution")) +
    layer(stat = "function",
          geom = "line",
          position = "identity",
          params = list(
            fun = dnorm, 
            args = list(mean = df_$y, sd = proposal_sd),
            xlim = c(-3, 3)),
          mapping = aes(color = "Proposal Distribution")) +
    scale_color_manual(values = c("forestgreen", "royalblue")) +
    labs(x = "Quantiles", y = "Density", color = "Distribution") +
    theme(legend.position = "top")
  
  
  plt <- gridExtra::arrangeGrob(p1, p2, p3, heights = c(3, 3, 4),
                                layout_matrix = matrix(c(1, 2, 3), 3, 1, byrow = TRUE))
  grid::grid.newpage()
  grid::grid.draw(plt)
  cat("Plot Number:", i, "generated.\n")
  dev.off()
}
system("ffmpeg -y -r 20 -i test/MHplot%03d.png -c:v libx264 -r 30 -pix_fmt yuv420p test/MHplot.mp4")
# system("rm test/*.png")