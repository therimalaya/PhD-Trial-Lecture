abx_mean <- function(nobs = 1e2, mu = 0, sigma = 1, seed = 7) {
  set.seed(seed)
  mean_theo <- sqrt(2 * sigma^2 / pi)
  lbl_y <- dnorm(0, mu, sigma)
  # Theoritically -------
  par(mar = c(2, 2, 1, 1), tck = 0.01, mgp = c(1, 0, 0))
  curve(dnorm(x, mu, sigma), from = 0, to = 4, xlab = "Quantiles", ylab = "Density")
  points(rep(mean_theo, 2), 
         c(0, dnorm(mean_theo, mu, sigma)),
         pch = 21, bg = "royalblue")
  segments(x0 = mean_theo, y0 = 0, 
           x1 = mean_theo, 
           y1 = dnorm(mean_theo, mu, sigma),
           col = "royalblue",
           lty = 2, lwd = 2)
  text(3, lbl_y, col = "royalblue", pos = 3, offset = -1,
       label = paste0("Theoritical Mean: ", round(mean_theo, 3)))
  
  # With Simulation -------
  x <- rnorm(nobs, mu, sigma)
  mean_sim <- mean(abs(x))
  points(rep(mean_sim, 2), 
         c(0, dnorm(mean_sim, mu, sigma)),
         pch = 21, bg = "forestgreen")
  segments(x0 = mean_sim, y0 = 0, 
           x1 = mean_sim, 
           y1 = dnorm(mean_sim, mu, sigma),
           col = "forestgreen",
           lty = 2, lwd = 2)
  text(3, lbl_y, col = "forestgreen", pos = 3, offset = -2,
       label = paste0("Simulated Mean: ", round(mean_sim, 3)))
  text(3, lbl_y, col = "darkgrey", pos = 3, offset = -3,
       label = paste0("Total Observations: ", nobs))
  text(3, lbl_y, col = "darkgrey", pos = 3, offset = -4,
       label = paste0("Standard Deviation: ", sigma))
}

png2mp4 <- function(image_dir, movie_name, speed = 20) {
  # ---- Convert to a MP4 Movie ----------------
  cmd <- paste0("ffmpeg -y -r ", 
                speed, " -i ",
                image_dir,
                "/Abs-Mean-Sim-%02d.png ",
                "-c:v libx264 -r 30 -pix_fmt yuv420p ", 
                movie_name)
  system(cmd)
}

png(filename="images/AbsMean/Abs-Mean-Sim-%02d.png", res = 150, width = 800, height = 800)
for (n in seq(10, 100, 5)) {
  abx_mean(nobs = n)
}
dev.off()
png2mp4("images/AbsMean", "AbsMean.mp4", speed = 2)

png(filename="images/AbsMean2/Abs-Mean-Sim-%02d.png", res = 150, width = 800, height = 800)
for (n in seq(10, 100, 5)) {
  abx_mean(nobs = n, sigma = 0.5)
}
dev.off()
png2mp4("images/AbsMean2", "AbsMean-2.mp4", speed = 2)
