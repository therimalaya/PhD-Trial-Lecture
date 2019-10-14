#### Metropolis-Hastings Visualization ####
MHplot <- function(prop_sd = 0.6, target_mu = 0, target_sd = 1, seed = 2, iter = 500) {
  plot_range <- c(target_mu - 3 * target_sd, target_mu + 3 * target_sd)
  track <- NULL
  xc <- seed  ## set xc to the seed position
  
  if (!dir.exists("images")) dir.create("images")
  png(filename = "images/MH/MHplot%03d.png", width = 800, height = 700, res = 150, units = "px")
  #pdf(file = "MHplot.pdf", onefile = TRUE, width = 6, height = 5)
  par(mgp = c(1.8, 0.5, 0), bty = "n")
  layout(matrix(c(1, 3, 2, 2), 2, 2, byrow = T))
  
  for (i in 1:iter) {
    track <- c(track, xc)  ## The chain
    xi <- rnorm(1, xc, prop_sd)  ## Candidate point
    
    ## -- Plot Density of approaching Distribution -----------
    par(mar = c(1, 3, 1, 0))
    curve(dnorm(x, target_mu, target_sd), col = "royalblue", lty = 2, lwd = 2, 
          xlim = plot_range, ylab = "Density", ylim = c(0, 0.5))
    if (i > 2) lines(density(track, adjust = 1.5), col = "forestgreen", lwd = 2)
    
    ## -- plot the Trace -----------
    par(mar = c(3, 3, 2, 0))
    plot(1:i, track, ylim = plot_range, main = "", type = "l", ylab = "Trace", 
         xlab = "Iteration")
    
    pi_Y <- dnorm(xi, target_mu, target_sd, log = TRUE)
    pi_X <- dnorm(xc, target_mu, target_sd, log = TRUE)
    
    ## -- plot the target distribution and propsal distribution actions -------
    par(mar = c(1, 3, 1, 0))
    curve(dnorm(x, target_mu, target_sd), xlim = plot_range, col = "royalblue", 
          lty = 2, ylab = "Metropolis-Hastings", lwd = 2, ylim = c(0, 0.5))
    curve(dnorm(x, xc, prop_sd), col = "black", add = TRUE)
    points(xi, 0, pch = 19, cex = 2, col = "forestgreen")
    segments(x0 = xi, y0 = 0, x1 = xi, y1 = 0.5, col = "forestgreen", lty = 2)
    
    ## Accept move with a probability accept -----------
    accept <- (pi_Y) - (pi_X)
    if (accept > 0) accept <- 0
    if (log(runif(1)) <= accept) {
      xc <- xi
      ## If accepted change the point and segment color to red ------------
      points(xi, 0, pch = 19, col = "red4", cex = 2)
      segments(x0 = xi, y0 = 0, x1 = xi, y1 = 0.5, col = "red4", lty = 2)
    }
    
    ## Adapt the poposal ---------
    if (i > 100) prop_sd <- sd(track[floor(i/2):i])
    
    if (i%%100 == 0) cat("Plot", i, "of", iter, "is generated.\n")
  }
  dev.off()

}

MHplot(iter = 2500)
png2mp4 <- function(image_dir, movie_name, speed = 20) {
  # ---- Convert to a MP4 Movie ----------------
  cmd <- paste0("ffmpeg -y -r ", 
  				speed, " -i ",
  			    image_dir,
               	"/MHplot%03d.png ",
               	"-c:v libx264 -r 30 -pix_fmt yuv420p ", 
               movie_name)
  system(cmd)
}
