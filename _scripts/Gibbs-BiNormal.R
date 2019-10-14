library(ggplot2)

# https://observablehq.com/@herbps10/gibbs-sampler-for-bivariate-normal-posterior

rho <- 0.8
starting_values <- c(-3, 3)
iter <- 100
theta <- matrix(NA, ncol = 2, nrow = iter)
theta[1, ] <- starting_values
for (i in 1:(iter - 1)) {
	theta[i+1, 1] <- rnorm(1, rho * theta[i, 2], 1 - rho^2)
	theta[i+1, 2] <- rnorm(1, rho * theta[i, 1], 1 - rho^2)
}
colnames(theta) <- c("theta1", "theta2")
dta <- as.data.frame(theta) 
png(filename = "images/Gibbs/Gibbs%03d.png", width = 800, height = 800, res=120)
for (i in 3:nrow(dta)) {
	df <- dta[1:i, ]
	plt <- ggplot(df, aes(theta1, theta2)) +
	stat_density_2d(aes(fill = stat(density), size = stat(density)),
						 geom = "point", contour = FALSE, n = 25, shape = 21) +
	stat_density_2d(aes(size = stat(density)),
						 geom = "point", contour = FALSE, n = 25, shape = 21) +
	scale_fill_viridis_c(option = "A", alpha = 0.75, begin=0.2) +
	theme(legend.position = "right") +
	labs(x = "Theta1", y = "Theta2", size = "Density", fill = NULL) +
	ggtitle("Joint Bivariate Normal Density of Theta1 and Theta2",
			subtitle = paste0("Estimated Using Gibbs Sampling\n",
								"Correlation: ", rho, ", iteration: ", i)) +
	guides(size = guide_legend(order = 1),
		   colorbar = guide_legend(order = 2))
	plot(plt)
}
dev.off()

png2mp4 <- function(image_dir, movie_name, speed = 20) {
  # ---- Convert to a MP4 Movie ----------------
  cmd <- paste0("ffmpeg -y -r ", 
  				speed, " -i ",
  			    image_dir,
               	"/Gibbs%03d.png ",
               	"-c:v libx264 -r 30 -pix_fmt yuv420p ", 
               movie_name)
  system(cmd)
}

png2mp4("images/Gibbs", "Gibbs.mp4", speed = 10)