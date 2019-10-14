library(ggplot2)

# The data:
S <- 39    # Number of sites
K <- 5     # Number of visits to each site
xObs <- 18 # Number of sites where salamanders were detected
d <- 30    # Total number of detections

# Priors: independent beta(1, 1) priors
priPsi <- c(1, 1)
priPi <- c(1, 1)

nIter <- 1000               # Number of iterations
psi <- pi <- numeric(nIter) # Objects to hold results
psi[1] <-  pi[1] <- 0.5     # Starting values

for(i in 2:nIter) {
  # 1. Calculate prob(occupied | not detected):
  psi0 <- (psi[i-1] * (1 - pi[i-1])^K) / (psi[i-1] * (1 - pi[i-1])^K + (1 - psi[i-1]))
  # ...and draw number of additional sites occupied
  xAdd <- rbinom(1, S - xObs, psi0)
  x <- xObs + xAdd
  # 2a. Draw new psi from beta(occupied+prior, unoccupied+prior)
  psi[i] <- rbeta(1, x + priPsi[1], S - x + priPsi[2])
  # 2b. Draw new pi from beta(detected+prior, undetected+prior).
  pi[i] <- rbeta(1, d + priPi[1], x * K - d + priPi[2])
}

df <- data.frame(psi = psi, pi = pi)
pdf(file = "Gibbs.pdf")
for (i in 2:nrow(df)) {
	df_ <- df[1:i, ]
	plt <- ggplot(df_, aes(psi, pi)) +
		geom_point(col = "royalblue") +
		geom_line(group = 1, col = "grey") +
		coord_cartesian(xlim = c(0.2, 1), ylim = c(0, 0.8)) +
		labs(x = "Occupancy, psi", y = "Detection, Pi") +
		geom_point(col = "firebrick", data = df_[1,])
	plot(plt)
}
dev.off()