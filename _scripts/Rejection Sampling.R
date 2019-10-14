## ---- Acceptance-Rejection Sampling -----

fn <- function(x, a, b) {
	1/beta(a, b) * x^(a - 1) * (1 - x)^(b - 1)
}
rfn <- function(n, a, b) {
	x <- seq(0, 1, length.out=1e2)
	M0 <- max(fn(x, a, b))
	browser()
	y0 <- runif(n*2, 0, 1)
	gy <- dunif(y0, 0, 1)
	fy <- fn(y0, a, b)
	accept <- runif(n*2, 0, 1) < fy/(M0 * gy)
	return(y0[accept][1:n])
}

a <- 1.33
b <- 1.67
y <- rfn(1e5, a, b)

library(ggplot2)
plt <- ggplot(data.frame(y), aes(y)) +
	stat_density(geom = "path") +
	stat_function(fun = fn, args = list(a = a, b = b),
				  col = "red", linetype = "dashed")
plot(plt)