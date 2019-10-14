library(tidyverse)

lm_eqn <- function(mdl){
  cf <- unname(coef(mdl))
  rsq <- summary(mdl)[["r.squared"]]
  eq <- substitute(hat(y) == a + b %.% italic(x) * "," ~~ 
                     italic(r)^2 ~ "=" ~ r2 * "," ~~ 
                     hat(sigma) ~ "=" ~ s,
                   list(a = format(cf[1], digits = 2),
                        b = format(cf[2], digits = 2),
                        r2 = format(rsq, digits = 3),
                        s = format(sigma(mdl), digits = 3)
                   ))
  as.character(as.expression(eq))
}

## From Real Data
dta_real <- read.csv(url("https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/ThreeCars2017.csv"))
mdl_real <- lm(Price ~ Mileage, data = dta_real)
dta_real <- dta_real %>% mutate(fitted = fitted(mdl_real))
plt_real <- ggplot(dta_real, aes(Mileage, Price)) +
  geom_segment(aes(yend = fitted, xend = Mileage), size = 0.25,
               linetype = "dashed", color = "red1") +
  geom_point(shape = 21, fill = "lightgrey", size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  geom_point(aes(y = fitted), shape = 4, size = 0.5) +
  annotate(x = Inf, y = Inf, label = lm_eqn(mdl_real), geom = "text",
           hjust = 1, vjust = 1, parse = TRUE) +
  labs(x = "Mileage (in thousands of miles)", y = "Price (in thousands of dollars)") +
  ggtitle("Real dataset of used car price in 2007 from cars.com",
          subtitle = as.expression(bquote(atop(
            "From dataset: "~
              n == .(nrow(dta_real))~", "~
              beta[0] == .(round(coef(mdl_real)[1], 2))~", "~
              beta[1] == .(round(coef(mdl_real)[2], 2))~", "~ 
              sigma == .(round(sigma(mdl_real), 2))~", "~
              mu[x] == .(round(mean(dta_real$Mileage), 2))~", "~
              sigma[x] == .(round(sd(dta_real$Mileage), 2))
          )))
  )


## (in Thousand)
nobs <- nrow(dta_real) # Number of cars
beta0 <- round(coef(mdl_real)[1], 2) # Price of new car
beta1 <- round(coef(mdl_real)[2], 2) # Decrease in price due to increase of milage by 1000Km
sigma <- 3 # Variation in price in general
mu_x <- round(mean(dta_real$Mileage)) # Average Mileage
sigma_x <- round(sd(dta_real$Mileage), 2) # Mileage Variation

mileage <- abs(rnorm(nobs, mu_x, sigma_x))
price <- beta0 + beta1 * mileage + rnorm(nobs, 0, sigma)
mdl_sim <- lm(price ~ mileage)
df_sim <- tibble(
  cars = 1:nobs,
  mileage = mileage,
  price = price,
  fitted = mdl_sim$fitted
)

plt_sim <- ggplot(df_sim, aes(mileage, price)) +
  geom_abline(slope = beta1, intercept = beta0, color = "royalblue", linetype = "dashed") +
  geom_segment(aes(yend = fitted, xend = mileage), size = 0.25,
               linetype = "dashed", color = "red1") +
  geom_point(shape = 21, fill = "lightgrey", size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  geom_point(aes(y = fitted), shape = 4, size = 0.5) +
  annotate(x = Inf, y = Inf, label = lm_eqn(mdl_sim), geom = "text",
           hjust = 1, vjust = 1, parse = TRUE) +
  labs(x = "Mileage (in thousands of miles)", y = "Price (in thousands of dollars)") +
  ggtitle("Simulated data with controlled regression coefficients and model variance",
          subtitle = as.expression(bquote(
            "Controlled values: "~
              n == .(nobs)~", "~
              beta[0] == .(beta0)~", "~
              beta[1] == .(beta1)~", "~ 
              sigma == .(sigma)~", "~
              mu[x] == .(mu_x)~", "~
              sigma[x] == .(sigma_x)
          )))

# gridExtra::grid.arrange(plt_real, plt_sim, ncol = 1)
ggsave(plt_real, filename = "_images/real.svg", width = 7, height = 7)
ggsave(plt_sim, filename = "_images/sim.svg", width = 7, height = 7)