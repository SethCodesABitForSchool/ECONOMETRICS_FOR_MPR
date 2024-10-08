# R Tutorials - Bayesian VAR #

# 1. Install packages
install.packages("bvartools")
install.packages("BVAR")
library(bvartools)
library(BVAR)


# 2. Data Loading and Preprocessing
data("e1") # the dataset E1 from Lütkepohl (2007)
View(e1)

e1 <- diff(log(e1)) # Calculate first-log-differences
e1 <- window(e1, end = c(1978, 4))

e1 <- e1 * 100 # Rescale data
plot(e1) # Plot the series


# 3. VAR Model Specification
data <- gen_var(e1, p = 2, deterministic = "const")
y <- t(data$data$Y)
x <- t(data$data$Z)


# 4. Frequentist VAR Estimation
A_freq <- tcrossprod(y, x) %*% solve(tcrossprod(x)) # Calculate estimates
round(A_freq, 3) # Round estimates and print

u_freq <- y - A_freq %*% x
u_sigma_freq <- tcrossprod(u_freq) / (ncol(y) - nrow(x))
round(u_sigma_freq, 2)


# 5. Bayesian VAR Model Setup
set.seed(1234567) # Reset random number generator for reproducibility

iter <- 30000 # Number of iterations of the Gibbs sampler
burnin <- 15000 # Number of burn-in draws
store <- iter - burnin

tt <- ncol(y) # Number of observations
k <- nrow(y) # Number of endogenous variables
m <- k * nrow(x) # Number of estimated coefficients

# Set priors
a_mu_prior <- matrix(0, m) # Vector of prior parameter means
a_v_i_prior <- diag(1, m) # Inverse of the prior covariance matrix

u_sigma_df_prior <- 6 # Prior degrees of freedom
u_sigma_scale_prior <- diag(1, k) # Prior covariance matrix
u_sigma_df_post <- tt + u_sigma_df_prior # Posterior degrees of freedom

# Initial values
u_sigma_i <- solve(u_sigma_freq)

# Data containers for posterior draws
draws_a <- matrix(NA, m, store)
draws_sigma <- matrix(NA, k * k, store)



# 6. Gibbs Sampling
for (draw in 1:iter) {
  # Draw conditional mean parameters
  a <- post_normal(y, x, u_sigma_i, a_mu_prior, a_v_i_prior)
  
  # Draw variance-covariance matrix
  u <- y - matrix(a, k) %*% x # Obtain residuals
  u_sigma_scale_post <- solve(u_sigma_scale_prior + tcrossprod(u))
  u_sigma_i <- matrix(rWishart(1, u_sigma_df_post, u_sigma_scale_post)[,, 1], k)
  u_sigma <- solve(u_sigma_i) # Invert Sigma_i to obtain Sigma
  
  # Store draws
  if (draw > burnin) {
    draws_a[, draw - burnin] <- a
    draws_sigma[, draw - burnin] <- u_sigma
  }
}

A <- rowMeans(draws_a) # Obtain means for every row
A <- matrix(A, k) # Transform mean vector into a matrix
A <- round(A, 3) # Round values
dimnames(A) <- list(dimnames(y)[[1]], dimnames(x)[[1]]) # Rename matrix dimensions
A # Print

Sigma <- rowMeans(draws_sigma) # Obtain means for every row
Sigma <- matrix(Sigma, k) # Transform mean vector into a matrix
Sigma <- round(Sigma, 2) # Round values
dimnames(Sigma) <- list(dimnames(y)[[1]], dimnames(y)[[1]]) # Rename matrix dimensions
Sigma # Print


# 7. Bayesian VAR Estimation
bvar_est <- bvar(y = data$data$Y, x = data$data$Z, A = draws_a[1:18,], C = draws_a[19:21, ], Sigma = draws_sigma)
summary(bvar_est)


# 8. Impulse response analysis
FEIR <- irf(bvar_est, impulse = "income", response = "cons", n.ahead = 8)
plot(FEIR, main = "Forecast Error Impulse Response", xlab = "Period", ylab = "Response")
OIR <- irf(bvar_est, impulse = "income", response = "cons", n.ahead = 8, type = "oir")
plot(OIR, main = "Orthogonalised Impulse Response", xlab = "Period", ylab = "Response")
GIR <- irf(bvar_est, impulse = "income", response = "cons", n.ahead = 8, type = "gir")
plot(GIR, main = "Generalised Impulse Response", xlab = "Period", ylab = "Response")


# 9. Forecast error variance decomposition (FEVD)
bvar_fevd_oir <- fevd(bvar_est, response = "cons")
plot(bvar_fevd_oir, main = "OIR-based FEVD of consumption")

bvar_fevd_gir <- fevd(bvar_est, response = "cons", type = "gir")
plot(bvar_fevd_gir, main = "GIR-based FEVD of consumption")
