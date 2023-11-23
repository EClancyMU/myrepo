# 3.1
df <- with(mtcars, data.frame(y = mpg, x1 = disp, x2 = hp, x3 = wt))

#3.2
nll_lm <- function(data, par) {
  y <- data$y
  X <- cbind(1, as.matrix(data[, -1]))
  epsilon <- y - X %*% par #residuals
  nll <- -sum(dnorm(epsilon, mean = 0, sd = sqrt(var(epsilon)), log = TRUE))
  return(nll)
}

#3.3
lower_bounds <- c(-1000, -100, -100, -100) 
upper_bounds <- c(1000, 100, 100, 100)  
#for this q these limits make sense looking at the data

initial_guess <- c(mean(df$y),0,0,0)

# Using optim to find MLE
result <- optim(
  par = initial_guess,
  fn = nll_lm,
  data = df,
  lower = lower_bounds,
  upper = upper_bounds,
  method = "L-BFGS-B"
)

beta_hat <- result$par
sigma_hat <- sqrt(var(df$y - cbind(1, as.matrix(df[, -1])) %*% beta_hat))

cat("Beta_hat:", beta_hat, "\n")
cat("Sigma_hat (Estimated Residual Standard Deviation):", sigma_hat, "\n")

#3.4
#optim() minimize functions therefore to find maximum likelihood we minimize the negative likelihood => maximizing positive likelihood

#3.5
beta_LS3 <- function(X, y) { ##from lec5 notes
  solve(crossprod(X), crossprod(X, y))
}

X <- cbind(1, as.matrix(df[, -1])) 
y <- df$y

beta_optim <- result$par #from 3.3

betals3 <- beta_LS3(X, y)

# Compare coefficients
comparison <- data.frame(
  Parameter = c("Intercept", "beta_x1", "beta_x2", "beta_x3"),
  Coef_optim = beta_optim,
  Coef_ls3 = betals3
)

print(comparison)
#as shown they are basicaly equal, just some accuracy issues due to rounding

#3.6
sigma_optim <- sigma_hat

residuals <- df$y - cbind(1, as.matrix(df[, -1])) %*% beta_hat
sigma_matrix <- sqrt(var(residuals))

#Compare
cat("Estimated Residual Standard Deviation (sigma) - optim:", sigma_optim, "\n")
cat("Estimated Residual Standard Deviation (sigma) - matrix operations:", sigma_matrix, "\n")
#again they're equal

#3.7
#mine are equal

#3.8
hessian <- (vcov(lm(y ~ x1 + x2 + x3, data = df)))
se <- sqrt(diag(hessian))
cat("Standard Errors of Coefficients (beta):", se, "\n")