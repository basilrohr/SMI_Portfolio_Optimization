# These two functions perform the Markovitz optimization with covariance matrix inversion.
# The inputs are a data frame with the column mean return, the corresponding covariance matrix and
# the risk-free rate in the case of the tangency portfolio.
# The outputs are the weights of the minimum variance portfolio and tangency portfolio respectively.

markovitz_optim_mvp = function(df, cov) {
  # Calculate minimum variance portfolio weights
  ones = rep(1, nrow(cov))
  mu_inv = ones %*% solve(cov) %*% ones
  weights_mvp = c(1/mu_inv) * solve(cov) %*% ones
  return(weights_mvp)
}

markovitz_optim_tp = function(df, cov, Rf = 0) {
  # Calculate tangency portfolio weights
  ones = rep(1, nrow(cov))
  r = df[,2]
  lambda_inv = ones %*% solve(cov) %*% (r - c(Rf %*% ones))
  weights_tp = c(1/lambda_inv) * solve(cov) %*% (r - c(Rf %*% ones))
  return(weights_tp)
}
