# Groups
groups = list(Consumer = c("Adecco", "Nestle", "Richemont", "Swatch", "Swisscom"),
              Finance = c("Credit Suisse", "Julius Baer", "Swiss Life", "Swiss Re",
                          "UBS", "Zurich Insurance"),
              Industrial = c("ABB", "Geberit", "Givaudan", "LafargeHolcim", "SGS"),
              Pharma = c("Lonza", "Novartis", "Roche", "Sika"))

# Groups returns
groups_returns = function(returns, groups) {
  groups_returns = data.frame(matrix(nrow = nrow(returns), ncol = length(groups)+1,
                              dimnames = list(NULL, c("Date", names(groups)))))
  for (i in seq_along(groups)) {groups_returns[i+1] = rowMeans(returns[-1][groups[[i]]])}
  groups_returns[1] = returns[1]
  groups_returns
}

# Covariance matrix
cov_mat = function(returns) {
  cov(returns[-1])
}

# Correlation matrix
cor_mat = function(cov_mat) {
  cov2cor(cov_mat)
}

# Mean returns
mean_returns = function(returns) {
  colMeans(returns[-1])
}

# Volatilities
volatilities = function(cov_mat) {
  sqrt(diag(cov_mat))
}

# Minimum variance portfolio weights
mvp_weights = function(cov_mat) {
  ones = rep(1, nrow(cov_mat))
  mu_inv = ones %*% solve(cov_mat) %*% ones
  mvp_weights = c(1/mu_inv) * solve(cov_mat) %*% ones
  c(mvp_weights)
}

# Tangency portfolio weights
tp_weights = function(cov_mat, mean_returns) {
  ones = rep(1, nrow(cov_mat))
  lambda_inv = ones %*% solve(cov_mat) %*% mean_returns
  tp_weights = c(1/lambda_inv) * solve(cov_mat) %*% mean_returns
  c(tp_weights)
}

# Portfolio return
pf_return = function(weights, mean_returns) {
  c(weights %*% mean_returns)
}

# Portfolio volatility
pf_volatility = function(weights, cov_mat) {
  c(weights %*% cov_mat %*% weights)
}

# Efficiency frontier weights
ef_weights = Vectorize(function(mvp_weights, tp_weights, alpha){alpha * mvp_weights +
    (1-alpha) * tp_weights}, vectorize.args = "alpha")







