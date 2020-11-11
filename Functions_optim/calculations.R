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
cov_mat = function(returns) {cov(returns[-1])}

# Correlation matrix
cor_mat = function(returns) {cor(returns[-1])}

# Mean returns
mean_returns = function(returns) {colMeans(returns[-1])}

# Volatilities
volatilities = function(cov_mat) {sqrt(diag(cov_mat))}

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
pf_return = function(weights, mean_returns) {c(weights %*% mean_returns)}

# Portfolio volatility
pf_volatility = function(weights, cov_mat) {c(sqrt(weights %*% cov_mat %*% weights))}

# Efficiency frontier weights
ef_weights = Vectorize(function(mvp_weights, tp_weights, alpha){alpha * mvp_weights +
    (1-alpha) * tp_weights}, vectorize.args = "alpha")

# Efficiency frontier points
ef_points = function(ef_weights, cov_mat, mean_returns) {
  x = apply(ef_weights, 2, function(x){pf_volatility(x, cov_mat)})
  y = apply(ef_weights, 2, function(x){pf_return(x, mean_returns)})
  cbind(x, y)
}

# Minimum variance portfolio point via efficiency frontier points
mvp_point = function(ef_points) {
  i = which.min(ef_points[,1])
  cbind(ef_points[,1][i], ef_points[,2][i])
}

# Tangency portfolio point via efficiency frontier points
tp_point = function(ef_points) {
  i = which.max(ef_points[,2] / ef_points[,1])
  cbind(ef_points[,1][i], ef_points[,2][i])
}

# Bootstrap mvp and tp weights standard errors and efficiency frontiers
bootstrap = function(returns) {
  n = 100
  samples_mvp_weights = samples_tp_weights = matrix(nrow = ncol(returns)-1, ncol = n)
  samples_ef_points = vector(mode = "list", length = n)
  for (i in 1:n) {
    sample = returns[sample(nrow(returns), replace = T),]
    cm = cov_mat(sample); mr = mean_returns(sample)
    mvpw = mvp_weights(cm); tpw = tp_weights(cm, mr)
    samples_mvp_weights[,i] = mvp_weights(cm)
    samples_tp_weights[,i] = tp_weights(cm, mr)
    samples_ef_weights = ef_weights(mvpw, tpw, seq(-2, 2, 0.1))
    samples_ef_points[[i]] = ef_points(samples_ef_weights, cm, mr)
  }
  weights_sd = function(x) {quantile(x, 0.84) - quantile(x, 0.5)}
  mvp_weights_sd = apply(samples_mvp_weights, 2, weights_sd)
  tp_weights_sd = apply(samples_tp_weights, 2, weights_sd)
  list(mvp_weights_sd = mvp_weights_sd, tp_weights_sd = tp_weights_sd,
       samples_ef_points = samples_ef_points)
}














