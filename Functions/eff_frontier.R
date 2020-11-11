# This function calculates the efficiency frontier.
# The inputs are the weights of the minimum variance portfolio and tangency portfolio,
# a data frame with the column mean return and the corresponding covariance matrix.
# The outputs are the coordinates of the minimum variance portfolio and tangency portfolio and
# the weights and coordinates of the efficiency frontier.

eff_frontier = function(weights_mvp, weights_tp, df, cov) {
  
  # Functions to calculate coordinates for given weights
  sd_f = function(weights, cov) {sqrt(c(t(weights) %*% cov %*% weights))}
  mu_f = function(weights, df) {c(t(weights) %*% df[,2])}
  
  # Calculate coordinates of mvp and tp
  mvp_sd = sd_f(weights_mvp, cov)
  mvp_mu = mu_f(weights_mvp, df)
  tp_sd = sd_f(weights_tp, cov)
  tp_mu = mu_f(weights_tp, df)
  
  # Function to calculate weights of efficiency frontier for given mvp and tp weights
  weights_eff_frontier_f = function(weights_mvp, weights_tp, alpha) {
    alpha * weights_mvp + (1-alpha) * weights_tp
  }
  weights_eff_frontier_f_vec = Vectorize(weights_eff_frontier_f, vectorize.args = "alpha")
  
  # Calculate weights of efficiency frontier
  alpha = seq(-2, 2, by = 0.1)
  weights_eff_frontier = weights_eff_frontier_f_vec(weights_mvp, weights_tp, alpha)
  
  # Calculate coordinates of efficiency frontier
  eff_frontier_sd = apply(weights_eff_frontier, 2, sd_f, cov = cov)
  eff_frontier_mu = apply(weights_eff_frontier, 2, mu_f, df = df)
  
  weights_eff_frontier = t(weights_eff_frontier)
  colnames(weights_eff_frontier) = df[,1]
  
  return(list("coord_mvp" = cbind(mvp_sd, mvp_mu),
              "coord_tp" = cbind(tp_sd, tp_mu),
              "weights_eff_frontier" = as.data.frame(weights_eff_frontier),
              "coord_eff_frontier" = as.data.frame(cbind(eff_frontier_sd, eff_frontier_mu))))
}
