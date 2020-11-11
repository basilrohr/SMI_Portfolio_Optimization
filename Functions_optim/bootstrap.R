# This function samples the returns and calculates the efficiency frontier for each sample and
# the standard error of the weights across all samples.
# The inputs are the returns.
# The outputs are the sample weights of the minimum variance portfolio and tangency portfolio,
# the coordinates of the efficiency frontier, the coordinates of the mvp and tp
# and the standard error of the weights of the mvp and tp.

bootstrap_old = function(returns) {
  
  # Define number of bootstrap samples
  returns = returns[-1]
  n = 100
  
  sample_weights_mvp = sample_weights_tp = matrix(nrow = n, ncol = ncol(returns))
  colnames(sample_weights_mvp) = colnames(sample_weights_tp) = colnames(returns)
  sample_eff_frontier_sd = sample_eff_frontier_mu = c()
  sample_mvp_sd = sample_mvp_mu = sample_tp_sd = sample_tp_mu = numeric(n)
  
  for (i in 1:n) {
    
    # Draw nrow(returns) times one row from all returns with replacement
    sample = returns[sample(nrow(returns), size = nrow(returns), replace = T),]
    
    # Calculate mean return and volatility per stock and covariance of returns
    mu_return = colMeans(sample)
    volatility = sqrt(diag(cov(sample)))
    cov = cov(sample)
    mu_sd = data.frame(colnames(sample), mu_return, volatility)
    colnames(mu_sd) = c("Stock", "Mean_return", "Volatility")
    
    # Calculate weights of mvp and tp
    weights_mvp = markovitz_optim_mvp(mu_sd, cov)
    weights_tp = markovitz_optim_tp(mu_sd, cov)
    sample_weights_mvp[i,] = weights_mvp
    sample_weights_tp[i,] = weights_tp
    
    # Calculate coordinates of efficiency frontier
    eff_frontier_coord_output = eff_frontier(weights_mvp, weights_tp, mu_sd, cov)
    sample_eff_frontier_sd = rbind(sample_eff_frontier_sd,
                                   eff_frontier_coord_output$coord_eff_frontier[,1])
    sample_eff_frontier_mu = rbind(sample_eff_frontier_mu,
                                   eff_frontier_coord_output$coord_eff_frontier[,2])
    
    # Calculate coordinates of mvp and tp
    sample_mvp_sd[i] = eff_frontier_coord_output$coord_mvp[1]
    sample_mvp_mu[i] = eff_frontier_coord_output$coord_mvp[2]
    sample_tp_sd[i] = eff_frontier_coord_output$coord_tp[1]
    sample_tp_mu[i] = eff_frontier_coord_output$coord_tp[2]
  }
  
  # Calculate standard error of weights of mvp and tp
  sd_fun = function(x){quantile(x, 0.84) - quantile(x, 0.5)}
  sd_mvp = apply(sample_weights_mvp, 2, sd_fun)
  sd_tp = apply(sample_weights_tp, 2, sd_fun)
  
  return(list("sample_weights_mvp" = as.data.frame(sample_weights_mvp),
              "sample_weights_tp" = as.data.frame(sample_weights_tp),
              "sample_eff_frontier_sd" = sample_eff_frontier_sd,
              "sample_eff_frontier_mu" = sample_eff_frontier_mu,
              "sample_mvp_sd" = sample_mvp_sd,
              "sample_mvp_mu" = sample_mvp_mu,
              "sample_tp_sd" = sample_tp_sd,
              "sample_tp_mu" = sample_tp_mu,
              "sd_mvp" = sd_mvp,
              "sd_tp" = sd_tp))
}
