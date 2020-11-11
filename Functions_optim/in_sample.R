# This function calculates the in sample sharpe ratio.
# The inputs are the constructor output and optionally the data interval.
# The outputs are the SMI and SMI groups in sample sharpe ratio.

in_sample_old = function(constructor, interval = "1d") {
  
  # Calculate TP weights
  tpw_SMI = markovitz_optim_tp(constructor$SMI_mu_sd,
                               constructor$SMI_cov)
  tpw_SMI_groups = markovitz_optim_tp(constructor$SMI_groups_mu_sd,
                                      constructor$SMI_groups_cov)
  
  # Apply TP weights to returns
  in_sample_SMI = c(as.matrix(constructor$SMI_returns[,-1]) %*% tpw_SMI)
  in_sample_SMI_groups = c(as.matrix(constructor$SMI_groups_returns[,-1]) %*% tpw_SMI_groups)
  
  # Calculate sharpe ratio
  sr_is_SMI = sharpe_ratio(mean(in_sample_SMI), sd(in_sample_SMI), 0, interval)
  sr_is_SMI_groups = sharpe_ratio(mean(in_sample_SMI_groups), sd(in_sample_SMI_groups), 0, interval)
  
  return(list("sr_is_SMI" = sr_is_SMI,
              "sr_is_SMI_groups" = sr_is_SMI_groups))
}
