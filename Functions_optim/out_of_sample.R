# This function calculates the out of sample sharpe ratio.
# The inputs are the slices and optionally shrinking coefficients for the returns and correlation,
# data interval and section.
# The default outputs are the SMI and SMI groups out of sample sharpe ratio and their returns
# and the alternative outputs are additionally the SMI and SMI groups returns, mean returns
# and volatility, covariance and TP weights.

out_of_sample_old = function(slices, r_shrink = 0, cor_shrink = 1, interval = "1d", section = NULL) {
  
  out_of_sample_SMI = out_of_sample_SMI_groups = c()
  
  # Check if function argument "section" is provided or not for alternative outputs
  if (is.null(section)) {seq = 1:length(slices[[1]])}
  else {seq = section}
  
  # Iterate through each slice
  for (i in seq) {
    
    # Shrinking of SMI mean returns
    slices[[1]][[i]]$SMI_mu_sd[,2] = (1 - r_shrink) * slices[[1]][[i]]$SMI_mu_sd[,2] +
      r_shrink * mean(slices[[1]][[i]]$SMI_mu_sd[,2])
    
    # Shrinking of SMI correlation
    cor = cov2cor(slices[[1]][[i]]$SMI_cov)
    diag(cor) = 0
    cor = diag(nrow(cor)) + cor_shrink * cor
    slices[[1]][[i]]$SMI_cov = diag(slices[[1]][[i]]$SMI_mu_sd[,3]) %*% cor %*%
      diag(slices[[1]][[i]]$SMI_mu_sd[,3])
    
    # Calculate SMI TP weights
    tpw_SMI = markovitz_optim_tp(slices[[1]][[i]]$SMI_mu_sd,
                                 slices[[1]][[i]]$SMI_cov)
    
    # Shrinking of SMI groups mean returns
    slices[[1]][[i]]$SMI_groups_mu_sd[,2] = (1 - r_shrink) * slices[[1]][[i]]$SMI_groups_mu_sd[,2] +
      r_shrink * mean(slices[[1]][[i]]$SMI_groups_mu_sd[,2])
    
    # Shrinking of SMI groups correlation
    cor = cov2cor(slices[[1]][[i]]$SMI_groups_cov)
    diag(cor) = 0
    cor = diag(nrow(cor)) + cor_shrink * cor
    slices[[1]][[i]]$SMI_groups_cov = diag(slices[[1]][[i]]$SMI_groups_mu_sd[,3]) %*% cor %*%
      diag(slices[[1]][[i]]$SMI_groups_mu_sd[,3])
    
    # Calculate SMI groups TP weights
    tpw_SMI_groups = markovitz_optim_tp(slices[[1]][[i]]$SMI_groups_mu_sd,
                                        slices[[1]][[i]]$SMI_groups_cov)
    
    # Apply TP weights to not optimized slice
    mu_return_SMI = c(as.matrix(slices[[2]][[i]]$SMI_returns[,-1]) %*% tpw_SMI)
    mu_return_SMI_groups = c(as.matrix(slices[[2]][[i]]$SMI_groups_returns[,-1]) %*% tpw_SMI_groups)
    
    # Combine not optimized slices
    out_of_sample_SMI = c(out_of_sample_SMI, mu_return_SMI)
    out_of_sample_SMI_groups = c(out_of_sample_SMI_groups, mu_return_SMI_groups)
  }
  
  # Calculate sharpe ratio
  sr_os_SMI = sharpe_ratio(mean(out_of_sample_SMI), sd(out_of_sample_SMI), 0, interval)
  sr_os_SMI_groups = sharpe_ratio(mean(out_of_sample_SMI_groups), sd(out_of_sample_SMI_groups), 0, interval)
  
  if (is.null(section)) {
    return(list("sr_os_SMI" = sr_os_SMI,
                "sr_os_SMI_groups" = sr_os_SMI_groups,
                "out_of_sample_SMI" = out_of_sample_SMI,
                "out_of_sample_SMI_groups" = out_of_sample_SMI_groups))
  }
  else {
    return(list("SMI_returns" = slices[[1]][[i]]$SMI_returns,
                "SMI_mu_sd" = slices[[1]][[i]]$SMI_mu_sd,
                "SMI_cov" = slices[[1]][[i]]$SMI_cov,
                "tpw_SMI" = tpw_SMI,
                "out_of_sample_SMI" = out_of_sample_SMI,
                "sr_os_SMI" = sr_os_SMI,
                "SMI_returns_groups" = slices[[1]][[i]]$SMI_groups_returns,
                "SMI_groups_mu_sd" = slices[[1]][[i]]$SMI_groups_mu_sd,
                "SMI_groups_cov" = slices[[1]][[i]]$SMI_groups_cov,
                "tpw_SMI_groups" = tpw_SMI_groups,
                "out_of_sample_SMI_groups" = out_of_sample_SMI_groups,
                "sr_os_SMI_groups" = sr_os_SMI_groups))
  }
}

out_of_sample_vec = Vectorize(out_of_sample, vectorize.args = c("r_shrink", "cor_shrink"),
                              SIMPLIFY = F)
