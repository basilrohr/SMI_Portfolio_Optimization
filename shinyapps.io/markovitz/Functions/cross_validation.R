slicer = function(SMI_returns, SMI_stocks, SMI_groups_names, SMI_groups) {
  
  slices = 5
  years = split(SMI_returns, format(SMI_returns$Date, "%Y"))
  seq = seq(1, length(years), by = length(years)/slices)
  
  years_optim = list()
  years_other = list()
  
  for (i in 1:length(seq)){
    slice = seq[i]:(seq[i]+(length(years)/slices-1))
    years_optim[[i]] = constructor(bind_rows(years[-slice]), SMI_stocks, SMI_groups_names, SMI_groups)
    years_other[[i]] = constructor(bind_rows(years[slice]), SMI_stocks, SMI_groups_names, SMI_groups)
  }
  
  return(list(years_optim, years_other))
}


sharpe_ratio = function(mu, sd, r_rf = 0) {
  (mu - r_rf) / sd
}


out_of_sample = function(slices) {

  out_of_sample_SMI = c()
  out_of_sample_SMI_groups = c()

  for (i in 1:length(slices[[1]])) {

    tpw_SMI = markovitz_optim(slices[[1]][[i]]$SMI_mu_sd,
                              slices[[1]][[i]]$SMI_cov)$tang_port_weights
    tpw_SMI_groups = markovitz_optim(slices[[1]][[i]]$SMI_groups_mu_sd,
                                     slices[[1]][[i]]$SMI_groups_cov)$tang_port_weights

    mu_return_SMI = c(as.matrix(slices[[2]][[i]]$SMI_returns[,-1]) %*% tpw_SMI)
    mu_return_SMI_groups = c(as.matrix(slices[[2]][[i]]$SMI_groups_returns[,-1]) %*% tpw_SMI_groups)

    out_of_sample_SMI = c(out_of_sample_SMI, mu_return_SMI)
    out_of_sample_SMI_groups = c(out_of_sample_SMI_groups, mu_return_SMI_groups)
  }

  sr_os_SMI = sharpe_ratio(mean(out_of_sample_SMI), sd(out_of_sample_SMI))
  sr_os_SMI_groups = sharpe_ratio(mean(out_of_sample_SMI_groups), sd(out_of_sample_SMI_groups))
  return(c(data.frame(sr_os_SMI), data.frame(sr_os_SMI_groups)))
}


in_sample = function(constructor) {
  
  tpw_SMI = markovitz_optim(constructor$SMI_mu_sd,
                            constructor$SMI_cov)$tang_port_weights
  tpw_SMI_groups = markovitz_optim(constructor$SMI_groups_mu_sd,
                                   constructor$SMI_groups_cov)$tang_port_weights
  
  in_sample_SMI = c(as.matrix(constructor$SMI_returns[,-1]) %*% tpw_SMI)
  in_sample_SMI_groups = c(as.matrix(constructor$SMI_groups_returns[,-1]) %*% tpw_SMI_groups)
  
  sr_is_SMI = sharpe_ratio(mean(in_sample_SMI), sd(in_sample_SMI))
  sr_is_SMI_groups = sharpe_ratio(mean(in_sample_SMI_groups), sd(in_sample_SMI_groups))
  return(c(data.frame(sr_is_SMI), data.frame(sr_is_SMI_groups)))
}
