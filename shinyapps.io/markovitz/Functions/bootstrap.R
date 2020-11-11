bootstrap = function(returns, stock_1 = 1, stock_2 = 2) {
  
  returns = timeSeries(data = returns[,2:ncol(returns)], charvec = returns[,1])
  
  bts.amount = 100
  bt.output_mvp = matrix(nrow = bts.amount, ncol = ncol(returns))
  bt.output_tp = matrix(nrow = bts.amount, ncol = ncol(returns))
  
  for (i in 1:bts.amount){
    bt.sample = sample(returns, size = nrow(returns), replace = TRUE)
    
    m_return = colMeans(bt.sample)
    sd_error = colSds(bt.sample)
    
    bt.SMI = data.frame(colnames(bt.sample),m_return,sd_error)
    colnames(bt.SMI) = c("Stock", "Mean_return", "Volatility")
    
    bt.cov = cov(bt.sample)
    
    bt.markovitz = markovitz_optim(bt.SMI, bt.cov)
    bt.output_mvp[i,] = bt.markovitz$sd_mu_min_weights
    bt.output_tp[i,] = bt.markovitz$tang_port_weights
  }
  
  bt.df_mvp = cbind(bt.output_mvp[,stock_1], bt.output_mvp[,stock_2])
  bt.df_mvp = as.data.frame(bt.df_mvp)
  bt.df_tp = cbind(bt.output_tp[,stock_1], bt.output_tp[,stock_2])
  bt.df_tp = as.data.frame(bt.df_tp)
  
  # Standard error weights
  bt.sd_mvp = numeric(ncol(bt.output_mvp))
  bt.sd_tp = numeric(ncol(bt.output_tp))
  for (i in 1:ncol(bt.output_mvp)){
    bt.sd_mvp[i] = quantile(bt.output_mvp[,i], 0.84) - quantile(bt.output_mvp[,i],0.5)
    bt.sd_tp[i] = quantile(bt.output_tp[,i], 0.84) - quantile(bt.output_tp[,i],0.5)
  }
  
  return(list("bt.df_mvp" = bt.df_mvp,
              "bt.df_tp" = bt.df_tp,
              "bt.sd_mvp" = bt.sd_mvp,
              "bt.sd_tp" = bt.sd_tp))
}
