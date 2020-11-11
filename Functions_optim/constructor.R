# The SMI stocks and groups are defined.

SMI_stocks = c("ABB", "Adecco", "Credit Suisse", "Geberit", "Givaudan",
               "Julius Baer", "LafargeHolcim", "Lonza", "Nestle", "Novartis",
               "Richemont", "Roche", "SGS", "Sika", "Swatch",
               "Swiss Life", "Swiss Re", "Swisscom", "UBS", "Zurich Insurance")

SMI_groups_names = c("Consumer", "Finance", "Industrial", "Pharma")

SMI_groups = list(assign(SMI_groups_names[1], SMI_stocks[c(2, 9, 11, 15, 18)]),
                  assign(SMI_groups_names[2], SMI_stocks[c(3, 6, 16, 17, 19, 20)]),
                  assign(SMI_groups_names[3], SMI_stocks[c(1, 4, 5, 7, 13)]),
                  assign(SMI_groups_names[4], SMI_stocks[c(8, 10, 12, 14)]))
names(SMI_groups) = SMI_groups_names

# This function calculates all parameters for the Markovitz optimization.
# The inputs are the SMI returns, stocks, group names and group compositions.
# The outputs are the SMI returns, mean returns, volatility and covariance matrix and
# the SMI groups returns, mean returns, volatility and covariance matrix.

constructor = function(SMI_returns, SMI_stocks, SMI_groups_names, SMI_groups) {
  
  # Calculate mean return and volatility per stock
  mu_return = colMeans(SMI_returns[,-1])
  volatility = sqrt(diag(cov(SMI_returns[,-1])))
  SMI_mu_sd = data.frame(colnames(SMI_returns[,-1]), mu_return, volatility)
  colnames(SMI_mu_sd) = c("Stock", "Mean return [%]", "Volatility [%]")
  rownames(SMI_mu_sd) = NULL
  
  # Calculate mean return and volatility per group
  SMI_groups_returns = data.frame(matrix(nrow = nrow(SMI_returns),
                                         ncol = length(SMI_groups_names)))
  SMI_groups_mu_sd = data.frame(matrix(nrow = length(SMI_groups_names), ncol = 3))
  for (i in 1:length(SMI_groups)) {
    SMI_groups[[i]] = match(SMI_groups[[i]], SMI_stocks)
    # Calculate mean return of group composition
    mean_return = tryCatch(rowMeans(SMI_returns[,-1][,SMI_groups[[i]]]),
                           error = function(cond){return(SMI_returns[,-1][,SMI_groups[[i]]])})
    SMI_groups_returns[,i] = mean_return
    # Calculate mean return and volatility of group
    SMI_groups_mu_sd[i, 1] = SMI_groups_names[i]
    SMI_groups_mu_sd[i, 2] = mean(mean_return)
    SMI_groups_mu_sd[i, 3] = sd(mean_return)
  }
  SMI_groups_returns = cbind(SMI_returns$Date, SMI_groups_returns)
  colnames(SMI_groups_returns) = c("Date", SMI_groups_names)
  colnames(SMI_groups_mu_sd) = c("Group", "Mean return [%]", "Volatility [%]")

  return(list("SMI_returns" = SMI_returns,
              "SMI_mu_sd" = SMI_mu_sd,
              "SMI_cov" = cov(SMI_returns[,-1]),
              "SMI_groups_returns" = SMI_groups_returns,
              "SMI_groups_mu_sd" = SMI_groups_mu_sd,
              "SMI_groups_cov" = cov(SMI_groups_returns[,-1])
              ))
}
