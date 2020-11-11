# Sharpe ratio
sharpe_ratio = function(mu, sd, r_rf = 0, interval = "1d") {
  if (interval == "1d") {sf = sqrt(260)}
  if (interval == "1wk") {sf = sqrt(52)}
  if (interval == "1mo") {sf = sqrt(12)}
  return((mu - r_rf) / sd * sf)
}
