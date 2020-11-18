load("./Data/returns_1d.Rda")
stocks = colnames(returns[-1])
groups = list(Consumer = stocks[c(2, 9, 11, 15, 18)],
              Finance = stocks[c(3, 6, 16, 17, 19, 20)],
              Industrial = stocks[c(1, 4, 5, 7, 13)],
              Pharma = stocks[c(8, 10, 12, 14)])

groups_returns = function(returns, groups) {
  groups_returns = data.frame(matrix(nrow = nrow(returns), ncol = length(groups)+1,
                              dimnames = list(NULL, c("Date", names(groups)))))
  for (i in seq_along(groups)) {groups_returns[i+1] = rowMeans(returns[-1][groups[[i]]])}
  groups_returns[1] = returns[1]
  groups_returns
}

cov_mat = function(returns) {cov(returns[-1])}

cor_mat = function(returns) {cor(returns[-1])}

mean_returns = function(returns) {colMeans(returns[-1])}

volatilities = function(cov_mat) {sqrt(diag(cov_mat))}

se_mean = function(x) {sd(x) / sqrt(length(x))}

se_sd = function(x) {1 / sqrt(2 * (length(x)-1)) * sd(x)}

mvp_weights = function(cov_mat) {
  ones = rep(1, nrow(cov_mat))
  mu_inv = ones %*% solve(cov_mat) %*% ones
  mvp_weights = c(1/mu_inv) * solve(cov_mat) %*% ones
  c(mvp_weights)
}

tp_weights = function(cov_mat, mean_returns) {
  ones = rep(1, nrow(cov_mat))
  lambda_inv = ones %*% solve(cov_mat) %*% mean_returns
  tp_weights = c(1/lambda_inv) * solve(cov_mat) %*% mean_returns
  c(tp_weights)
}

pf_return = function(weights, mean_returns) {c(weights %*% mean_returns)}

pf_volatility = function(weights, cov_mat) {c(sqrt(weights %*% cov_mat %*% weights))}

ef_weights = Vectorize(function(mvp_weights, tp_weights, alpha){alpha * mvp_weights +
    (1-alpha) * tp_weights}, vectorize.args = "alpha")

ef_points = function(ef_weights, cov_mat, mean_returns) {
  x = apply(ef_weights, 2, function(x){pf_volatility(x, cov_mat)})
  y = apply(ef_weights, 2, function(x){pf_return(x, mean_returns)})
  cbind(x, y)
}

mvp_point = function(ef_points) {
  i = which.min(ef_points[,1])
  cbind(ef_points[,1][i], ef_points[,2][i])
}

tp_point = function(ef_points) {
  i = which.max(ef_points[,2] / ef_points[,1])
  cbind(ef_points[,1][i], ef_points[,2][i])
}

bootstrap = function(returns, n = 100) {
  samples_mvp_weights = samples_tp_weights = matrix(nrow = ncol(returns)-1, ncol = n)
  samples_ef_points = vector(mode = "list", length = n)
  for (i in 1:n) {
    sample = returns[sample(nrow(returns), replace = T),]
    cm = cov_mat(sample); mr = mean_returns(sample)
    mvpw = mvp_weights(cm); tpw = tp_weights(cm, mr)
    samples_mvp_weights[,i] = mvpw; samples_tp_weights[,i] = tpw
    samples_ef_weights = ef_weights(mvpw, tpw, seq(-1.5, 1.5, 0.1))
    samples_ef_points[[i]] = ef_points(samples_ef_weights, cm, mr)
  }
  weights_sd = function(x) {quantile(x, 0.84) - quantile(x, 0.5)}
  mvp_weights_sd = apply(samples_mvp_weights, 1, weights_sd)
  tp_weights_sd = apply(samples_tp_weights, 1, weights_sd)
  list(mvp_weights_sd = mvp_weights_sd, tp_weights_sd = tp_weights_sd,
       samples_ef_points = samples_ef_points)
}

ann_sharpe_ratio = function(sharpe_ratio, interval) {
  if (interval == "1d") {f = sqrt(252)}
  if (interval == "1wk") {f = sqrt(52)}
  if (interval == "1mo") {f = sqrt(12)}
  sharpe_ratio * f
}

in_sample = function(returns, interval = "1d") {
  tpw = tp_weights(cov_mat(returns), mean_returns(returns))
  weighted_returns = c(as.matrix(returns[-1]) %*% tpw)
  ann_sharpe_ratio(mean(weighted_returns) / sd(weighted_returns), interval)
}

cross_validation_sets = function(returns, n = 5) {
  sets = split(returns, rep(1:n, length.out = nrow(returns), each = ceiling(nrow(returns)/n)))
  training_sets = test_sets = vector(mode = "list", length = n)
  for (i in seq_along(sets)) {
    training_set = bind_rows(sets[-i]); test_set = bind_rows(sets[i])
    training_sets[[i]] = list(returns = training_set, cov_mat = cov_mat(training_set),
                              mean_returns = mean_returns(training_set))
    test_sets[[i]] = list(returns = test_set, cov_mat = cov_mat(test_set),
                          mean_returns = mean_returns(test_set))
  }
  list(training_sets = training_sets, test_sets = test_sets)
}

out_of_sample = function(sets, sfr = 0, sfcor = 1, interval = "1d", set = NULL) {
  if(is.null(set)) {seq = seq_along(sets[[1]])} else {seq = set}
  weighted_returns = c()
  for (i in seq) {
    covm = sets[[1]][[i]]$cov_mat; mr = sets[[1]][[i]]$mean_returns
    corm = cov2cor(covm); diag(corm) = 0
    scorm = diag(nrow(corm)) + sfcor * corm
    scovm = diag(volatilities(covm)) %*% scorm %*% diag(volatilities(covm))
    smr = sfr * mr + (1-sfr) * mean(mr)
    tpw = tp_weights(scovm, smr)
    weighted_returns_set = c(as.matrix(sets[[2]][[i]]$returns[-1]) %*% tpw)
    weighted_returns = c(weighted_returns, weighted_returns_set)
  }
  if(is.null(set)) {ann_sharpe_ratio(mean(weighted_returns) /sd(weighted_returns), interval)}
  else {
    list(sharpe_ratio = ann_sharpe_ratio(mean(weighted_returns) / sd(weighted_returns), interval),
         shrinking_cov_mat = scovm, shrinking_mean_returns = smr, tp_weights = tpw)
  }
}

out_of_sample_vec = Vectorize(out_of_sample, vectorize.args = c("sfr", "sfcor"), SIMPLIFY = F)
