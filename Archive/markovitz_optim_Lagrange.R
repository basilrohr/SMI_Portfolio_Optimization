# This function performs the Markovitz optimization with the Lagrange multiplier.
# The inputs are a data frame with the columns mean return and volatility,
# the corresponding covariance matrix and the risk-free rate.
# By default, a plot of the efficiency frontier and capital market line is created.
# The outputs are expected return, volatility and weights of the minimum variance portfolio
# and tangency portfolio.

markovitz_optim_Lagrange = function(df, cov, Rf = 0, plot = T) {
  
  # Calculate sd (volatility) and mu (expected return) given weights, volatility,
  # mean return and covariance matrix
  sd_mu_calc = function(row, sd, mu, cov) {
    sum = 0
    for (i in 1:length(row)) {
      sum_j = 0
      for (j in 1:length(row)) {
        if (i != j) {
          sum_j = sum_j + row[i] * row[j] * cov[i,j]
        }
      }
      sum = sum + sum_j
    }
    sd_Y = sqrt(sum(row^2 * sd^2) + sum)
    mu_Y = sum(row * mu)
    return(c(sd_Y, mu_Y))
  }
  
  # Used by Lagrange multiplier to find weights of minimum variance portfolio given
  # volatility, mean_return and covariance matrix
  sd_mu_solve = function(x, sd, mu, cov) {
    sum = 0
    for (i in 1:length(x)) {
      sum_j = 0
      for (j in 1:length(x)) {
        if (i != j) {
          sum_j = sum_j + x[i] * x[j] * cov[i,j]
        }
      }
      sum = sum + sum_j
    }
    sd_Y = sum(x^2 * sd^2) + sum
    return(sd_Y)
  }
  
  # Condition for Lagrange multiplier to find minimum variance portfolio
  # (sum of all weights must be equal to 1)
  constraint1 = function(x, sd, mu, cov) {
    z1 = sum(x)
    return(z1)
  }
  
  # Conditions for Lagrange multiplier to calculate efficiency frontier
  # (sum of all weights must be equal to 1 and
  # sum of all weights times mean return equals an expected return)
  constraint2 = function(x, sd, mu, cov) {
    z1 = sum(x)
    z2 = sum(x * mu)
    return(c(z1, z2))
  }
  
  # Lagrange multiplier finds weights of minimum variance portfolio by minimizing volatility
  sd_mu_min_weights = solnp(pars = rep(0, nrow(df)), fun = sd_mu_solve, eqfun = constraint1,
                            eqB = 1, sd = df[,3], mu = NA, cov = cov,
                            control = list(trace = 0))$pars
  
  # Calculate volatility and expected return of minimum variance portfolio
  sd_mu_min_coord = sd_mu_calc(sd_mu_min_weights, sd = df[,3], mu = df[,2],
                               cov = cov)
  
  # Sequence of expected returns starting from minimum variance portfolio
  mu_exp = sd_mu_min_coord[2]
  seq = seq(mu_exp, mu_exp*3, length = 15)
  
  # For each value in sequence of expected returns Lagrange multiplier finds
  # minimal volatility which gives points on efficiency frontier and corresponding weights
  eff_frontier_weights = function(x) {
    solnp(pars = rep(0, nrow(df)), fun = sd_mu_solve, eqfun = constraint2, eqB = c(1, x),
          sd = df[,3], mu = df[,2], cov = cov,
          control = list(trace = 0))$pars
  }
  eff_frontier_coord = function(x) {
    sd_mu_calc(eff_frontier_weights(x), sd = df[,3], mu = df[,2], cov = cov)
  }
  eff_frontier_coord = t(sapply(seq, eff_frontier_coord))
  eff_frontier_weights = t(sapply(seq, eff_frontier_weights))
  
  # Stepwise approximation of tangent (capital market line)
  lin_eq = function(x) {
    slope = (eff_frontier_coord[x,2] - Rf) / (eff_frontier_coord[x,1])
    i = eff_frontier_coord[x+1,1]
    y = slope * i + Rf
    return(y > eff_frontier_coord[x+1,2])
  }
  tangent = sapply(seq(1:(length(eff_frontier_coord[,1])-1)), lin_eq)
  
  # Determine tangency portfolio volatility and expected return
  tang_index = which(tangent == T)[1]
  tang_port_coord = c(eff_frontier_coord[tang_index,1],
                      eff_frontier_coord[tang_index,2])
  
  # Determine tangency portfolio weights
  tang_port_weights = eff_frontier_weights[tang_index,]
  
  # Create plot of efficiency frontier and capital market line
  if (plot) {
    print(ggplot() +
            geom_point(aes(sd_mu_min_coord[1], sd_mu_min_coord[2])) +
            geom_line(aes(eff_frontier_coord[,1],eff_frontier_coord[,2])) +
            geom_point(aes(x = eff_frontier_coord[tang_index,1],
                           y = eff_frontier_coord[tang_index,2])) +
            geom_abline(slope = (eff_frontier_coord[tang_index,2] - Rf) /
                          (eff_frontier_coord[tang_index,1]), intercept = Rf) +
            labs(x = "Volatility in %", y = "Expected return in %"))
  }
  
  # Convert outputs to data frames for easier handling
  sd_mu_min_coord = as.data.frame(sd_mu_min_coord)
  sd_mu_min_weights = as.data.frame(sd_mu_min_weights)
  tang_port_coord = as.data.frame(tang_port_coord)
  tang_port_weights = as.data.frame(tang_port_weights)
  
  return(c(sd_mu_min_weights, sd_mu_min_coord, tang_port_weights, tang_port_coord))
}
