# This function slices the data in n slices which are used for the cross validation.
# The inputs are the SMI returns, stocks, group names and group compositions and the number of slices.
# The outputs are a the years to be optimized and the years to not be optimized.

slicer_old = function(SMI_returns, SMI_stocks, SMI_groups_names, SMI_groups, slices = 5) {
  
  # Slice data
  years = split(SMI_returns, format(SMI_returns$Date, "%Y"))
  seq = seq(1, length(years), by = length(years)/slices)
  
  # Run constructor function for every slice to obtain its returns, volatility and covariance
  years_optim = years_other = list()
  for (i in 1:length(seq)){
    slice = seq[i]:(seq[i]+(length(years)/slices-1))
    years_optim[[i]] = constructor(bind_rows(years[-slice]), SMI_stocks, SMI_groups_names, SMI_groups)
    years_other[[i]] = constructor(bind_rows(years[slice]), SMI_stocks, SMI_groups_names, SMI_groups)
  }
  
  return(list(years_optim, years_other))
}
