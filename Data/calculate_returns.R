# Load data and Rf
interval = "1d"
load(paste0("./Data/data_", interval, ".Rda"))
load(paste0("./Data/Rf_", interval, ".Rda"))

# Calculate log returns log(x_t / x_t-1) in percentage
df[,2:ncol(df)] = apply(df[,2:ncol(df)], 2, function(x){c(NA, diff(log(x))*100)})
df = df[-1,]

# Subtract risk-free rate from log returns
years = split(df, format(df$Date, "%Y"))
returns = data.frame()
for (i in 1:length(years)) {
  years[[i]][,2:ncol(df)] = years[[i]][,2:ncol(df)] - Rf[names(years[i])]
  returns = rbind(returns, years[[i]])
}

# Save data frame
save(returns, file = paste0("./Data/returns_", interval, ".Rda"))
