# Get code directory
dir = "/Dropbox/BA-Boyz/PA/Code"
if (Sys.info()["sysname"] == "Windows") {dir = paste0("C:/Users/", Sys.info()["user"], dir)}
if (Sys.info()["sysname"] == "Darwin") {dir = paste0("/Users/", Sys.info()["user"], dir)}

# Load data and Rf
interval = "1mo"
load(paste0(dir, "/Data/data_", interval, ".Rda"))
load(paste0(dir, "/Data/Rf_", interval, ".Rda"))

# Calculate log returns log(x_t / x_t-1) in percentage
df[,2:ncol(df)] = apply(df[,2:ncol(df)], 2, function(x){c(NA, diff(log(x))*100)})
df = df[-1,]

# Subtract risk-free rate from log returns
years = split(df, format(df$Date, "%Y"))
SMI_returns = data.frame()
for (i in 1:length(years)) {
  years[[i]][,2:ncol(df)] = years[[i]][,2:ncol(df)] - Rf[names(years[i])]
  SMI_returns = rbind(SMI_returns, years[[i]])
}

# Save data frame
save(SMI_returns, file = paste0(dir, paste0("/Data/SMI_returns_", interval, ".Rda")))
