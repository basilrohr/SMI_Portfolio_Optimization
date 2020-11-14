interval = "1d"
load(paste0("./Data/data_", interval, ".Rda"))
load(paste0("./Data/rf_", interval, ".Rda"))

df[,2:ncol(df)] = apply(df[,2:ncol(df)], 2, function(x){c(NA, diff(log(x))*100)})
df = df[-1,]

years = split(df, format(df$Date, "%Y"))
returns = data.frame()
for (i in 1:length(years)) {
  years[[i]][,2:ncol(df)] = years[[i]][,2:ncol(df)] - rf[names(years[i])]
  returns = rbind(returns, years[[i]])
}

save(returns, file = paste0("./Data/returns_", interval, ".Rda"))
