# https://finance.yahoo.com/quote/%5ESSMI/components?p=%5ESSMI
stocks = c("ABB", "Adecco", "Credit Suisse", "Geberit", "Givaudan",
           "Julius Baer", "LafargeHolcim", "Lonza", "Nestle", "Novartis",
           "Richemont", "Roche", "SGS", "Sika", "Swatch",
           "Swiss Life", "Swiss Re", "Swisscom", "UBS", "Zurich Insurance")

stock_symbols = c("ABBN.SW", "ADEN.SW", "CSGN.SW", "GEBN.SW", "GIVN.SW",
                  "BAER.SW", "LHN.SW", "LONN.SW", "NESN.SW", "NOVN.SW",
                  "CFR.SW", "ROG.SW", "SGSN.SW", "SIKA.SW", "UHR.SW",
                  "SLHN.SW", "SREN.SW", "SCMN.SW", "UBSG.SW", "ZURN.SW")

start_date = "2001-05-23" # Limited by LafargeHolcim
end_date = "2020-10-17"
interval = "1d" # Daily: 1d; Weekly: 1wk; Monthly: 1mo

# start_date_unix = as.numeric(as.POSIXct(start_date))
# end_date_unix = as.numeric(as.POSIXct(end_date))
# 
# for (i in 1:length(stocks)) {
#   url = paste0("https://query1.finance.yahoo.com/v7/finance/download/", stock_symbols[i],
#                "?period1=", start_date_unix, "&period2=", end_date_unix,
#                "&interval=", interval, "&events=history&includeAdjustedClose=true")
#   dest = paste0("./Data/", interval, "/", stocks[i], ".csv")
#   download.file(url, dest)
# }

for (i in 1:length(stocks)) {
  data = read.csv(paste0("./Data/", interval, "/", stocks[i], ".csv"),
                  na.strings = "null")[,c(1, 5)]
  if (i == 1) {df = data.frame(as.Date(data[,1]))}
  df = cbind(df, data[,2])
  # plot(x = as.Date(data[,1]), y = data[,2], type = "l", xlab = stocks[i], ylab = "Value")
}

colnames(df) = c("Date", stocks)
df = df[rowSums(is.na(df)) == 0,]

save(df, file = paste0("./Data/data_", interval, ".Rda"))
# load(paste0("./Data/data_", interval, ".Rda"))
