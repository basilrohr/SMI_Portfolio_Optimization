# Read SARON data file
# https://www.six-group.com/exchanges/indices/data_centre/swiss_reference_rates/reference_rates_en.html
data = read.csv("./Data/Risk-free rate/hsrron.csv", skip = 3, sep = ";")[,c(1, 2)]
data$Date = as.POSIXct(data$Date, format = "%d.%m.%Y")

# Split years
SARON_years = 7
years = split(data, format(data$Date, "%Y"))
years = years[-c(1:(length(years)-SARON_years))]

# Calculate SARON yearly mean rates
SARON_mean_rates = numeric(length(years))
for (i in 1:length(years)) {
  values = years[[i]]$Close
  SARON_mean_rates[i] = mean(values)
}
names(SARON_mean_rates) = rev(seq(2020, by = -1, length = SARON_years))

# LIBOR overnight yearly mean rates
# https://www.global-rates.com/en/interest-rates/libor/swiss-franc/2001.aspx
LIBOR_mean_rates = c(3.037, 1.083, 0.277, 0.357, 0.722, 1.354, 2.245, 2.035, 0.104, 0.064, 0.049,
                     0.016, -0.005, -0.008, -0.760, -0.780, -0.786, -0.784, -0.797, -0.791)
names(LIBOR_mean_rates) = seq(2001, 2020)

# Combine LIBOR and SARON and save vector
interval = "1d" # Daily: 1d; Weekly: 1wk; Monthly: 1mo
if (interval == "1d") {divisor = 252}
if (interval == "1wk") {divisor = 52}
if (interval == "1mo") {divisor = 12}
Rf = c(LIBOR_mean_rates[1:(length(LIBOR_mean_rates)-SARON_years)], SARON_mean_rates) / divisor
save(Rf, file = paste0("./Data/Rf_", interval, ".Rda"))
