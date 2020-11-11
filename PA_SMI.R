library(ggplot2)
library(ggrepel)
library(dplyr)
library(ggcorrplot)
library(quantmod)
library(tidyquant)

# Get code directory
dir = "/Dropbox/BA-Boyz/PA/Code"
if (Sys.info()["sysname"] == "Windows") {dir = paste0("C:/Users/", Sys.info()["user"], dir)}
if (Sys.info()["sysname"] == "Darwin") {dir = paste0("/Users/", Sys.info()["user"], dir)}
# Load data and functions
load(paste0(dir, "/Data/SMI_returns_1d.Rda"))
R.utils::sourceDirectory(paste0(dir, "/Functions"), modifiedOnly = F)

# Calculate all parameters
assign_list_output(constructor(SMI_returns, SMI_stocks, SMI_groups_names, SMI_groups))



tq_index_options()

t = tq_index("SP500")

a = tq_get("FB",
       get = "stock.prices",
       from = "2016-01-01",
       to = "2016-12-31",
       periodicity = "weekly")

