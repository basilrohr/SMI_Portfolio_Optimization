library(ggplot2)
library(ggrepel)
library(dplyr)
library(ggcorrplot)

# Get code directory
dir = "/Dropbox/BA-Boyz/PA/Code"
if (Sys.info()["sysname"] == "Windows") {dir = paste0("C:/Users/", Sys.info()["user"], dir)}
if (Sys.info()["sysname"] == "Darwin") {dir = paste0("/Users/", Sys.info()["user"], dir)}
# Load data and functions
load(paste0(dir, "/Data/SMI_returns_1d.Rda"))
R.utils::sourceDirectory(paste0(dir, "/Functions_optim"), modifiedOnly = F)


# Weights
gr = groups_returns(SMI_returns, groups)

mr = mean_returns(SMI_returns)
covm = cov_mat(SMI_returns)
gmr = mean_returns(gr)
gcovm = cov_mat(gr)

mvpw = mvp_weights(covm)
gmvpw = mvp_weights(gcovm)

pf_return(mvpw, mr)
pf_volatility(mvpw, covm)
pf_return(gmvpw, gmr)
pf_volatility(gmvpw, gcovm)




seq = seq(0, 1, 0.01)
c = ef_weights(mvp_weights(a), tp_weights(a, b), seq)







