library(ggplot2)
library(ggrepel)
library(dplyr)
library(ggcorrplot)

# Get code directory
dir = "."
# Load data and functions
load(paste0(dir, "/Data/SMI_returns_1d.Rda"))
R.utils::sourceDirectory(paste0(dir, "/Functions_optim"), modifiedOnly = F)


system.time(replicate(500, out_of_sample(t)))
system.time(replicate(500, out_of_sample(g)))



out_of_sample(g, set = 1)


gr = groups_returns(SMI_returns, groups)

t = cross_validation_sets(SMI_returns)
g = cross_validation_sets(gr)


out_of_sample(t)
