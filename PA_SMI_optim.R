library(ggplot2)
library(ggrepel)
library(dplyr)
library(ggcorrplot)

# Get code directory
dir = "."
# Load data and functions
load(paste0(dir, "/Data/SMI_returns_1d.Rda"))
R.utils::sourceDirectory(paste0(dir, "/Functions_optim"), modifiedOnly = F)






