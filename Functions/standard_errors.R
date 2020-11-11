# Standard error of mean
se_mean = function(x) {sd(x)/sqrt(length(x))}

# Standard error of standard deviation
se_sd = function(x) {1/sqrt(2*(length(x)-1))*sd(x)}
