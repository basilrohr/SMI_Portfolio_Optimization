library(ggplot2)
library(ggrepel)
library(dplyr)
library(ggcorrplot)
library(ggpubr)

load("./Data/returns_1d.Rda")
R.utils::sourceDirectory("./Code", modifiedOnly = F)


cor = cor_mat(returns)
d = dist(cor)
heatmap(as.matrix(d), symm = T)

par(mfrow = c(1,3))
hcsing = hclust(d, method = "single")
hcave = hclust(d, method = "average")
hccom = hclust(d, method = "complete")
plot(hcsing, main = "Single")
plot(hcave, main = "Average")
plot(hccom, main = "Complete")


order = match(unlist(groups), stocks)

r = returns
#r = returns[sample(nrow(returns), replace = T),]
gr = groups_returns(r, groups)
is_r = in_sample(r)
sets_r = cross_validation_sets(r)
sets_gr = cross_validation_sets(gr)
os_r = out_of_sample(sets_r)

os_r_sr = unlist(out_of_sample_vec(sets_r, seq(0, 1, 0.01)))
os_gr_sr = unlist(out_of_sample_vec(sets_gr, seq(0, 1, 0.01)))
gg_shrinking2D(os_r_sr, os_gr_sr, "SMI", "Groups", "Return", theme = custom_theme_markdown)


# Erkenntnisse:
# -  Normalverteilung wird vorausgesetzt, nochmals nachlesen in Breymann Folien
# -  wenn man Returns max. shrinkt (faktor 0), dann sind TP weights = MVP weights
#    was Sinn macht, weil dann alle Returns gleich sind und man so quasi nur nach
#    Volatilität das Portfolio zusammenstellt
# -  wenn zwei Aktien eine Cor von 1 bzw. -1 haben wird die Matrix singulär (dh det = 0) oder anders
#    gesagt lässt sich eine Aktie als Linearkombination einer anderen darstellen,
#    Cor nahe bei 1 bzw. -1 sind aus Computer rechnerischer Sicht auch schon heikel,
#    singulär bedeutet dass sie nicht invertiert werden kann -> markovitz optim geht nicht


a = cbind(c(1,2,-3), c(-1,-2,3),c(-4,10,-6))



#sets_r[[1]][[1]]$mean_returns = sets_r[[1]][[1]]$mean_returns - mean(sets_r[[1]][[1]]$mean_returns)


n = 20
start = -1
stop = 1
### Weights (max bei Shrinking Faktor 0.12 in Set 3 (also Zeile 13 in Output))
t1 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 1)
out1 = t(sapply(t1, function(x){return(x$tp_weights)}))
t2 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 2)
out2 = t(sapply(t2, function(x){return(x$tp_weights)}))
t3 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 3)
out3 = t(sapply(t3, function(x){return(x$tp_weights)}))
t4 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 4)
out4 = t(sapply(t4, function(x){return(x$tp_weights)}))
t5 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 5)
out5 = t(sapply(t5, function(x){return(x$tp_weights)}))
gg1 = ggplot()
for (i in 1:20) {gg1 = gg1 + geom_line(aes_string(x = seq(start, stop, 0.01), y = out1[,i]))}
gg1 = gg1 + labs(x = "Shrinkage factor", y = "Weights", title = "Model 1")
gg2 = ggplot()
for (i in 1:20) {gg2 = gg2 + geom_line(aes_string(x = seq(start, stop, 0.01), y = out2[,i]))}
gg2 = gg2 + labs(x = "Shrinkage factor", y = "Weights", title = "Model 2")
gg3 = ggplot()
for (i in 1:20) {gg3 = gg3 + geom_line(aes_string(x = seq(start, stop, 0.01), y = out3[,i]))}
gg3 = gg3 + labs(x = "Shrinkage factor", y = "Weights", title = "Model 3")
gg4 = ggplot()
for (i in 1:20) {gg4 = gg4 + geom_line(aes_string(x = seq(start, stop, 0.01), y = out4[,i]))}
gg4 = gg4 + labs(x = "Shrinkage factor", y = "Weights", title = "Model 4")
gg5 = ggplot()
for (i in 1:20) {gg5 = gg5 + geom_line(aes_string(x = seq(start, stop, 0.01), y = out5[,i]))}
gg5 = gg5 + labs(x = "Shrinkage factor", y = "Weights", title = "Model 5")
ggarrange(gg1, gg2, gg3, gg4, gg5)

### Returns
t1 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 1)
out1 = t(sapply(t1, function(x){return(x$shrinking_mean_returns)}))
t2 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 2)
out2 = t(sapply(t2, function(x){return(x$shrinking_mean_returns)}))
t3 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 3)
out3 = t(sapply(t3, function(x){return(x$shrinking_mean_returns)}))
t4 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 4)
out4 = t(sapply(t4, function(x){return(x$shrinking_mean_returns)}))
t5 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 5)
out5 = t(sapply(t5, function(x){return(x$shrinking_mean_returns)}))
gg1 = ggplot()
for (i in 1:20) {gg1 = gg1 + geom_line(aes_string(x = seq(start, stop, 0.01), y = out1[,i]))}
gg1 = gg1 + labs(x = "Shrinkage factor", y = "Weights", title = "Model 1")
gg2 = ggplot()
for (i in 1:20) {gg2 = gg2 + geom_line(aes_string(x = seq(start, stop, 0.01), y = out2[,i]))}
gg2 = gg2 + labs(x = "Shrinkage factor", y = "Weights", title = "Model 2")
gg3 = ggplot()
for (i in 1:20) {gg3 = gg3 + geom_line(aes_string(x = seq(start, stop, 0.01), y = out3[,i]))}
gg3 = gg3 + labs(x = "Shrinkage factor", y = "Weights", title = "Model 3")
gg4 = ggplot()
for (i in 1:20) {gg4 = gg4 + geom_line(aes_string(x = seq(start, stop, 0.01), y = out4[,i]))}
gg4 = gg4 + labs(x = "Shrinkage factor", y = "Weights", title = "Model 4")
gg5 = ggplot()
for (i in 1:20) {gg5 = gg5 + geom_line(aes_string(x = seq(start, stop, 0.01), y = out5[,i]))}
gg5 = gg5 + labs(x = "Shrinkage factor", y = "Weights", title = "Model 5")
ggarrange(gg1, gg2, gg3, gg4, gg5)

# Nenner Faktor
t1 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 1)
out1 = t(sapply(t1, function(x){return(rep(1, n) %*% solve(x$shrinking_cov_mat) %*% x$shrinking_mean_returns)}))
t2 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 2)
out2 = t(sapply(t2, function(x){return(rep(1, n) %*% solve(x$shrinking_cov_mat) %*% x$shrinking_mean_returns)}))
t3 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 3)
out3 = t(sapply(t3, function(x){return(rep(1, n) %*% solve(x$shrinking_cov_mat) %*% x$shrinking_mean_returns)}))
t4 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 4)
out4 = t(sapply(t4, function(x){return(rep(1, n) %*% solve(x$shrinking_cov_mat) %*% x$shrinking_mean_returns)}))
t5 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 5)
out5 = t(sapply(t5, function(x){return(rep(1, n) %*% solve(x$shrinking_cov_mat) %*% x$shrinking_mean_returns)}))
gg1 = ggplot() + geom_line(aes(x = seq(start, stop, 0.01), y = out1[1,])) + expand_limits(y = 0)
gg2 = ggplot() + geom_line(aes(x = seq(start, stop, 0.01), y = out2[1,])) + expand_limits(y = 0)
gg3 = ggplot() + geom_line(aes(x = seq(start, stop, 0.01), y = out3[1,])) + expand_limits(y = 0)
gg4 = ggplot() + geom_line(aes(x = seq(start, stop, 0.01), y = out4[1,])) + expand_limits(y = 0)
gg5 = ggplot() + geom_line(aes(x = seq(start, stop, 0.01), y = out5[1,])) + expand_limits(y = 0)
ggarrange(gg1, gg2, gg3, gg4, gg5)

# Kompletter Faktor
t1 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 1)
out1 = t(sapply(t1, function(x){return(1 / (rep(1, n) %*% solve(x$shrinking_cov_mat) %*% x$shrinking_mean_returns))}))
t2 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 2)
out2 = t(sapply(t2, function(x){return(1 / (rep(1, n) %*% solve(x$shrinking_cov_mat) %*% x$shrinking_mean_returns))}))
t3 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 3)
out3 = t(sapply(t3, function(x){return(1 / (rep(1, n) %*% solve(x$shrinking_cov_mat) %*% x$shrinking_mean_returns))}))
t4 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 4)
out4 = t(sapply(t4, function(x){return(1 / (rep(1, n) %*% solve(x$shrinking_cov_mat) %*% x$shrinking_mean_returns))}))
t5 = out_of_sample_vec(sets_r, seq(start, stop, 0.01), set = 5)
out5 = t(sapply(t5, function(x){return(1 / (rep(1, n) %*% solve(x$shrinking_cov_mat) %*% x$shrinking_mean_returns))}))
gg1 = ggplot() + geom_line(aes(x = seq(start, stop, 0.01), y = out1[1,])) + expand_limits(y = 0)
gg2 = ggplot() + geom_line(aes(x = seq(start, stop, 0.01), y = out2[1,])) + expand_limits(y = 0)
gg3 = ggplot() + geom_line(aes(x = seq(start, stop, 0.01), y = out3[1,])) + expand_limits(y = 0)
gg4 = ggplot() + geom_line(aes(x = seq(start, stop, 0.01), y = out4[1,])) + expand_limits(y = 0)
gg5 = ggplot() + geom_line(aes(x = seq(start, stop, 0.01), y = out5[1,])) + expand_limits(y = 0)
ggarrange(gg1, gg2, gg3, gg4, gg5)

# Cov mat inv
m1 = solve(sets_r[[1]][[1]]$cov_mat)
m2 = solve(sets_r[[1]][[2]]$cov_mat)
m3 = solve(sets_r[[1]][[3]]$cov_mat)
m4 = solve(sets_r[[1]][[4]]$cov_mat)
m5 = solve(sets_r[[1]][[5]]$cov_mat)

# Mean returns
r1 = sets_r[[1]][[1]]$mean_returns
r2 = sets_r[[1]][[2]]$mean_returns
r3 = sets_r[[1]][[3]]$mean_returns
r4 = sets_r[[1]][[4]]$mean_returns
r5 = sets_r[[1]][[5]]$mean_returns

(- rowSums(m3) %*% rep(mean(r3), 20)) / (rowSums(m3) %*% r3 - rowSums(m3) %*% rep(mean(r3), 20))
