nuisance.est <-
function(training.sample){

mu.hat = colMeans(training.sample)

training.sample = t(t(training.sample)-mu.hat)
M.hat = M.est(training.sample,M_threshold=0.05)
cor.hat = cor.est(M.hat,training.sample)

result = list()
result$mu.hat = mu.hat
result$M.hat = M.hat
result$cor.hat = cor.hat

return(result)
}
