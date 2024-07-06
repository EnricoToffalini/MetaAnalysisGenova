

simulateStudy_r = function(n = 100, true_r = .20){
  x = mvrnorm(n=n, mu=c(0,0), Sigma=matrix(c(1,true_r,true_r,1),2,2),empirical=F)
  return(cor(x[,1],x[,2]))
}


z2SumScore = function (z = NA, nItems = 10, minResp = 1, maxResp = 4, itemZmin = -2, 
                       itemZmax = +2) {
  require(scales)
  levels = min(c(minResp, maxResp)):max(c(minResp, maxResp))
  if (minResp%%1 != 0 | maxResp%%1 != 0) 
    stop("both minResp and maxResp must be integer numbers")
  itemDiff = seq(itemZmin, itemZmax, length.out = nItems)
  resp = matrix(rep(z, length(nItems)), nrow = length(z), ncol = nItems)
  resp = t(t(resp) - itemDiff)
  resp = matrix(resp, nrow = length(z), ncol = nItems)
  resp = pnorm(resp) * (max(levels) - min(levels)) + min(levels)
  resp = round(rowSums(resp))
  return(resp)
}



