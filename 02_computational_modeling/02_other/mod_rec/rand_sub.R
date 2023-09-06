rand_sub = function(dd, N = 30) {
  
  # get N random subjects
  sub_i = sort(sample(1:dd$N, N))
  
  # indices
  ii = as.vector( sapply(sub_i, function(s) which(dd$sub == s)) )
  
  # subset the data
  for(i in c('se', 'be', 'pse', 'pbe', 'se_g')) dd[[i]] = dd[[i]][ii, ]
  
  for(i in c('co', 'vax')) dd[[i]] = dd[[i]][ii]
  
  # set sub, n and N
  dd$N = N
  dd$n = N * 8
  dd$sub = rep(1:N, each = 8)
  
  # return data for stan
  return(dd)
  
}