# nagelkerke's R-squared
r_sq_nag = function(m1, m0, n) {
  
  r1 = 1 - exp(-2/n * (m1 - m0))
  r_max = 1 - exp(2/n * m0)
  r2 = r1 / r_max
  return(r2)
  
}

# # random model loglik
lpd_chance = function(n) sum(log( rep(.5, n) ))

# individual elpds
ind_elpd = function(ll, ncp = 8) {
  
  # lls into matrix
  llm = matrix(ll, nrow = ncp)
  
  # baseline lpd
  lpd_c = lpd_chance(ncp)
  
  
  # individual sum(ll)
  ill = colSums(llm)
  
  # # relative ind ll
  # ill_r = 1 - ill / lpd_c
  
  # expected proportion correct
  ev_prop_cor = colMeans(exp(llm))
  
  # r-squared
  r_sq_n = r_sq_nag(ill, lpd_c, ncp)
  
  # output
  res = data.frame(ill = ill,
                   # ill_r = ill_r,
                   ev_prop_cor = ev_prop_cor,
                   r_sq_n = r_sq_n)
  
}