cov_at_postHoc = function(m, 
                          nd, 
                          comp = list(neu_anti = c(2, 1), pro_neu = c(3, 2),
                                      pro_anti = c(3, 1))) {
  
  # get predicted probs
  p = posterior_linpred(m,
                        newdata = nd,
                        re_formula = NA)
  
  # 
  pd = sapply(comp, function(x) {
                     
                     r = quantile(p[,x[1]] - p[,x[2]], c(.5, .025, .975) )
                     names(r) = c('me_d', 'li_d', 'ui_d')
                     
                     return(r)
                     
                   })
  pd = as.data.frame(t(pd)); pd$comp = rownames(pd)
  
  return( list(ph = pd, p = p) )
  
}