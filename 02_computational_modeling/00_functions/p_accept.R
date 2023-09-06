p_accept = function(il, y, ncp = 8) { 
  
  # ll: vector with log(p[choice])
  # choice: matrix with choice data, with one col per subject
  
  # event probability
  pa = exp(il) 
  
  # recode to p(accept)
  pa[y == 0] = 1 - pa[y == 0] 
  
  # into matrix
  pa = matrix(pa, nrow = ncp)
  y = matrix(y, nrow = ncp)
  
  # accept proportions across vaccines
  p_accept_vax = data.frame(model_loo = apply(pa, 1, mean),
                            observed = apply(y, 1, mean))
  
  # LOO accept proportions across individuals
  p_accept_ind = data.frame(model_loo = apply(pa, 2, mean),
                            observed = apply(y, 2, mean))  
  # output
  return(list(p_accept_vax = p_accept_vax,
              p_accept_ind = p_accept_ind))
  
}