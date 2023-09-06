# SPT components

# value function
v_fun = function(x, a, l, phi) {
  
  sv = sapply(x, function(y) {
    
    if(y > 0)  l = 1 - l 
    
    sign(y) * l * abs(y)^a
    
  })
  
  return(phi*sv)
  
}

# pwf
pwf = function(p, g, d = 1) { 
  
  wp = sapply(p, function(pp) {
    
    if(pp > 0) { exp(- d * (-log(pp))^g) } else { wp = .5}
    
  })

  return(wp)
  
}

# spt
spt = function(x, p, a, l, d = 1, g, phi) {
  
  # wp
  wp = pwf(p, g, d)
  
  # vx
  vx = v_fun(x, a, l, phi = 1) 
  
  # U
  U = phi * sum(vx * wp)
  
  #
  return(U)
  
}

# vax
# vax_rb = function(X = contr.sum(8), b) crossprod( t(X), b)

# inv-logit
softmax = function(x) { 1 / (1 + exp(-x)) }
