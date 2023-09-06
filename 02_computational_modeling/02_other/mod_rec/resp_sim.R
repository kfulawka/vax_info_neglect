sim_resp = function(alpha, beta, gamma, phi, pse_est, pbe_est, rbi, vax_b, 
                    se, be, pse, pbe, 
                    vax, se_g, 
                    # vax_X = contr.sum(8),
                    vax_X = cbind(c(-1, -1, 1, 0, -1, 1, 1, 0),
                                  c(-1, -1, 0, 1, -1, 0, 0, 1),
                                  c(1, -1, 0, 1, 1, 0, -1, -1),
                                  c(0, -1, 1, 0, 0, 1, -1, -1)),
                    model = 'pt') {
  
  # x, p, and pse are vectors of lenght 3
  # the rest are scalars
  
  # value function
  vf = function(a, b, ph, x) {
    
    r = sapply(x, function(y) {
      
      L = ifelse(y < 0, b, 1)
      
      # value fun
      u = ph * sign(y) * L * abs(y)^a
      
      return(u)
      
    })
    
  return(r)
    
  }
  
  # weighting function
  pwf = function(g, pe_se, pe_b, se_g, p, x) {
    
    w = sapply(1:length(p), function(ii) {
      
      if(x[ii] == 0) return( 0 )
      
      if(x[ii] != 0 & p[ii] > 0) {
          
          return( exp( -(-log(p[ii]))^g ) ) # inverse-S-shape pwf
          
        } else if (x[ii] < 0) {
          
          return( pe_se[se_g[ii]] ) # se group specific info-neg parameter
          
        } else if (x[ii] > 0) {
          
          return( pe_b ) # benefit info-neg parameter
          
        }
      
    })
    
    return( unlist(w) )
    
  }

  # response bias model -----------------------------------------------------

  if(model == 'rb') {
    
    lin_pred_rb = sum(vax_X[vax, ] * vax_b) + rbi
    
    return(  1 / ( 1 + exp(-lin_pred_rb) ) ) # accept probability
    
  }
  
  # outcome heuristic -------------------------------------------------------

  if(model == 'oh') {
    
    rb_sub = sum(vax_X[vax, ] * vax_b) + rbi
    se_sub = sum( vf(alpha, beta, phi, se) )
    be_sub = sum( vf(alpha, beta, phi, be) )
    lin_pred_oh = rb_sub + se_sub + be_sub
    
    return(  1 / ( 1 + exp(-lin_pred_oh) ) )
    
  }
  
  # prospect theory ---------------------------------------------------------
  
  if(model == 'pt') {
    
    rb_sub = sum(vax_X[vax, ] * vax_b) + rbi
    se_sub = sum( vf(alpha, beta, phi, se) * pwf(gamma, pse_est, 0, se_g, pse, se) )
    be_sub = sum( vf(alpha, beta, phi, be) * pwf(gamma, 0, pbe_est, se_g, pbe, be) )
    lin_pred_pt = rb_sub + se_sub + be_sub
    
    return(  1 / ( 1 + exp(-lin_pred_pt) ) )
    
  }
  
}