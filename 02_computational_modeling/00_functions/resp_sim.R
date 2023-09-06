sim_resp = function(alpha, beta, gamma, phi, rb, vax_b, 
                    se, be, pse, pbe, 
                    vax,
                    # vax_X = contr.sum(8),
                    vax_X = cbind(c(-1, -1, 1, 0, -1, 1, 1, 0),
                                  c(-1, -1, 0, 1, -1, 0, 0, 1),
                                  c(1, -1, 0, 1, 1, 0, -1, -1),
                                  c(0, -1, 1, 0, 0, 1, -1, -1)),
                    model = 'pt') {
  
  # response bias model -----------------------------------------------------

  if(model == 'rb') {
    
    lin_pred_rb = sum(vax_X[vax, ] * vax_b) + rb
    
    return(  softmax(lin_pred_rb) ) 
    
  }
  
  # outcome heuristic -------------------------------------------------------

  if(model == 'oh') {
    
    rb_sub = sum(vax_X[vax, ] * vax_b) + rb
    se_sub = sum( v_fun(x = se, a = alpha, l = beta, phi = phi) )
    be_sub = sum( v_fun(x = be, a = alpha, l = beta, phi = phi) )
    lin_pred_oh = rb_sub + se_sub + be_sub
    
    return(  softmax(lin_pred_oh) ) 
    
  }
  
  # prospect theory ---------------------------------------------------------
  
  if(model == 'pt') {
    
    rb_sub = sum(vax_X[vax, ] * vax_b) + rb
    se_sub = sum( v_fun(x = se, a = alpha, l = beta, phi = phi) * pwf(p = pse, g = gamma) )
    be_sub = sum( v_fun(x = be, a = alpha, l = beta, phi = phi) * pwf(p = pbe, g = gamma) )
    lin_pred_pt = rb_sub + se_sub + be_sub
    
    return(  softmax(lin_pred_pt) ) 
    
  }
  
}