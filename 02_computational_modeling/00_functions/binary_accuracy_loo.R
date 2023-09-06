# 
optimal_cutoff = function(y, p_y, cutoffs = seq(.01, .99, .01)) {
  
  ac_cut = sapply(cutoffs, function(co)  {
    
    # predicted outcome
    y_pred = factor(ifelse(p_y > co, 1, 0), levels = 0:1)
    
    # confusion matirx
    cm = prop.table( table(y, y_pred), 1 )
    
    # accuracy by case
    ac_c = diag(cm); names(ac_c) = c('ac0', 'ac1')
    
    # accuracy
    ac = mean(ac_c); names(ac) = 'ac'
    
    #
    return(c(ac, ac_c))
    
  }); ac_cut = t(ac_cut)
  
  # determine the cutoff ----------------------------------------------------
  
  # differences between the accuracies
  acc_diff = function(x) sum( abs(x[1] - x[2]), abs(x[1] - x[3]), abs(x[2] - x[3]) )
  
  best_cutoff_index = which.min( apply(ac_cut, 1, acc_diff) )
  
  # best cutoff
  best_co = cutoffs[best_cutoff_index]
  
  #
  return(list(ac_cut = ac_cut, 
              best_co = best_co))
  
}

binary_accuracy_loo = function(fit, 
                               y,
                               binary_cutoff = 'optimal') {
  
  # log-liks
  ll = extract_log_lik(fit)
  r_eff = relative_eff(exp(-ll), chain_id = rep(1:4, each = nrow(ll)/4) )
  psis_object = psis(-ll, r_eff = r_eff)
  
  y_pred = exp(ll)
  y_pred[, y == 0] = 1 - y_pred[, y == 0]
  
  # balanced accuracy for binary outcome ------------------------------------
  
  # probablity of correct event classification
  y_loo = E_loo(y_pred,
                psis_object = psis_object,
                type = 'mean',
                log_ratios = -ll)$value
  
  
  # select the cutoff value to be used for prediction
  if(binary_cutoff == 'optimal') {
    
    co = optimal_cutoff(y, y_loo)$best_co
    
  } else {
    
    co = binary_cutoff
    
  }
  
  # accuracy by class
  ba_cat = diag( prop.table(table(y_loo > co, y), 2) )
  
  # balanced accuracy
  ba = mean(ba_cat)
  
  #
  res = list(ba = list(ba_cat = ba_cat, ba = ba, co = co))
  
  # OUTPUT
  return(res)
  
}