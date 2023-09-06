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

accuracy_loo = function(fit, 
                        type = 'binary',
                        binary_cutoff = 'optimal') {
  
  # fit is the model fitted with brms
  
  # get the vector with the outcome var
  y = fit$data[, sub(' ~.*', '', as.character(fit$formula)[1]) ]
  y = as.numeric(y)
  
  # log-liks
  ll = log_lik(fit)
  r_eff = relative_eff(exp(-ll), chain_id = rep(1:4, each = 1000))
  psis_object = psis(-ll, r_eff = r_eff)
  
  y_pred = posterior_epred(fit)
  
  # balanced accuracy for binary outcome ------------------------------------
  if(type == 'binary') {
    
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
    
  }
  
  if(type == 'ordinal') {

    # balanced accuracy for ordinal outcome -----------------------------------
    
    # matrix for p(x|x observed)
    mm = matrix(NA, nrow = dim(y_pred)[1], ncol = dim(y_pred)[2])
    
    for(i in 1:dim(y_pred)[2]) {
      
      mm[,i] = y_pred[,i, y[i] ]
      
    }
    
    # probablity of correct event clasification
    y_loo = E_loo(mm,
                  psis_object = psis_object,
                  type = 'mean',
                  log_ratios = -ll)$value
    
    # nl - no. of levels; so under random: p(level) = 1/nl
    nl = length( unique(y) )
    
    # accuracy by class
    ba_cat = prop.table(table(y_loo > 1/nl, y), 2)['TRUE',]
    
    # balanced accuracy
    ba = mean(ba_cat)

    # average mean absolute error ---------------------------------------------
    
    # this not loo-based metric!!!
    
    # posterior prediction of the outcome  
    y_pr_p = apply(y_pred, 1:2, which.max)
    
    # the mode of the posterior prediction
    y_pr = apply(y_pr_p, 2, function(x){
      
      y = factor(x, levels = sort( unique(y) ),
                 ordered = T)
      
      t = table(y)
      
      which.max(t)
      
    })
    
    # mean absoulte error by class
    mae_cat = aggregate(abs(y - y_pr) ~ y, FUN = mean)
    
    # average MAE
    mae_a =  mean(mae_cat[,2])
    
    # ordinal result ----------------------------------------------------------
    
    res = list(ba = list(ba_cat = ba_cat, ba = ba),
               mae = list(mae_cat = mae_cat, mae_a = mae_a),
               co = 1/nl
               )
  }
  
  print(res)
  
  # OUTPUT
  return(res)
  
}