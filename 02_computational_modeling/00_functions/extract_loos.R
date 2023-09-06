library(loo)
library(data.table)

# -------------------------------------------------------------------------

# this function takes a list of models M as inputs 
# and output lists with loo objects
# grouped as: model|vax-att-group

# extract loos 
extract_loos = function(M, 
                        m_names = names(M),
                        g_names = c('anti', 'neu', 'pro')) {
  
  # mod names
  mod_names = names(M)
  names(mod_names) = m_names
  
  # group names
  gr_names = names(M[[1]])
  names(gr_names) = g_names
  
  # gett loos
  ll = lapply(gr_names, function(g) {
    
    ill = lapply(mod_names, function(nm) {
      
      m = M[[nm]][[g]]
      
      m$looE
      
    })
  })
  
  #
  return(ll)
  
}


# -------------------------------------------------------------------------

# elpds for plotting
# takes as input list of lists of loo and returns data frame 
elpds_plot = function(ll, 
                      relative = T,
                      mod_ord = 1:length(ll[[1]]) 
) {
  
  res = sapply(ll, function(m) { 
    
    # elpds
    elpds = c(elpd = m$estimates[1,1], 
              li = m$estimates[1,1] - 1.96 * m$estimates[1,2],
              ui = m$estimates[1,1] + 1.96 * m$estimates[1,2])
    
    # relative elpds for the figure
    if(relative) {
      
      # number of participants
      n  = nrow(m$pointwise)
      
      # relative lpd
      # elpds = 1 - elpds / sum( log(rep(.5, n)) )
      
      # average 
      elpds = exp(elpds / n)
      
    }
    
    # # r-sq
    # elpds = r_sq_nag(elpds, lpd_chance(n), n)
    
    return(elpds)
    
  })
  
  res = data.frame( t(res) )
  res$model = rownames(res)
  
  # 
  return(res)
  
}


# -------------------------------------------------------------------------

# elpd differences for plotting
# takes list with loos
# and list of comparisons to conduct within a group
elpd_diffs = function(l, 
                      comp_list,
                      rel = F) {
  
  # comparisons
  res = sapply(comp_list, function(ii) { 
    
    # elpd differences
    elpd_d = l[[ ii[1] ]]$estimates[1,1] - l[[ ii[2] ]]$estimates[1,1]
    
    # elpd diff se
    elpd_d_se = loo_compare(l[[ ii[1] ]], l[[ ii[2] ]])[2,2]
    
    # 
    res = c(elpd_d = elpd_d, 
            li1 = elpd_d - 1.28 * elpd_d_se,
            ui1 = elpd_d + 1.28 * elpd_d_se,
            li2 = elpd_d - 1.96 * elpd_d_se,
            ui2 = elpd_d + 1.96 * elpd_d_se,
            li3 = elpd_d - 2.58 * elpd_d_se,
            ui3 = elpd_d + 2.58 * elpd_d_se)
    
    if(rel) {
      
      # number of data points
      n = nrow(l[[1]]$pointwise)
      
      # scale elpd diff
      res = -res / sum(log(rep(.5, n)))
      
    }
    
    return(res)
    
  })
  
  res = data.frame( t(res) )
  res$model_comp = factor(names(comp_list),
                          ordered = T)
  # 
  return(res)
  
}


# # -------------------------------------------------------------------------
# 
# # loo model weights for plotting
# elpd_weights = function(l,
#                         mod_ord = 1:length(ll[[1]]) ) {
#   
#   w = loo_model_weights(l)
#   w = as.data.frame( as.matrix(w) )
#   
#   # add model info
#   w$model = factor(rownames(w),
#                    ordered = T,
#                    levels = rownames(w)[mod_ord])
#   # clear rownames
#   rownames(w) = NULL
#   
#   return(w)
#   
# }
