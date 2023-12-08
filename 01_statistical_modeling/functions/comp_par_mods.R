# function to visually compare coefficients from main and full models
# the list should contain two models named
# m_full and m_main

# betas to odds ratios 
bor_mod = function(m, or = F) {
  
  ff = summary(m)$fixed
  
  # parameter names as a variable
  ff$par = rownames(ff)
  
  # get only the relevant columns
  ff = ff[, c('Estimate', "l-95% CI", "u-95% CI", 'par')]
  colnames(ff)[1:3] = c('m', 'li', 'ui')
  
  # exclude intercept
  ff = ff[ff$par != 'Intercept', ]
  
  # change sign of inegs (for fig 4a, so they show odds ratio for refusal instead of acceptance)
  ff[ff$par == 'ineg1', c('m', 'li', 'ui')] = ff[ff$par == 'ineg1', c('m', 'li', 'ui')] * -1
  ff[ff$par == 'ineg2', c('m', 'li', 'ui')] = ff[ff$par == 'ineg2', c('m', 'li', 'ui')] * -1
  
  # transform into odds ratio
  if(or) { ff[,1:3] = exp( ff[,1:3] ) }
  
  # return the table back
  return(ff)
  
}

# get model parameters summary
par_mods = function(mm) {
  
  # get post summaries
  ps = lapply(names(mm), function(n) {
    
    m = mm[[n]] # get the model
    
    ss = summary(m) # parameter summary
    
    f = ss$fixed
    r = ss$random$sub
    
    f$par_t = 'fixed'
    r$par_t = 'random'
    
    ff = rbind(f, r) # fixed and random effects
    
    # add model type indicator
    ff$mod = n
    
    # parameter names as a variable
    ff$par = rownames(ff)
    
    # get only the relevant columns
    ff = ff[, c('Estimate', "l-95% CI", "u-95% CI", 'mod', 'par', 'par_t')]
    colnames(ff)[1:3] = c('m', 'li', 'ui')
    
    if(n == 'm_full') {
      
      # set factor levels
      ff$par = factor(ff$par,
                      levels = ff$par,
                      ordered = T)
      
    }

    #
    return(ff)
    
  })
  
  # combine summaries
  d = data.frame( rbindlist(ps) )
  
  return(d)
  
}

# plot the summary
plot_comp_par_mods = function(ff, outcome = 'OUTCOME') {
  
  ggplot(data = ff[-grep('Intercept', ff$par), ],
         mapping = aes(x = m,
                       xmin = li,
                       xmax = ui,
                       y = par,
                       group = mod,
                       col = mod)) +
    geom_pointrange(position = position_dodge(.75)) +
    scale_color_manual(name = 'Individual and demographic vars',
                       values = viridis::cividis(2, .7),
                       labels = c('Included', 'Not included')) +
    ylab('Coefficient') +
    xlab('Estimate') +
    geom_vline(xintercept = 0, col = 'red') +
    ggtitle(outcome) +
    theme_bw() +
    theme(legend.position = 'bottom')
  
}

# wrapper
comp_par_mods = function(mm, outcome) {

  library(data.table)
  library(ggplot2)
  library(viridis)
    
  ff = par_mods(mm)
  plt = plot_comp_par_mods(ff, outcome)
  
  return(plt)
  
}
