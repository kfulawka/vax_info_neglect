elpd_loo_comparison = function(ll) {
  
  # into lis
  # ll = list(m1 = m1, m0 = m0)

  # point-wise comparison ----------------------------------------------------
  ll_d = data.frame( sapply(ll, function(l) l$pointwise[,1]) )
  
  ll_d$m1_m0 = ll_d$m1 - ll_d$m0

  # overall elpd ------------------------------------------------------------

  elpds = sapply(ll, function(l)  { 
    
    elpd = l$estimate[1,'Estimate']
    
    # 95% confidence intervals
    cis =  l$estimate[1,'Estimate'] + c(-1.96, 1.96) * l$estimate[1,'SE'] 
    
    # output
    r = c(elpd, cis)
    names(r) = c('est', 'li', 'ui')
    
    return(r)
    
    }); elpds = data.frame( t(elpds) )
  
  elpds$model = names(ll)


  # elpd difference ---------------------------------------------------------
  
  elpd_d = loo_compare(ll)
    
  # se into 95% CI
  

  # output ------------------------------------------------------------------

  return(list(ll_d = ll_d, elpds = elpds, elpd_d = elpd_d))
  
}

# figure for a single comparison
elpd_comp_fig = function(elpd_comp, outcome_name = 'OUTCOME') {
  
  # pointwise comparison
  bp = ggplot(data = elpd_comp$ll_d, 
              mapping = aes(x = 1,
                            y = m1_m0)) +
    geom_jitter(alpha = .1,
                width = .05,
                col = rgb(.1, .1, .9)) +
    geom_boxplot(outlier.colour = NA,
                 fill = NA) +
    scale_y_continuous('elpd_loo_m1 - elpd_loo_m0',
                       breaks = seq(-10, 10, by = .5)) +
    geom_hline(yintercept = 0, col = 'red', lty = 2) +
    theme_bw() +
    ggtitle(outcome_name) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
  
  # elpds + elpd difference
  elpd_d = round(elpd_comp$elpd_d['m0', c('elpd_diff', 'se_diff')], 2)
  elpd_d = paste0('elpd_dif (SE) = ', elpd_d[1], ' (', elpd_d[2], ')')
  
  re = ggplot(data = elpd_comp$elpds,
              mapping = aes(x = model,
                            y = est,
                            ymin = li,
                            ymax = ui)) +
    geom_pointrange(alpha = .75) +
    ylab('elpd') +
    ggtitle(outcome_name,
            elpd_d) +
    theme_bw() 

  # final fig
  # fig = (bp | re) 
  
  # show only aggregate
  
  return(re)
}