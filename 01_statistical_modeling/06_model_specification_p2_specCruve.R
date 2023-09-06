library(brms)
library(ggplot2)
library(patchwork)
library(data.table)

# models  -----------------------------------------------------------------

load('01_statistical_modeling/03_choice_info_neglect_spec_curve.RData') # v2 contains covid_vax_no---the results are the same

mm = m03_co_in_spec_curve
m_preds = c('ineg', 'b_pn', 'se_ext_pn', 'se_sev_pn', 'se_mild_pn')

rm(list = setdiff(ls(), c('m_preds', 'mm')))

# extract relevant coefficients -------------------------------------------

coefs_apn = lapply(mm, function(m) {
  
  fixef(m, 
        summary = F,
        pars = c('ineg1', 'ineg2',
                 paste0(m_preds[-1], '1')))
  
})


# summarize res fun -------------------------------------------------------

# x is a name of the coef of interest
coef_summ_fun = function(x) {
  
  # get models with the x coef
  rr = lapply(coefs_apn, function(y) {
    
    if( any( grepl(x, colnames(y)) )) {
      
      return(y)
      
    }
    
  }) 
  
  # rm the NULLs
  rr[sapply(rr, is.null)] = NULL
  
  # loop over the posteriors and get the quantiles
  coef_q = lapply(rr, function(y) {
    
    # get the posterior
    p = matrix( y[, grepl(x, colnames(y)) ],
                nrow = nrow(y) )
    
    # quantiles
    pq = t( apply(p, 2, quantile, probs = c(.025, .5, .975)) )
    
    # turn into odds ratios
    pq = exp(pq)
    
    # set up data frame
    if( nrow(pq) == 1 ) {
      
      d = data.frame(li = pq[1,1], me = pq[1,2], ui = pq[1,3],
                     row.names = NULL)
      
    } else {
      
      d = data.frame(li = pq[1,1], me = pq[1,2], ui = pq[1,3],
                     li2 = pq[2,1], me2 = pq[2,2], ui2 = pq[2,3],
                     row.names = NULL)
      
    }
    
    # add crediblity info
    d$cred = factor(ifelse(sign( log(d$li) ) == sign( log(d$ui) ), 'y', 'n'),
                    levels = c('n', 'y'),
                    ordered = T)
    
    # add predictor info
    for(pr in m_preds) {
      
      d[,pr] = ifelse( any( grepl(pr, colnames(y)) ), 1, 0 )
      
    }
    
    #
    return(d)
    
  }); coef_q = data.frame( rbindlist(coef_q) )
  
  # order by me/m1
  coef_q = coef_q[ order(coef_q[, 'me']), ]
  
  # add model no
  coef_q$model = factor(paste0('m', 1:16),
                        levels = paste0('m', 1:16),
                        ordered = T)
  
  # output
  return(coef_q)
  
}


# coefs summaries ---------------------------------------------------------

coef_q = lapply(m_preds, coef_summ_fun)
names(coef_q) = m_preds

# figure ------------------------------------------------------------------

# pred names
n_preds = c('info_neglect', 'apn_benefits', 'apn_extreme_se', 'apn_severe_se', 'apn_mild_se')
names(n_preds) = m_preds

spec_curves_plts = lapply(m_preds, function(pr) {
  
  # get data for the predictor
  xx = coef_q[[pr]]
  
  # top figure
  plt_top = ggplot(xx,
                   mapping = aes(x = model,
                                 y = me,
                                 ymin = li,
                                 ymax = ui,
                                 col = cred)) +
    geom_pointrange(size = .2) +
    theme_bw() +
    geom_hline(yintercept = 1, 
               linetype = 2) +
    scale_color_manual(values = c(n = rgb(.9, .1, .1, .6),
                                  y = rgb(.1, .1, .9, .6))) +
    scale_y_continuous(name = 'Odds ratio', 
                       limits = c(1/3, 3),
                       trans = 'log2') +
    guides(color = 'none') +
    ggtitle(n_preds[pr]) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())
  
  # 
  if(pr == 'ineg') {
    
    plt_top = plt_top +
      geom_pointrange(aes(y = me2, 
                          ymin = li2, 
                          ymax = ui2),
                      size = .2,
                      col = rgb(.5, .5, .5, .5))
    
  }
  
  # bottom figures
  btm_plts = lapply(m_preds, function(x) {
    
    # 
    xp = xx[, c(x, 'model')]
    colnames(xp) = c('x', 'model')
    
    # important!!
    xp$x = factor(xp$x,
                  levels = 0:1,
                  ordered = T)
    
    xp$y = x
    
    p = ggplot(data = xp,
               mapping = aes(x = model,
                             y = y,
                             col = x) ) +
      geom_point(shape = 3) +
      scale_color_manual('', values = c(rgb(1, 1, 1, 0), 'black')) +
      guides(color = 'none') +
      scale_y_discrete(x, expand = c(0, 0)) +
      theme_bw() +
      theme(axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.spacing = unit(c(0, 0, 0, 0), "null"))
    
    # set the main effect to prange
    if(x == pr) {
      
      p = p + geom_point(shape = 3,
                         col = 'orange')
      
    }
    
    if(pr != 'ineg') {
      
      p = p + theme(axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank())
      
    }
    
    # output
    return(p)
    
  }); btm_plts = wrap_plots(btm_plts, ncol = 1)
  
  # 
  if(pr != 'ineg') {
    
    plt_top = plt_top + theme(axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.y = element_blank())
    
  }
  
  # final figure
  plt = (plt_top / btm_plts) +
    plot_layout(heights = c(3, 2))
  
  #
  return(plt)
  
})

# final figure
fig_spec_curv = wrap_plots(spec_curves_plts, nrow = 1)

ggsave('04_online_supplement/02_statistical_modeling/model_specification_analyses/Fig_spec_curve.jpg',
       plot = fig_spec_curv,
       scale = 1.4,
       units = 'cm',
       width = 16,
       height = 4,
       dpi = 700)