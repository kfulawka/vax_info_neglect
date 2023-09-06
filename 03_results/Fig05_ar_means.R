rm(list = ls())

library(ggplot2)
library(patchwork)
library(viridis)
library(brms)
library(data.table)
library(ggpp)

# modeling results --------------------------------------------------------

load("01_statistical_modeling/04_affect_ratings.RData")

# function for post hocs computation
source('03_results/functions/00_cov_at_postHoc.R')

# conditional effects -----------------------------------------------------

ce_dat = data.frame(covid_vax_attitude = c('Anti', 'Neutral', 'Pro'),
                    event = NA)

pp_dat = lapply(names(m04_ar), function(m) {
  
  # posterior predicted scores
  pp = posterior_epred(m04_ar[[m]],
                       newdata = ce_dat,
                       re_formula = NA)
  
  # get a weighted mean score for each row
  pa = data.frame( apply(pp, 1:2, function(x) sum(x * 1:5)) )
  colnames(pa) = c('Anti', 'Neutral', 'Pro')
  pa$id = 1:nrow(pa)
  
  # into long format
  pa = data.frame( melt(data.table(pa),
                        id.vars = 'id') )
  
  # 
  if(m != 'positive') {
    pa$value = pa$value * -1
  }
  
  # quantiles
  pa = aggregate(value ~ variable, 
                 data = pa,
                 FUN = function(x) quantile(x, c(.025, .5, .975)))
  pa = do.call(data.frame, pa)
  colnames(pa) = c('covid_vax_attitude', 'li', 'me', 'ui')
  
  # add data
  d = m04_ar[[m]]$data
  d$covid_vax_attitude = factor(d$covid_vax_attitude,
                                levels = c('Anti', 'Neutral', 'Pro'))
  
  da = aggregate(ar ~ covid_vax_attitude,
                 data = d,
                 FUN = mean)

  # 
  if(m != 'positive') {
    da$ar = da$ar * -1
  }
  
  # merge with model predictions
  pa = merge(pa, da, by = 'covid_vax_attitude')
  
  # 
  pa$outcome = m
  
  # post-hocs
  ph = cov_at_postHoc(m04_ar[[m]], 
                      nd = ce_dat,
                      comp = list(neu_anti = c(2, 1),
                                  pro_neu = c(3, 2)))[[1]];
  ph$covid_vax_attitude = c('Neutral', 'Pro')
  
  pa = merge(pa, ph[,c('covid_vax_attitude', 'li_d', 'ui_d')],
             by = c('covid_vax_attitude'),
             all = T)
  
  pa$cred = ifelse( sign(pa$li_d * pa$ui_d) == 1,
                    pa$me, NA)
  
  #
  return(pa)
  
}); pp_dat = data.frame(rbindlist(pp_dat))

pp_dat$outcome = factor(pp_dat$outcome,
                        levels = c('extreme', 'severe', 'mild', 'positive'),
                        labels = c('Extreme side effect', 'Severe side effect', 
                                   'Mild side effect', 'Benefits'),
                        ordered = T)

# figure ------------------------------------------------------------------

xt = c('', 'Vaccination', 'attitude', '')
names(xt) = levels(pp_dat$outcome)

Fig05s = lapply(levels(pp_dat$outcome), function(o) {
  
  p = ggplot(data = pp_dat[pp_dat$outcome == o, ],
         mapping = aes(x = covid_vax_attitude,
                       y = me,
                       ymin = li,
                       ymax = ui,
                       col = covid_vax_attitude,
                       group = covid_vax_attitude)) +
    geom_linerange() +
    geom_point(aes(shape = 'Model')) +
    geom_point(aes(y = ar,
                   shape = 'Data'),
               show.legend = F,
               alpha = .2) +
    geom_point(mapping = aes(y = cred,
                             shape = 'Credible\ndifference'),
               position = position_nudge(x = -.5),
               col = 'blue') +
    scale_color_manual(name = '',
                       values = viridis(3, .8)) +
    scale_shape_manual(values = c(6, 4, 3)) +
    labs(shape = '') +
    ggtitle(o) +
    guides(color = 'none') +
    theme_bw() +
    xlab(xt[[o]]) +
    theme(axis.title.y = element_text(size = 10),
          # axis.text.x = element_blank(),
          # axis.ticks.x = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(size = 10))
  
  if(o != 'Benefits') {
    
    p = p + 
      scale_y_continuous('Negative affect rating',
                         breaks = -5:-1,
                         limits = c(-5, -1)) +
      guides(color = 'none', shape = 'none')
    
    if(o != 'Extreme side effect') {
      
      p = p + theme(axis.title.y = element_blank(),
                    axis.text.y = element_blank())
      
    }
      
  } else {
    
    p = p + 
      scale_y_continuous('Positive affect rating',
                         breaks = 1:5,
                         limits = c(1, 5))
    
  }
  
  if(o == 'Severe side effect') p = p + theme(axis.title.x = element_text(hjust = 1.06))
  if(o == 'Mild side effect') p = p + theme(axis.title.x = element_text(hjust = -.06))
  
  return(p)
  
})

Fig05 = wrap_plots(Fig05s, nrow = 1, ncol = 4)

# save to file
ggsave('03_results/figures/Fig05.jpg',
       plot = Fig05,
       units = 'cm',
       height = 4,
       width = 16,
       dpi = 700,
       scale = 1.4)