rm(list = ls())

# libraries
library(ggplot2)
library(patchwork)
library(data.table)
library(viridis)
library(ggdist)

# modeling results
load("02_computational_modeling/posteriors/05_ispt_vc_irb_ct_oe.RData")

source('02_computational_modeling/00_functions/spt.R')

# useful variables --------------------------------------------------------

# group names
vax_g_n = c('anti', 'neu', 'pro')
vax_g_names = c('Anti', 'Neutral', 'Pro')
names(vax_g_names) = vax_g_n

# groups colors
pwf_cols = c(rgb(.1, .3, .6, 1),
             rgb(.9, .2, .1, 1))

v_cols = c(viridis::inferno(5, 1)[5],
           rgb(.3, .3, .3, 1))

# posteriors --------------------------------------------------------------

# get the posteriors
pp = lapply(names(ispt_oe), function(a) {
  
  m = ispt_oe[[a]]$pars$p_pars # modeling results
  
  # n samples
  n = nrow(m$gam)
  
  # set data frame
  d = data.frame(vax_at = a,
                 m$gam, m$beta, m$alpha, m$phi)
  
  colnames(d) = c('covid_vax_attitude', 
                  paste0('gam', 1:3), 
                  paste0('beta', 1:3), 
                  'alpha', 'phi')
  
  return(d)
  
}); pp = data.frame( rbindlist(pp) )

pp$beta_d = pp$beta1 - pp$beta2
pp$gam_d = pp$gam2 - pp$gam1

# value funs --------------------------------------------------------------

# # value function
# v_fun = function(x, a, l, p) {
# 
#   sv = sapply(x, function(y) {
# 
#     p * sign(y) * abs(y)^a * ifelse( y < 0, l/(1-l), 1)
#     # p * sign(y) * abs(y)^a * ifelse( y < 0, l, (1-l) )
# 
# 
#   })
#   
#   svv = sv * .2
#   
#   return(svv)
# 
# }

# ggplot
v_plts = lapply(c('anti', 'neu', 'pro'), function(at) {
  
  # get posteriors
  al_post = pp[ pp[,'covid_vax_attitude'] == at, c('alpha', 'beta1', 'beta2',
                                                   'phi')]
  
  # sample nic samples
  nic = 100
  al_post = al_post[sample(1:nrow(al_post), nic), ]
  
  p = ggplot(data = data.frame(x = seq(-5, 5, by = 1),
                               y = seq(-5, 5, by = 1)),
             mapping = aes(x = x, y = y)) +
    # ggtitle(paste0('AVF: ', vax_g_names[at])) +
    scale_x_continuous('x [affect rating]', 
                       breaks = -5:5,
                       limits = c(-5, 5),
                       minor_breaks = NULL
    ) +
    scale_y_continuous('v(x)',
                       breaks = -5:5,
                       limits = c(-4.5, 1.5),
                       minor_breaks = NULL) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    theme_bw() +
    theme(axis.line = element_line(colour = 'white'),
          panel.border = element_rect(fill = NA,
                                      colour = 'white')) +
    
    # add used curves
    lapply(1:nic, function(i) {
      
      vf = function(x) v_fun(x,
                             a = al_post[i, 'alpha'],
                             l = al_post[i, 'beta1'],
                             p = al_post[i, 'phi']) 
      
      line = geom_function(fun = vf,
                           colour = pwf_cols[2],
                           alpha = .1)
    }) +
    
    # add unsed curves
    lapply(1:nic, function(i) {
      
      vf = function(x) v_fun(x,
                             a = al_post[i, 'alpha'],
                             l = al_post[i, 'beta2'],
                             p = al_post[i, 'phi']) 
      
      line = geom_function(fun = vf,
                           colour = pwf_cols[1],
                           alpha = .1)
    }) +

    geom_line(aes(x = 100, y = 100,
                  colour = 'Side effects only')) +
    geom_line(aes(x = 100, y = 100,
                  colour = 'Side effects & protection')) +
    scale_color_manual(name = 'Mentioned outcomes',
                       values = pwf_cols) +
    theme(legend.position = c(.75, .25),
          legend.key = element_rect(fill = NA),
          # legend.title = element_blank(),
          legend.background = element_rect(fill = NA))
  
  # return
  return(p)
  
})
# v_plts[[2]] = v_plts[[2]] + guides(color = 'none')
# v_plts[[3]] = v_plts[[3]] + guides(color = 'none')

# save to file
lapply(2:3, function(x) {
  
  ggsave(paste0('EGPRO/FigOE_vf_', x, '.jpg'),
         plot = v_plts[[x]],
         units = 'cm',
         height = 9,
         width = 10,
         dpi = 700,
         scale = 1)
  
})

# pwfs --------------------------------------------------------------------

# pwf
pwf = function(p, d = 1, g) {
  
  if(g == 0) {
    return(1) 
  } else {
    
    return( exp(-d * (-log(p))^g) )
    
  }
  
} 

# figures
pwf_plts = lapply(c('anti', 'neu', 'pro'), function(at) {
  
  # get posteriors
  al_post = pp[ pp[,'covid_vax_attitude'] == at, c('gam1', 'gam2')]
  
  # sample nic samples
  nic = 100
  al_post = al_post[sample(1:nrow(al_post), nic), ]
  
  p = ggplot(data = data.frame(x = seq(0, 1, length.out = 10),
                               y = seq(0, 1, length.out = 10)),
             mapping = aes(x = x, y = y)) +
    geom_line(col = rgb(.5, .5, .5, .9),
              lty = 2) +
    # ggtitle(paste0('PWF: ', vax_g_names[at])) +
    scale_x_continuous('p', 
                       breaks = seq(0, 1, by = .2),
                       limits = 0:1) +
    scale_y_continuous('w(p)', 
                       breaks = seq(0, 1, by = .2),
                       limits = 0:1) +
    theme_bw() +
    
    # add individual curves
    
    lapply(1:nic, function(i) {
      
      pwf = function(x, 
                     g = al_post[i, 'gam1']) { exp(-(-log(x))^g) }
      
      line = geom_function(fun = pwf,
                           colour = pwf_cols[1],
                           alpha = .2)
    }) +
    
    lapply(1:nic, function(i) {
      
      pwf = function(x, 
                     g = al_post[i, 'gam2']) { exp(-(-log(x))^g) }
      
      line = geom_function(fun = pwf,
                           colour = pwf_cols[2],
                           alpha = .2)
    }) +
    
    
    geom_line(aes(x = 100, y = 100,
                  colour = 'Mentioned')) +
    geom_line(aes(x = 100, y = 100,
                  colour = 'Not mentioned')) +
    scale_color_manual(name = 'Probabilities',
                       values = pwf_cols) +
    theme(legend.position = c(.3, .8),
          legend.key = element_rect(fill = NA),
          # legend.title = element_blank(),
          legend.background = element_rect(fill = NA))
  
  
  # return
  return(p)
  
})
# pwf_plts[[2]] = pwf_plts[[2]] + guides(color = 'none')
# pwf_plts[[3]] = pwf_plts[[3]] + guides(color = 'none')

# save to file
# save to file
lapply(2, function(x) {
  
  ggsave(paste0('EGPRO/FigOE_pwf_', x, '.jpg'),
         plot = pwf_plts[[x]],
         units = 'cm',
         height = 9,
         width = 10,
         dpi = 700,
         scale = 1)
  
})

# density plots -----------------------------------------------------------

cc = viridis(3, .7)
names(cc) = c('anti', 'neu', 'pro')

# 
par_n = c('gam_d', 'beta_d')
par_tt = c('Difference in probability sensitivity', 
           'Difference in side effect aversion')
par_xx = c( '\u0394\u03B3', '\u0394\u03BB')
names(par_tt) = par_n; names(par_xx) = par_n; 

plp_plts = lapply(par_n, function(pn) {
  
  pp$y = pp[,pn]
  
  ggplot(data = pp,
         mapping = aes(x = y,
                       fill = covid_vax_attitude,
                       col = covid_vax_attitude)) +
    geom_histogram(bins = 30,
                   position = 'identity') +
    scale_fill_manual(values = cc,
                      labels = c('Anti', 'Neutral', 'Pro')) +
    scale_color_manual(values = cc) +
    guides(color = 'none') +
    scale_x_continuous(name = par_xx[pn]) +
    geom_vline(xintercept = 0,
               col = 'red',
               lty = 2) +
    ylab('') +
    # ggtitle(par_tt[pn]) +
    theme_bw() +
    theme(legend.position = c(.8, .9),
          legend.key = element_rect(fill = NA),
          legend.background = element_rect(fill = NA),
          legend.title = element_blank())
  
})
plp_plts[[1]] = plp_plts[[1]] + guides(fill = 'none')


# mini-conf figure --------------------------------------------------------

fig07_mc = wrap_plots(v_plts, nrow = 1, ncol = 3) /
  wrap_plots(pwf_plts, nrow = 1, ncol = 3)

rm(list = setdiff(ls(), c('fig07_mc', 'v_fun')))

save.image("results/Fig07_mc.RData")

# # param final figure ------------------------------------------------------
# 
# fig07b = (plp_plts[[2]] | v_plts[[1]] | v_plts[[2]]) / 
#   (plp_plts[[1]] | pwf_plts[[1]] | pwf_plts[[2]]) +
#   plot_annotation(tag_levels = 'a') &
#   theme(plot.tag.position = c(0, 1),
#         plot.tag = element_text(face = 'bold'))
# 
# rm(list = setdiff(ls(), c('fig07b', 'v_fun')))
# 
# # # save to file
# # ggsave('results/figures/Fig07b_oe.jpg',
# #        plot = fig07b,
# #        units = 'cm',
# #        height = 10,
# #        width = 16,
# #        dpi = 700,
# #        scale = 1.5)
# 
# save.image("results/Fig07b_.RData")