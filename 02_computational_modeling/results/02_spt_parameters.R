rm(list = ls())

# libraries
library(ggplot2)
library(patchwork)
library(data.table)
library(viridis)
library(ggdist)

# data
load("01_statistical_modeling/data_analyses.RData")

# data for modeling
rm(list = setdiff(ls(), 'md'))

source('02_computational_modeling/00_functions/spt.R')

# modeling results
load("02_computational_modeling/posteriors/01_ispt_vc_irb_ct.RData")

# useful variables --------------------------------------------------------

# group names
vax_g_n = c('anti', 'neu', 'pro')
vax_g_names = c('Anti', 'Neutral', 'Pro')
names(vax_g_names) = vax_g_n; names(vax_g_n) = vax_g_n

# vax groups colors
vax_col = viridis(3, .1)
names(vax_col) = vax_g_n

# Model to plot ----------------------------------------------------------

mm_plt = ispt
rm(ispt)

# posteriors --------------------------------------------------------------

pp = lapply(vax_g_n, function(g) {
  
  m = mm_plt[[g]]$pars$p_pars
  
  res = data.frame( m[c('alpha', 'beta', 'gam', 'phi')] )
  res$covid_vax_attitude = g
  
  return(res)
  
}); pp = data.frame( rbindlist(pp) )

# medians
me_p = aggregate(. ~  covid_vax_attitude, 
                 data = pp,
                 FUN = median)
rownames(me_p) = me_p$covid_vax_attitude

# individual level parameters
ip = lapply(vax_g_n, function(g) {

  m = mm_plt[[g]]$pars$i_pars[c('igam', 'ibeta')]

  res = data.frame( sapply(m, colMeans) )

  res$covid_vax_attitude = g

  return(res)

}); ip = data.frame( rbindlist(ip) )

# posterior densities -----------------------------------------------------

# 
par_d_plt = c('gam', 'beta')
par_tt = c('Probability weighting', 'Loss aversion')
par_xx = c('\u03B3', '\u03BB')
# par_xx = c(bquote(gamma), bquote(lambda))

names(par_tt) = par_d_plt; names(par_xx) = par_d_plt

plp_plts = lapply(par_d_plt, function(p) {
  
  # set data
  d_pl = pp[,c('covid_vax_attitude', p)]
  colnames(d_pl)[2] = 'y'
  
  # id-lvl data
  i_pl = ip[,c('covid_vax_attitude', paste0('i', p))]
  colnames(i_pl)[2] = 'y'
  
  # # rescale lambda
  # if(p == 'beta') {
  #   
  #   d_pl$y = d_pl$y / (1 - d_pl$y)
  #   i_pl$y = i_pl$y / (1 - i_pl$y)
  #   
  # }
  
  plt = ggplot(d_pl,
               mapping = aes(x = covid_vax_attitude,
                             y = y)) +
    stat_pointinterval(mapping = aes(color = covid_vax_attitude,
                                     fill = covid_vax_attitude),
                       position = position_nudge(x = -.07),
                       point_color = NA) +
    stat_histinterval(data = i_pl, 
                      aes(x = covid_vax_attitude,
                          y = y,
                          fill = covid_vax_attitude), 
                      breaks = 'scott',
                      outline_bars = F,
                      slab_color = NA,
                      normalize = 'all',
                      position = position_dodge(1),
                      point_color = NA,
                      interval_color = NA,
                      alpha = .4) +
    scale_x_discrete(name = 'Vaccination attitude',
                     labels = c('Anti', 'Neutral', 'Pro'),
                     expand = c(.1, 0)) +
    scale_fill_manual(values = viridis(3, .7)) +
    scale_color_manual(values = viridis(3, .7)) +
    ggtitle(par_tt[p]) +
    theme_bw() +
    theme(legend.position = 'none',
          plot.title = element_text(hjust = .5))
  
  if(p == 'beta') plt = plt + geom_hline(yintercept = .5, lty = 2) + theme(axis.title.x = element_blank())
  
  return(plt)
  
})

plp_plts[[1]] = plp_plts[[1]] + scale_y_continuous(bquote(gamma),
                                                   breaks = seq(0, 1, .2),
                                                   limits = c(0, NA))


la_b = c(.1, .2, 1/3, .5, 2/3, .75, .8, 6/7)
plp_plts[[2]] = plp_plts[[2]] + scale_y_continuous(bquote(lambda),
                                                   breaks = la_b,
                                                   labels = round(la_b / (1 - la_b), 2),
                                                   limits = c(.35, .93))

# frequencies -------------------------------------------------------------

ml = md[,c('sub', 'vax', 'covid_vax_attitude',
           'SE1', 'SE2', 'SE3', 'CRE1', 'CRE2', 'CRE3',
           'P_SE1', 'P_SE2', 'P_SE3', 'P_CRE1', 'P_CRE2', 'P_CRE3')]

# 

ml = melt( data.table(ml),
           id.vars = c('sub', 'vax', 'covid_vax_attitude'), 
           measure.vars = list(o = c('SE1', 'SE2', 'SE3', 'CRE1', 'CRE2', 'CRE3'),
                               p = c('P_SE1', 'P_SE2', 'P_SE3', 'P_CRE1', 'P_CRE2', 'P_CRE3')))
ml = data.frame(ml)

# 
ml$o[ml$vax %in% sort(unique(ml$vax))[c(2, 4, 7)] & ml$variable == 6] = NA
ml$p[ml$vax %in% sort(unique(ml$vax))[c(2, 4, 7)] & ml$variable == 6] = NA

# # remove unseen ps and os
# ml$o[ml$o == 0] = NA
# ml$p[ml$p == .5] = NA

# set unseen Ps to .5
ml$p[ml$p == 0] = .5

# frequencies
fr = lapply(c('o', 'p'), function(v) {
  
  res = data.frame( prop.table( table(ml$covid_vax_attitude, ml[,v]), 1) )
  colnames(res) = c('covid_vax_attitude', 'v', 'freq')
  
  res[,'v'] = as.numeric(as.character(res[,'v']))
  
  res$covid_vax_attitude = factor(res$covid_vax_attitude,
                                  levels = c('Anti', 'Neutral', 'Pro'),
                                  labels = c('anti', 'neu', 'pro'))
  
  return(res)
  
}); names(fr) = c('o', 'p')

# outcome frequencies
o_fr = fr$o
o_fr$og = cut(o_fr$v, 
              breaks = c(-6, -1, 0, 6),
              labels = c('a', 'b', 'c'))

# prob frequencies
p_fr = fr$p
p_fr$og = cut(p_fr$v, 
              breaks = c(-.1, .2, .5, 1),
              labels = c('a', 'b', 'c'))



# value funs --------------------------------------------------------------

# median-est based subjective outcomes
o_fr$sv = NA
for(i in 1:nrow(o_fr)) {
  
  o_fr$sv[i] = v_fun(o_fr$v[i],
                     l = me_p$beta[o_fr$covid_vax_attitude[i]],
                     a = me_p$alpha[o_fr$covid_vax_attitude[i]],
                     phi = me_p$phi[o_fr$covid_vax_attitude[i]])
}

# percentages of uninspected outcomes
o_neg = o_fr[o_fr$v == 0, ]
o_neg$ll = round(o_neg$freq, 2)

# ggplot
v_plts = lapply(c('anti', 'neu', 'pro'), function(at) {
  
  # get posterior for a gve grop
  al_post = pp[ pp[,'covid_vax_attitude'] == at, c('alpha', 'beta', 'phi')]
  
  # sample nic samples
  nic = 5e2
  al_post = al_post[sample(1:nrow(al_post), nic), ]
  
  p = ggplot(data = data.frame(x = seq(-5, 5, by = 1),
                               y = seq(-5, 5, by = 1)),
             mapping = aes(x = x, y = y)) +
    # geom_abline(col = rgb(.5, .5, .5, .9),
    #             slope = me_p[at, 'phi'], 
    #             intercept = 0) +
    ggtitle(paste0('VF: ', vax_g_names[at])) +
    ylim(-4.5, 2) +
    scale_x_continuous('a [affect rating]', 
                       breaks = -5:5,
                       minor_breaks = NULL
    ) +
    geom_hline(yintercept = 0,
               color = rgb(.2, .2, .2, .5)) +
    geom_vline(xintercept = 0,
               color = rgb(.2, .2, .2, .5)) +
    ylab('v(a)') +
    theme_bw() +
    # add pop-lvl curves
    lapply(1:nic, function(i) {
      
      vf = function(x) v_fun(x,
                             a = al_post[i, 'alpha'],
                             l = al_post[i, 'beta'],
                             p = al_post[i, 'phi']) 
      
      line = geom_function(fun = vf,
                           colour = vax_col[at],
                           alpha = .07)
    }) +
    
    # add median curve
    geom_function(fun = function(x) v_fun(x,
                                          a = me_p[at, 'alpha'],
                                          l = me_p[at, 'beta'],
                                          p = me_p[at, 'phi']),
                  colour = rgb(1, 1, 1, .8),
                  lwd = 1) +
    
    # add frequencies
    geom_point(data = o_fr[o_fr$covid_vax_attitude == at, ],
               mapping = aes(x = v,
                             y = sv,
                             size = freq,
                             fill = og),
               shape = 21,
               colour = rgb(1, 1, 1, .1),
               show.legend = T,
               alpha = .7) +
    scale_size(range = c(1, 3)) +
    scale_fill_manual(values = c(rgb(1, .1, .1, .7),
                                 rgb(.5, .5, .5, .7),
                                 rgb(.1, .1, 1, .7)),
                      labels = c('Side effects', 'Ignored', 'Benefits')) +
    

    geom_text(data = o_neg[o_neg$covid_vax_attitude == at, ],
              mapping = aes(x = -1,
                            y = .7,
                            label = ll),
              size = 3,
              col = rgb(.5, .5, .5, 1)) +
    guides(size = 'none') +
    theme(legend.background = element_rect(fill = NA),
          legend.key = element_rect(fill = NA),
          legend.title = element_blank(),
          legend.position = c(.7, .3))
  
  if(at != 'anti') { p = p + guides(fill = 'none')}
  
  # 
  
  # return
  return(p)
  
})

# with indvidual...
iv_plts = lapply(c('anti', 'neu', 'pro'), function(at) {

  # get posterior for average grop
  ibetas = ip[ip$covid_vax_attitude == at, 'ibeta']

  p = ggplot(data = data.frame(x = seq(-5, 5, by = 1),
                               y = seq(-5, 5, by = 1)),
             mapping = aes(x = x, y = y)) +
    ggtitle(' ') +
    ylim(-4.5, 2) +
    scale_x_continuous('x (affect rating)',
                       breaks = -5:5,
                       minor_breaks = NULL
    ) +
    geom_hline(yintercept = 0,
               color = rgb(.2, .2, .2, .5)) +
    geom_vline(xintercept = 0,
               color = rgb(.2, .2, .2, .5)) +
    ylab('v(x)') +
    theme_bw() +

    # add ind-lvl curves
    lapply(1:length(ibetas), function(i) {

      vf = function(x) v_fun(x,
                             a = me_p[at, 'alpha'],
                             l = ibetas[i],
                             p = me_p[at, 'phi'])

      line = geom_function(fun = vf,
                           colour = vax_col[at],
                           alpha = .1)
    }) +

    # add median curve
    geom_function(fun = function(x) v_fun(x,
                                          a = me_p[at, 'alpha'],
                                          l = me_p[at, 'beta'],
                                          p = me_p[at, 'phi']),
                  colour = rgb(1, 1, 1, .8),
                  lwd = 1) +

    # add frequencies
    geom_point(data = o_fr[o_fr$covid_vax_attitude == at, ],
               mapping = aes(x = v,
                             y = sv,
                             size = freq,
                             fill = og),
               shape = 21,
               colour = rgb(1, 1, 1, .1),
               show.legend = T,
               alpha = .7) +
    scale_size(range = c(1, 3)) +
    scale_fill_manual(values = c(rgb(1, .1, .1, .7),
                                 rgb(.5, .5, .5, .7),
                                 rgb(.1, .1, 1, .7)),
                      labels = c('Side effects', 'Info neglect', 'Benefits')) +

    #
    geom_text(data = o_neg[o_neg$covid_vax_attitude == at, ],
              mapping = aes(x = -1,
                            y = .7,
                            label = ll),
              size = 3,
              col = rgb(.5, .5, .5, 1)) +
    guides(size = 'none') +
    theme(legend.background = element_rect(fill = NA),
          legend.key = element_rect(fill = NA),
          legend.title = element_blank(),
          legend.position = c(.7, .3))

  if(at != 'anti') { p = p + guides(fill = 'none')}

  #

  # return
  return(p)

})

# pwfs --------------------------------------------------------------------

# pwf
pwf = function(p, d = 1, g) exp(-d * (-log(p))^g)

#
p_fr$sv = NA
for(i in 1:nrow(p_fr)) {
  
  p_fr$sv[i] = pwf(p_fr$v[i],
                   g = me_p$gam[p_fr$covid_vax_attitude[i]])
  
}

# w(p) = 5 for unseen probs
p_fr$sv[p_fr$v == .5] = .5

# 
p_neg = p_fr[p_fr$v == .5, ]
p_neg$ll = round(p_neg$freq, 2)

# figures
pwf_plts = lapply(c('anti', 'neu', 'pro'), function(at) {
  
  # get posteriors
  dg_post = pp[ pp[,'covid_vax_attitude'] == at, 'gam']

  # sample nic samples
  nic = 5e2
  dg_post = dg_post[sample(length(dg_post), nic)]
  
  p = ggplot(data = data.frame(x = seq(0, 1, length.out = 10),
                               y = seq(0, 1, length.out = 10)),
             mapping = aes(x = x, y = y)) +
    geom_line(col = rgb(.5, .5, .5, .9),
              lty = 2) +
    ggtitle(paste0('PWF: ', vax_g_names[at])) +
    scale_x_continuous('p [probability]', breaks = seq(0, 1, by = .2)) +
    scale_y_continuous('w(p)', breaks = seq(0, 1, by = .2)) +
    theme_bw() +
    
    # add po-lvl posterior curves
    lapply(1:nic, function(i) {
      
      pwf = function(x, 
                     g = dg_post[i]) { exp(-(-log(x))^g) }
      
      line = geom_function(fun = pwf,
                           colour = vax_col[at],
                           alpha = .07)
    }) +
  
    # add median curve
    geom_function(fun = function(x,
                                 g = me_p[at, 'gam']) { exp(-(-log(x))^g) },

                  # colour = vax_col[at],
                  colour = rgb(1, 1, 1, .8),
                  lwd = 1) +
    
    # add frequencies
    geom_point(data = p_fr[p_fr$covid_vax_attitude == at, ],
               mapping = aes(x = v,
                             y = sv,
                             size = freq,
                             fill = og),
               shape = 21,
               colour = rgb(1, 1, 1, .1),
               alpha = .7,
               show.legend = F) +
    scale_size(range = c(1, 3)) +
    scale_fill_manual(values = c(rgb(1, .1, .1, .7),
                                 rgb(.5, .5, .5, .7),
                                 rgb(.1, .1, 1, .7))) +

    #
    geom_text(data = p_neg[p_neg$covid_vax_attitude == at, ],
              mapping = aes(x = .4,
                            y = .55,
                            label = ll),
              size = 3,
              col = rgb(.5, .5, .5, 1))
  
  # return
  return(p)
  
})

# with individual curves
ipwf_plts = lapply(c('anti', 'neu', 'pro'), function(at) {

  # get individual gamma means
  igams = ip[ip$covid_vax_attitude == at, 'igam']

  p = ggplot(data = data.frame(x = seq(0, 1, length.out = 10),
                               y = seq(0, 1, length.out = 10)),
             mapping = aes(x = x, y = y)) +
    geom_line(col = rgb(.5, .5, .5, .9),
              lty = 2) +
    ggtitle(' ') +
    scale_x_continuous('p', breaks = seq(0, 1, by = .2)) +
    scale_y_continuous('w(p)', breaks = seq(0, 1, by = .2)) +
    theme_bw() +

    # add individual curves
    lapply(1:length(igams), function(i) {

      pwf = function(x,
                     g = igams[i]) { exp(-(-log(x))^g) }

      line = geom_function(fun = pwf,
                           colour = vax_col[at],
                           alpha = .1)
    }) +

    # add median curve
    geom_function(fun = function(x,
                                 g = me_p[at, 'gam']) { exp(-(-log(x))^g) },

                  # colour = vax_col[at],
                  colour = rgb(1, 1, 1, .8),
                  lwd = 1) +

    # add frequencies
    geom_point(data = p_fr[p_fr$covid_vax_attitude == at, ],
               mapping = aes(x = v,
                             y = sv,
                             size = freq,
                             fill = og),
               shape = 21,
               colour = rgb(1, 1, 1, .1),
               alpha = .7,
               show.legend = F) +
    scale_size(range = c(1, 3)) +
    scale_fill_manual(values = c(rgb(1, .1, .1, .7),
                                 rgb(.5, .5, .5, .7),
                                 rgb(.1, .1, 1, .7))) +

    #
    geom_text(data = p_neg[p_neg$covid_vax_attitude == at, ],
              mapping = aes(x = .4,
                            y = .55,
                            label = ll),
              size = 3,
              col = rgb(.5, .5, .5, 1))

  # return
  return(p)

})

# # spudm talk  -------------------------------------------------------------
# 
# # save to file
# lapply(2, function(x) {
#   
#   ggsave(paste0('SPUDM/pwf_', x, '.jpg'),
#          plot = pwf_plts[[x]],
#          units = 'cm',
#          height = 9,
#          width = 10,
#          dpi = 700,
#          scale = .75)
#   
# })
# 
# # save to file
# lapply(2, function(x) {
#   
#   ggsave(paste0('SPUDM/vf_', x, '.jpg'),
#          plot = v_plts[[x]],
#          units = 'cm',
#          height = 9,
#          width = 10,
#          dpi = 700,
#          scale = .75)
#   
# })
# 
# param final figure ------------------------------------------------------

rm(list = setdiff(ls(), c('v_plts', 'iv_plts', 'pwf_plts', 'ipwf_plts', 'v_fun', 'pwf', 'me_p', 'plp_plts')))
save.image("03_results/Fig06_mb.RData")