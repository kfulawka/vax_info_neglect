rm(list = ls())

library(ggplot2)
library(ggpp)
library(patchwork)
library(viridis)
library(brms)
library(bayesplot)
library(data.table)

# data --------------------------------------------------------------------

# data prepared for the analyses
load('01_statistical_modeling/data_analyses.RData')
rm(list = setdiff(ls(), c('d', 'pr_d', 'preds')))

# function for post hocs computation
source('03_results/functions/00_cov_at_postHoc.R')

# participant-level proportions of at least one pn neglect
pneg = aggregate(cbind(se_pn, b_pn) ~ covid_vax_attitude + sub,
                 data = pr_d,
                 FUN = function(x) sum(x == 1))

for(i in c('se_pn', 'b_pn')) {
  
  print(i)
  print( prop.table(table(pr_d$covid_vax_attitude, pr_d[,i]), 1) )
  print( table(pneg$covid_vax_attitude, pneg[,i] > 0) )
  print( prop.table(table(pneg$covid_vax_attitude, pneg[,i] > 0), 1) * 100 )
  
}

# prob neg by attitude ----------------------------------------------------

load("01_statistical_modeling/02_information_neglect.RData")

# data frame for conditional effects
ce_dat = m02_in$se_pn$main$data[1,]; ce_dat$sub = NULL; ce_dat$se_pn = NULL; ce_dat[1,] = NA

# data for plotting
pn_va = lapply(names(m02_in)[-1], function(pn) {
  
  # model
  m = m02_in[[pn]]$main
  
  # get the predicted posterior probs with  95% CI
  pp = conditional_effects(m,
                           conditions = ce_dat,
                           effects = 'covid_vax_attitude',
                           prob = .95)[[1]]
  
  # 
  pp[,pn] = NULL
  
  # get the observed proportions
  ff = data.frame( prop.table(table(m$data[,pn], m$data$covid_vax_attitude), 2)) 
  colnames(ff)[1:2] = c(pn, 'covid_vax_attitude')
  ff = ff[ff[,pn] == 1, -1]
  
  pp = merge(pp, ff, by = c('covid_vax_attitude'))

  # into percentages
  pp[c('li', 'ui', 'me', 'Freq')] =  pp[c('lower__', 'upper__', 'estimate__', 'Freq')] #* 100
  
  # post-hocs
  nd = ce_dat; nd[1:3,'covid_vax_attitude'] = levels(ce_dat$covid_vax_attitude)
  ph = cov_at_postHoc(m, 
                      nd = nd,
                      comp = list(neu_anti = c(2, 1),
                                  pro_neu = c(3, 2)))[[1]]; rm(nd)
  ph$effect1__ = c('Neutral', 'Pro')
  
  pp = merge(pp, ph[,c('effect1__', 'li_d', 'ui_d')],
             by = c('effect1__'),
             all = T)
  
  pp$cred = ifelse( sign(pp$li_d * pp$ui_d) == 1,
                    pp$estimate__, NA)
  
  # neglect type
  pp$pn = pn
  
  return(pp)
  
}); pn_va = data.frame( rbindlist(pn_va) )

# figure
pn_ce_plt = ggplot(pn_va, 
             mapping = aes(x = pn,
                           col = covid_vax_attitude,
                           group = covid_vax_attitude)) +
  geom_linerange(mapping = aes(ymin = li, 
                               ymax = ui),
                 lwd = .5,
                 position = position_dodge(.5),
                 alpha = 1) +
  geom_point(mapping = aes(y = me,
                           shape = 'Model'),
             size = 2,
             position = position_dodge(.5)) +
  geom_point(mapping = aes(y = Freq,
                           shape = 'Data'),
             size = 2,
             position = position_dodge(.5),
             alpha = .5) +
  geom_point(mapping = aes(y = cred,
                           shape = 'Credible\ndifference'),
             size = 2,
             color = 'blue',
             position = position_dodgenudge(width = .5,
                                            x = -.1),
             alpha = 1) +
  scale_color_manual(name = 'Attitude', 
                     values = viridis(3, 1)) +
  scale_y_continuous('Proportion of decisions', 
                     breaks = seq(0, .20, by = .02),
                     limits = c(0, .16)) +
  scale_x_discrete('Attentional probability neglect',
                   labels = c('Benefits', 'Side effects (SE)')) +
  scale_shape_manual(name = NULL,
                     values = c(6, 4, 3)) +
  theme_bw() +
  theme(legend.position = 'right',
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.title = element_text(size = 9)
        )

# by outcome type ---------------------------------------------------------

# total effect
load("01_statistical_modeling/03_choice_info_neglect.RData")
round( exp(fixef(m03_co_in$full_v1)[c('se_pn1', 'b_pn1'),-2]), 2)

# univariate effects
load("01_statistical_modeling/03_choice_info_neglect_uni.RData")

round( exp( fixef(m03_co_in_uni$se_pn$vax_att)['se_pn1',-2]), 2)
round( exp( fixef(m03_co_in_uni$b_pn$vax_att)['b_pn1',-2]), 2)

sef_mods = list() # full mods for pps; main for post hocs
sef_mods[['tot']] = m03_co_in$full_v2

# add vax-att-specific effects
load("01_statistical_modeling/03_choice_info_neglect_sep.RData")

sef_mods[['anti']] = m03_co_in_sep$anti$full
sef_mods[['neu']] = m03_co_in_sep$neu$full
sef_mods[['pro']] = m03_co_in_sep$pro$full

# choice and total search
ce_dat = sef_mods[['tot']]$data[1,]; ce_dat$sub = NULL; ce_dat[1,] = NA; ce_dat$choice = NULL

# choice and probability neglect (including sep figures for the supplement)
co_pnot_plts = lapply(names(sef_mods), function(m) {
  
  # adjust conditions if not tot
  if(m != 'tot') { ce_dat$covid_vax_attitude = NULL }
  
  # set model
  mm = sef_mods[[m]]
  
  # for each predictor type
  co_pnot_d = lapply(c('se_ext_pn', 'se_sev_pn', 
                       'se_mild_pn', 'b_pn'), function(pn) {
                         
    ce = ce_dat; ce[,pn] = NULL
                         
    # get the predicted posterior probs with 50%,95% CI
    pp = conditional_effects(mm,
                             conditions = ce,
                             effects = pn,
                             prob = .95)[[1]]
    
    pp$pn = pn
    pp = pp[,c('pn', 'effect1__', 'estimate__', 'lower__', 'upper__')]
    
    # get raw proportions
    pd = data.frame(prop.table(table( mm$data[,pn], mm$data$choice ), 1))
    colnames(pd)[1:2] = c('effect1__', 'choice')
    pd = pd[pd$choice == 1, ]
    pd$choice = NULL
    
    pp = merge(pp, pd, by = 'effect1__')
    
    return(pp)
    
  }); 
  co_pnot_d = data.frame( rbindlist(co_pnot_d) )
  
  co_pnot_d$pn = factor(co_pnot_d$pn,
                        levels = c('se_ext_pn', 'se_sev_pn', 'se_mild_pn', 'b_pn'),
                        labels = c('Extreme SE','Severe SE', 'Mild SE',
                                   'Benefits'),
                        ordered = T)
  co_pnot_d$og = ifelse(co_pnot_d$pn == 'Benefits', 'b', 'a')
  
  # # ONLY FOR SPUDM
  # co_pnot_d$cred = NA
  
  co_pnot_d$cred[(co_pnot_d$pn %in% c('Extreme SE', 'Mild SE')) & (co_pnot_d$effect1__ == 0)] = .25
  
  # figures
  plt = ggplot(co_pnot_d, 
               mapping = aes(x = pn,
                             col = effect1__,
                             group = effect1__)) +
  geom_linerange(mapping = aes(ymin = lower__, 
                               ymax = upper__),
                 linewidth = .5,
                 position = position_dodge(.75),
                 # alpha = .5,
                 show.legend = F) +
    geom_point(mapping = aes(y = estimate__),
               position = position_dodge(.75),
               shape = 3,
               size = 2) +
    geom_point(mapping = aes(y = Freq),
               position = position_dodge(.75),
               alpha = .2,
               shape = 4,
               size = 2,
               show.legend = F) +
    
    # # ONLY FOR SPUDM!!!
    # geom_point(mapping = aes(y = cred),
    #            size = 1,
    #            color = 'black',
    #            shape = 6,
    #            # position = position_nudge(x = .25),
    #            alpha = 1) +
    
    scale_color_manual(name = 'APN in info acquisition', 
                       values = c(rgb(.1, .3, .6, .9),
                                  rgb(.9, .4, .1, .9)),
                       labels = c('No', 'Yes')) +
    xlab('Attentional probability neglect (APN)') +
    scale_y_continuous(name = 'P(accept)',
                       breaks = seq(0, 1, .2),
                       limits = 0:1) +
    facet_grid(~og, 
               scales = 'free_x',
               space = 'free_x') +
    theme_bw() +
    theme(legend.position = c(.35, .77),
          legend.key = element_rect(fill = NA),
          strip.background = element_blank(),
          strip.text = element_blank(),
          legend.background = element_rect(fill = rgb(1, 1, 1, 0))
          )
  
  # 
  if(m != 'tot') { plt = plt + ggtitle(m) }
  
  #
  return(plt)
  
})

# info neglect effects in context -----------------------------------------

source('01_statistical_modeling/functions/comp_par_mods.R')

ff = bor_mod(sef_mods$tot, T)
ff$par = factor(ff$par,
                levels = ff$par,
                ordered = T)

ff$par_g = factor(NA,
                  levels = c('APNs and information neglect (IN)', 
                             'Vaccine-related', 
                             'Demographics'),
                  ordered = T)
ff$par_g[ff$par %in% grep('vax', ff$par, value = T)] = 'Vaccine-related'
ff$par_g[ff$par %in% grep('pn|ineg', ff$par, value = T)] = 'APNs and information neglect (IN)'
ff$par_g[is.na(ff$par_g)] = 'Demographics'

# adjust par names
ff$par = gsub('covid_vax_attitude', 'Att.', ff$par)

ff = ff[order(ff$m), ]
ff$par = factor(ff$par,
                levels = ff$par,
                ordered = T)

ff$labs = NA
ff$labs[ff$par == 'se_ext_pn1'] = "Extreme SE"
ff$labs[ff$par == 'se_sev_pn1'] = "Severe SE"
ff$labs[ff$par == 'se_mild_pn1'] = "Mild SE"
ff$labs[ff$par == 'b_pn1'] = "Benefit"
ff$labs[ff$par == 'ineg1'] = "Complete IN"
ff$labs[ff$par == 'ineg2'] = "Selective IN"

ff$labs[is.na(ff$labs)] = ''

# THE FIGURE
coefs_pnot_plt = ggplot(data = ff,
                        mapping = aes(x = par,
                                      y = m,
                                      ymin = li, 
                                      ymax = ui,
                                      col = par_g)) +
  geom_hline(yintercept = exp(c(-.2, 0, .2)),
             col = c(rgb(.5, .5, .8, .5),
                     rgb(.1, .1, .1, .5),
                     rgb(.5, .5, .8, .5)),
             lty = c(2, 1, 2)) +
  geom_pointrange(alpha = .6,
                  size = .2) +
  scale_x_discrete(name = 'Regression coefficient',
                   labels = ff$labs) +
  scale_y_log10('Odds ratio',
                breaks = round( exp(log(c(1/10, 1/4, 1/2, 1, 2, 4, 10))), 2),
                limits = exp(c(-2.302585, 2.302585))
                ) +
  scale_color_manual(name = '',
                     values = c('orange', 'black', 'grey')) +
  theme_bw() +
  theme(axis.text.x = element_text(color = 'orange', 
                                   angle = 90, 
                                   vjust = .5,
                                   hjust = 1),
        legend.position = c(.2, .8),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = rgb(1, 1, 1, 0)))


# info neglect effects by attitude group ----------------------------------

# get the coefficients for each group
apn_at_ff = lapply(names(sef_mods)[-1], function(a) {
  
  ff = bor_mod(sef_mods[[a]], T)
  
  ff$labs = NA
  ff$labs[ff$par == 'se_ext_pn1'] = "Extreme SE"
  ff$labs[ff$par == 'se_sev_pn1'] = "Severe SE"
  ff$labs[ff$par == 'se_mild_pn1'] = "Mild SE"
  ff$labs[ff$par == 'b_pn1'] = "Benefit"
  ff$labs[ff$par == 'ineg1'] = "Complete IN"
  ff$labs[ff$par == 'ineg2'] = "Selective IN"
  
  ff = ff[!is.na(ff$labs), ]
  
  ff$par = factor(ff$par,
                  levels = c('se_mild_pn1', 'ineg1', 'se_sev_pn1', 'ineg2', 'b_pn1', 'se_ext_pn1'),
                  ordered = T)
  
  ff$covid_vax_attitude = a
  
  return(ff)
  
}); apn_at_ff = data.frame( rbindlist(apn_at_ff) )

# figure
coefs_pnot_sep_plt = ggplot(data = apn_at_ff,
                            mapping = aes(x = par,
                                          y = m,
                                          ymin = li, 
                                          ymax = ui,
                                          col = covid_vax_attitude)) +
  geom_hline(yintercept = exp(c(-.2, 0, .2)),
             col = c(rgb(.5, .5, .8, .5),
                     rgb(.1, .1, .1, .5),
                     rgb(.5, .5, .8, .5)),
             lty = c(2, 1, 2)) +
  geom_pointrange(alpha = .6,
                  size = .2,
                  show.legend = F,
                  position = position_dodge(.5)) +
  scale_x_discrete(name = '...by vaccination attitude',
                   labels = c('Mild SE', 'Complete IN', 'Severe SE',
                              'Selective IN', 'Benefit', 'Extreme SE')) +
  scale_y_log10('',
                breaks = round( exp(log(c(1/10, 1/4, 1/2, 1, 2, 4, 10))), 2),
                limits = exp(c(-2.302585, 2.302585))) +
  scale_color_manual(name = '',
                     values = viridis(3, .7)) +
  theme_bw() +
  theme(axis.text.x = element_text(color = 'orange', 
                                   angle = 90, 
                                   vjust = .5,
                                   hjust = 1),
        legend.position = c(.13, .8),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA),
        axis.text.y = element_blank(),
        legend.background = element_rect(fill = rgb(1, 1, 1, 0)))

# final figure ------------------------------------------------------------

fig04t = (pn_ce_plt | co_pnot_plts[[1]] ) +
  plot_layout(widths = c(1.2, 2)) 
fig04b = ( coefs_pnot_plt | coefs_pnot_sep_plt ) +
  plot_layout(widths = c(2.5, 1)) & 
  theme(plot.margin = unit(c(16.5, 5.5, 5.5, 5.5), 'pt'))

fig04 = (fig04t / fig04b) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(face = 'bold')) 

# save to file
source('03_results/functions/99_fig_to_pdf.R')
pdf_save(path = '03_results/figures/Fig04.pdf',
         fig = fig04,
         height = 10,
         width = 16,
         scale = 1.4)
# # spudm figs --------------------------------------------------------------
# 
# # save to file
# ggsave('03_results/spudm/Fig_apn2.jpg',
#        plot = co_pnot_plts[[1]],
#        units = 'cm',
#        height = 4,
#        width = 7,
#        dpi = 700,
#        scale = 1.4)
# 
# # save to file
# ggsave('03_results/spudm/Fig_in_apn_context.jpg',
#        plot = coefs_pnot_plt,
#        units = 'cm',
#        height = 6,
#        width = 12,
#        dpi = 700,
#        scale = 1.4)
