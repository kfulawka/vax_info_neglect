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

# participant-level proportions of at least one full neglect
ineg = aggregate(ineg ~ covid_vax_attitude + sub,
                 data = pr_d,
                 FUN = function(x) sum(x == 'full'))

table(ineg$covid_vax_attitude, ineg$ineg == 0)
prop.table(table(ineg$covid_vax_attitude, ineg$ineg == 0), 1) * 100

# information neglect and vax attitudes -----------------------------------

load("01_statistical_modeling/02_information_neglect.RData")

# model to analyze
mm = m02_in$ineg$full

# data frame for conditional effects
ce_dat = mm$data[1,]; ce_dat$ineg = NULL; ce_dat$sub = NULL;ce_dat[1,] = NA

# get the predicted posterior probs with  95% CI
pp = conditional_effects(mm,
                         conditions = ce_dat,
                         effects = 'covid_vax_attitude',
                         prob = .95,
                         categorical = T)[[1]]

# get the observed proportions
ff = data.frame( prop.table(table(mm$data$ineg, mm$data$covid_vax_attitude), 2)) 
colnames(ff)[1:2] = c('effect2__', 'covid_vax_attitude')

pp = merge(pp, ff, by = c('effect2__', 'covid_vax_attitude'))

# into percentages
pp[c('li', 'ui', 'me', 'Freq')] =  round( pp[c('lower__', 'upper__', 'estimate__', 'Freq')] * 100 )

# post-hocs
nd = ce_dat; nd[1:3,'covid_vax_attitude'] = levels(ce_dat$covid_vax_attitude)
cov_at_postHoc(mm, nd = nd)[[1]]; rm(nd)

# # figure
# plt_ineg_type_pp = ggplot(pp, 
#              mapping = aes(x = covid_vax_attitude,
#                            col = covid_vax_attitude,
#                            group = effect2__)) +
#   geom_linerange(mapping = aes(ymin = li, 
#                                ymax = ui),
#                  lwd = .5,
#                  position = position_dodge(.5),
#                  alpha = 1) +
#   geom_point(mapping = aes(y = me,
#                            shape = effect2__),
#              size = 2,
#              position = position_dodge(.5)) +
#   geom_point(mapping = aes(y = Freq),
#              size = 2,
#              shape = 4,
#              position = position_dodge(.5),
#              alpha = .5) +
#   scale_color_manual(name = 'Vaccination attitude', 
#                      values = viridis(3, 1)) +
#   scale_y_continuous('Proportion of decisions', 
#                      breaks = seq(0, 1, by = .20),
#                      limits = c(0, 1)) +
#   scale_shape_manual(name = 'Information neglect',
#                      values = 0:2) +
#   xlab('Vaccination attitude') + 
#   guides(color = 'none') +
#   theme_bw() +
#   theme(legend.position = c(.5, .9),
#         legend.background = element_rect(fill = NA),
#         legend.key = element_rect(fill = NA),
#         # axis.title.y = element_text(size = 9),
#         legend.direction = 'horizontal')

# or piechart...
pp$lab = paste0( pp$Freq, '% [', pp$me, '%]'  )

plt_ineg_type_pp = ggplot(pp,
                          mapping = aes(x = "", y = Freq, fill = cats__)) +
  geom_col(color = rgb(0, 0, 0, .2)) +
  geom_text(aes(label = lab,
                x = 1.2,
                # color = cats__
                ),
            position = position_stack(vjust = .5),
            alpha = 1,
            show.legend = F,
            size = 3) +
  coord_polar(theta = "y") +
  scale_fill_manual(name = 'Information neglect',
                    # values = turbo(3, .5),
                    values = c(rgb(0, 0, 0, .5), rgb(.5, .5, .5, .5), rgb(1, 1, 1, .5)),
                    labels = c('Complete', 'Selective', 'None')) +
  scale_color_manual(name = 'Information neglect',
                    values = turbo(3, 1),
                    labels = c('Complete', 'Selective', 'None')) +
  ylab('Proportion of decisions') +
  facet_wrap(~effect1__) +
  theme_void() +
  theme(legend.position = c(.5, 0),
        legend.key.size = unit(6, 'pt'),
        legend.title = element_text(size = 10),
        legend.direction = 'horizontal')

# overall search and vax-accept -------------------------------------------

# total effect
load("01_statistical_modeling/03_choice_info_neglect.RData")
sef_mods = list() # full mods for pps; main for post hocs
sef_mods[['tot']] = m03_co_in$full_v2

# add vax-att-specific effects
load("01_statistical_modeling/03_choice_info_neglect_sep.RData")

sef_mods[['anti']] = m03_co_in_sep$anti$full
sef_mods[['neu']] = m03_co_in_sep$neu$full
sef_mods[['pro']] = m03_co_in_sep$pro$full

# choice and total search
ce_dat = sef_mods[['tot']]$data[1,]; ce_dat$sub = NULL; ce_dat[1,] = NA

# get the predicted posterior probs with 50%, 80% and 95% CI
pd_ce = lapply(names(sef_mods), function(m) {
  
  # adjust conditions if not tot
  if(m != 'tot') { ce_dat$covid_vax_attitude = NULL }
  
  pd_ce = conditional_effects(sef_mods[[m]],
                              conditions = ce_dat,                            
                              effects = 'ineg',
                              prob = .95)[[1]]
  
  # add frequencies
  dat = sef_mods[[m]]$data
  props = data.frame( prop.table(table(dat$ineg, dat$choice), 1) )
  colnames(props)[1] = 'effect1__'
  props = props[4:6, ]
  
  pd_ce = merge(pd_ce, props, by = 'effect1__')
  
  # post-hocs
  nd = ce_dat; nd[1:3,'ineg'] = levels(ce_dat$ineg)
  ph = cov_at_postHoc(sef_mods[[m]], 
                      nd = nd,
                      comp = list(sel_full = c(2, 1),
                                  no_sel = c(3, 2)))[[1]]; rm(nd)
  ph$effect1__ = c('selective', 'full')
  
  pd_ce = merge(pd_ce, ph[,c('effect1__', 'li_d', 'ui_d')],
                by = c('effect1__'),
                all = T)
  
  pd_ce$cred = ifelse( sign(pd_ce$li_d * pd_ce$ui_d) == 1,
                       pd_ce$estimate__, NA)
  
  # add data source
  pd_ce$group = m
  
  return(pd_ce)
  
}); names(pd_ce) = c('tot', 'Anti', 'Neutral', 'Pro')

# colors
ccs = c(rgb(.1, .1, .1, .8), viridis(3, 1))
names(ccs) = names(pd_ce)

# figure
co_se_ce_plts = lapply(names(pd_ce), function(m) {
  
  p = ggplot(pd_ce[[m]],
             mapping = aes(x = effect1__)) +
    geom_linerange(mapping = aes(ymin = lower__, 
                                 ymax = upper__),
                   linewidth = .5,
                   position = position_dodge(.5),
                   show.legend = F,
                   col = ccs[m]) +
    geom_point(mapping = aes(y = estimate__,
                             shape = 'Model'),
               position = position_dodge(.5),
               size = 2,
               col = ccs[m]) + 
    geom_point(mapping = aes(y = Freq,
                             shape = 'Data'),
               position = position_dodge(.5),
               alpha = .2,
               size = 2,
               col = ccs[m]) +
    geom_point(mapping = aes(y = cred,
                             shape = 'Credible difference'),
               position = position_dodgenudge(width = .5, x = .5),
               col = 'blue') +
    scale_y_continuous('P(accept)', 
                       breaks = seq(0, 1, by = .20),
                       limits = c(0, 1)) +
    scale_x_discrete('Information neglect',
                     labels = c('Complete', 'Selective', 'None')) +
    ggtitle(m) +
    labs(shape = '') +
    theme_bw() +
    scale_shape_manual(values = c(6, 4, 3)) +
    theme(legend.position = c(.45, .8),
          legend.background = element_rect(fill = NA),
          legend.key = element_rect(fill = NA),
          plot.title = element_text(size = 12))
  
  if(m != 'tot') {
    
    p = p + guides(shape = 'none')
    
  } else {
    
    p = p + theme(plot.title = element_blank())
    
  }
  
  if(m %in% c('Neutral', 'Pro')) { p = p + ylab(' ')}
  
  return(p)
  
})

# final figure ------------------------------------------------------------

design = "
  112
  345
"

fig03 = plt_ineg_type_pp + co_se_ce_plts[[1]] + co_se_ce_plts[[2]] +
  co_se_ce_plts[[3]] + co_se_ce_plts[[4]] +
  plot_layout(design = design) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(face = 'bold')) 

# save to file
ggsave('03_results/figures/Fig03.pdf',
       plot = fig03,
       units = 'cm',
       device = 'pdf',
       height = 8,
       width = 16,
       scale = 1.4)

# # spudm figs --------------------------------------------------------------
# 
# # save to file
# ggsave('03_results/spudm/Fig_in.jpg',
#        plot = co_se_ce_plts[[1]],
#        units = 'cm',
#        height = 4,
#        width = 5,
#        dpi = 700,
#        scale = 1.4)