rm(list = ls())

# libraries
library(loo)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(data.table)
library(viridis)

# pre-prepared figures
load("results/mm_figures/Fig03.RData")
load("results/mm_figures/Fig03pp.RData")

# elpd data ---------------------------------------------------------------

# modeling results
load("modeling/posteriors/fin.RData")

# useful functions
source('modeling/functions/01_elpd_plots.R')

lls <- extract_loos(fin_mods[c(4, 8)],
                    m_names = c('Affect Heuristic',
                                'Prospect Theory'),
                    g_names = c('Anti', 'Neu', 'Pro'))
elpd_dat <- elpds_plot(lls)
diff_loo <- elpd_diffs(lls,
                       rel = F,
                       comp_list = list(spt_ah = c(2, 1)))

vax_col = viridis(3, .9)

# model performance panel
elpd_plt <- ggplot(data = elpd_dat,
                   mapping = aes(x = covid_vax_attitude,
                                 y = elpd,
                                 ymin = li,
                                 ymax = ui,
                                 shape = model,
                                 group = model,
                                 col = covid_vax_attitude)) +
  geom_pointrange(position = position_dodge2(.75)) +
  scale_color_manual(values = vax_col) +
  xlab('Vaccination attitude') +
  ylab('(relative) elpd') +
  geom_hline(yintercept = 0,
             col = 'black',
             lty = 2) +
  ggtitle('Model performance',
          'Expected log pointwise density (elpd)') +
  scale_x_discrete(labels = c('Anti', 'Neutral', 'Pro')) +
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(family = "serif", size = 11))

# model comparison panel
diff_loo_plt <- ggplot(data = diff_loo,
                       mapping = aes(x = covid_vax_attitude,
                                     y = elpd_d,
                                     ymin = li,
                                     ymax = ui,
                                     col = covid_vax_attitude)) +
  geom_point() +
  geom_errorbar(width = .2) +
  scale_color_manual(values = vax_col) +
  scale_x_discrete(labels = c('Anti', 'Neutral', 'Pro')) +
  xlab('Vaccination attitude') +
  ylab('elpd difference') +
  geom_hline(yintercept = 0,
             col = 'black',
             lty = 2) +
  ggtitle('Prospect Theory - Affect Heuristic',
          'Model performance comparison') +
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(family = "serif", size = 11)) 

# # fig 3 -------------------------------------------------------------------
# 
# fig03a = ggarrange(elpd_plt, v_plt, 
#                   labels = c('b', 'd'),
#                   widths = c(1, 3),
#                   nrow = 1)
# 
# fig03b = ggarrange(diff_loo_plt, pwf_plt, 
#                    labels = c('c', 'e'),
#                    widths = c(1, 3),
#                    nrow = 1)
# 
# fig03 = ggarrange(ar_plt, fig03a, fig03b,
#                   labels = c('a', '', ''),
#                   nrow = 3)
# 
# ggsave('results/mm_figures/Fig03.jpg',
#        plot = fig03,
#        units = 'cm',
#        dpi = 300,
#        width = 15,
#        height = 11,
#        scale = 2)
# 
# # fig 3b ------------------------------------------------------------------
# 
# fig03b = ggarrange(diff_loo_plt, pwf_plt, 
#                    labels = c('b', 'c'),
#                    widths = c(1, 3),
#                    nrow = 1)
# 
# fig03 = ggarrange(ar_plt, fig03b,
#                   labels = c('a', ''),
#                   nrow = 2)
# 
# ggsave('results/mm_figures/Fig03b.jpg',
#        plot = fig03,
#        units = 'cm',
#        dpi = 300,
#        width = 15,
#        height = 7,
#        scale = 2)
# 
# fig 3c ------------------------------------------------------------------

fig03c = ar_plt + 
  diff_loo_plt + 
  plot_layout(widths = c(2.5, 1)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = 0, vjust = 1,
                                face = 'bold'))
fig03c

ggsave('results/mm_figures/Fig03c.jpg',
       plot = fig03c,
       units = 'cm',
       dpi = 300,
       width = 15,
       height = 5,
       scale = 2)
