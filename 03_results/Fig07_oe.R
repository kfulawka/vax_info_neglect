rm(list = ls())

library(ggplot2)
library(patchwork)
library(data.table)

# oe data
load("data/oe_dat.RData")

#
cc = c('prob', 'se', 'be', 'country', 'consp', 'emo', 'all')
oe[,cc][oe[,cc] == 99] = NA

# outcomes ----------------------------------------------------------------

oe$out = NA
oe$out[oe$se & !oe$be] = 'SE only' 
oe$out[!oe$se & oe$be] = 'PA only' 
oe$out[oe$se & oe$be] = 'SE & PA' 
oe$out[!oe$se & !oe$be] = 'Neither' 
oe$out[is.na(oe$out)] = 'No resp.'

oe$out = factor(oe$out,
                levels = c('No resp.', 'Neither', 'SE & PA', 
                           'PA only','SE only'),
                labels = c('No response', 'Neither', 'SE & B', 
                           'Benefits (B) only','Side effects (SE) only'),
                ordered = T)

# into props and frequencies
out_tab = table(oe$covid_vax_attitude, oe$out)
out_prop = prop.table(out_tab, 1)

outcomes = data.frame(out_tab)
outcomes$prop = data.frame(out_prop)[,'Freq']
outcomes$prop = outcomes$prop * 100

# figure
out_plt = ggplot(data = outcomes,
                 mapping = aes(y = Var1,
                               x = prop,
                               fill = Var2)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(name = '',
                    values = viridis::inferno(5, .7)) +
  scale_y_discrete(name = 'Vax attitude',
                   labels = c('Anti', 'Neutral', 'Pro')) +
  scale_x_continuous(name = 'Percentage of outcome mentions',
                     breaks = seq(0, 100, 10)) +
  theme_bw() +
  theme(legend.position = 'right',
        axis.text.y = element_text(angle = 90,
                                   hjust = .5))

# # probabilities ------------------------------------------------------------
# 
# oe$probs = NA
# oe$probs[oe$prob & !oe$se & !oe$be] = 'Prob. only' 
# oe$probs[oe$prob & oe$se & !oe$be] = 'Prob. & SE only' 
# oe$probs[oe$prob & !oe$se & oe$be] = 'Prob. & PA only' 
# oe$probs[oe$prob & oe$se & oe$be] = 'Prob. & SE & PA' 
# oe$probs[!oe$prob & (oe$se | oe$be)] = 'No Prob. but SE or PA' 
# oe$probs[!oe$prob & !oe$se & !oe$be] = 'Neither' 
# oe$probs[is.na(oe$probs)] = 'No resp.'
# 
# oe$probs = factor(oe$probs,
#                   levels = c('No resp.', 'Neither', 'No prob. but SE or PA',
#                              'Prob. & SE & PA', 
#                              'Prob. & PA only' , 'Prob. & SE only', 
#                              'Prob. only'),
#                   ordered = T)
# 
# # into props and frequencies
# probs_tab = table(oe$covid_vax_attitude, oe$probs)
# probs_prop = prop.table(probs_tab, 1)
# 
# probs = data.frame(probs_tab)
# probs$prop = data.frame(probs_prop)[,'Freq']
# 
# # figure
# prob_plt = ggplot(data = probs,
#                   mapping = aes(y = Var1,
#                                 x = prop,
#                                 fill = Var2)) +
#   geom_bar(stat = 'identity') +
#   scale_fill_manual(name = '',
#                     values = viridis::turbo(nlevels(oe$probs), .7)) +
#   scale_y_discrete(name = '',
#                    labels = c('Anti', 'Neutral', 'Pro')) +
#   scale_x_continuous(name = 'Proportion',
#                      breaks = seq(0, 1, .1)) +
#   theme_bw() +
#   theme(legend.position = 'bottom',
#         axis.text.y = element_text(angle = 90))
# 
# # prob_plt
# 
# rm(list = setdiff(ls(), c('oe', 'out_plt', 'prob_plt')))
# 
# 
# probabilities only ------------------------------------------------------

# into props and frequencies
probs_tab = table(oe$covid_vax_attitude, oe$prob)
probs_prop = prop.table(probs_tab, 1)

probs = data.frame(probs_tab)
probs$prop = data.frame(probs_prop)[,'Freq']
probs = probs[probs$Var2 == 1, ]
probs$prop = probs$prop * 100

# figure
prob_plt = ggplot(data = probs,
                  mapping = aes(y = Var1,
                                x = prop)) +
  geom_bar(stat = 'identity',
           color = rgb(.1, .3, .6, .7),
           fill = rgb(.1, .3, .6, .7)) +
  # scale_fill_manual(name = '',
  #                   values = viridis(3, .5)) +
  scale_y_discrete(name = '',
                   labels = c('Anti', 'Neutral', 'Pro')) +
  scale_x_continuous(name = 'Percentage of probability mentions',
                     breaks = seq(0, 20, by = 5)) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.y = element_text(angle = 90,
                                   hjust = .5))

rm(list = setdiff(ls(), c('oe', 'out_plt', 'prob_plt')))

# # modeling ----------------------------------------------------------------
# 
# load("results/Fig07b_.RData")
# 
# # final figure ------------------------------------------------------------
# 
# f7a = (out_plt | prob_plt) + plot_layout(widths = c(10, 6))
# 
# fig07 = ( f7a / fig07b) +
#   plot_layout(heights = c(3, 10)) +
#   plot_annotation(tag_levels = 'a') &
#   theme(plot.tag.position = c(0, 1),
#         plot.tag = element_text(face = 'bold'))
# 
# # save to file
# ggsave('results/figures/Fig07_oe.jpg',
#        plot = fig07,
#        units = 'cm',
#        height = 13,
#        width = 16,
#        dpi = 700,
#        scale = 1.4)

# mm talk figure ----------------------------------------------------------

load("results/Fig07_mc.RData")

f7a = (out_plt | prob_plt) + plot_layout(widths = c(10, 6))

fig07 = ( f7a / fig07_mc ) +
  plot_layout(heights = c(3, 10)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(face = 'bold'))

# save to file
ggsave('results/figures/Fig07_mc2.jpg',
       plot = fig07,
       units = 'cm',
       height = 13,
       width = 16,
       dpi = 700,
       scale = 1.4)