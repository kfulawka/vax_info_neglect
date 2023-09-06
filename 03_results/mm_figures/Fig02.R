rm(list = ls())

library(ggplot2)
library(patchwork)
library(ggpubr)

load("results/mm_figures/Fig02.RData")

# minor edits -------------------------------------------------------------

# minor edits
pn_ce_plt = pn_ce_plt + 
  ggtitle('Predicted posterior proportion of attentional probability neglect (APN)') +
  scale_x_discrete(labels = c("Benefits' APN", "Side effects' APN")) +
    theme(axis.title.x = element_blank())

co_pn_ce_plt = co_pn_ce_plt + 
  ggtitle('Predicted posterior probability of vaccine accpetance') +
  scale_x_discrete(labels = c("Benefits", "Side effects")) +
  theme(axis.title.x = element_blank())

ce_pnVax_d = lapply(ce_pnVax_beta_plt, function(p) {
  
  d = p$data[, c('vax', 'll', 'hh', 'g', 'vax_g') ]
  d$cred = ifelse( sign(d$ll * d$hh) == 1, .3, NA)
  d$covid_vax_attitude = d$g
  
  return(d[,c('vax', 'covid_vax_attitude', 'vax_g', 'cred')])
  
})

co_pnVax_ce_plt = lapply(names(co_pnVax_ce_plt), function(pp) {
  
  p = co_pnVax_ce_plt[[pp]]
  
  p$data = merge(p$data, 
                 ce_pnVax_d[[pp]], 
                 by = c('vax', 'covid_vax_attitude', 'vax_g'))
  
  p = p + ggtitle('Predicted posterior probability of vaccine accpetance') +
    scale_y_continuous(breaks = seq(0, 1, .2),
                       labels = seq(0, 1, .2)) +
    theme(axis.title.x = element_blank()) +
    guides(shape = 'none') +
    geom_point(mapping = aes(y = cred),
               shape = 13,
               col = 'red')
  
  return(p)
  
}); names(co_pnVax_ce_plt) = names(ce_pnVax_d)

# ce_pnVax_beta_plt = lapply(ce_pnVax_beta_plt, function(p) {
#   
#   p = p + theme(axis.title.x = element_blank()) +
#     ylab('Estimate (logit)')
#   
#   return(p)
#   
# })

# prob neglect ------------------------------------------------------------

m_pn = ggarrange(pn_ce_plt, co_pn_ce_plt,
                 labels = c('a', 'b'),
                 nrow = 1,
                 font.label = list(family = 'serif',
                                   face = 'bold'))


# pnVax_se = ggarrange(co_pnVax_ce_plt$pse_neg1,
#                      ce_pnVax_beta_plt$pse_neg1,
#                      nrow = 2,
#                      labels = c('c', 'd'),
#                      heights = c(1.7, 1),
#                      legend = F)

pNeg_se = ggarrange(m_pn, co_pnVax_ce_plt$pse_neg1,
                    nrow = 2,
                    labels = c('', 'c'),
                    heights = c(1, 1.6),
                    font.label = list(family = 'serif',
                                      face = 'bold'))

# pNeg_se = (pn_ce_plt | co_pn_ce_plt) / co_pnVax_ce_plt$pse_neg1 +
#   plot_annotation(tag_levels = 'a') +
#   plot_layout(heights = c(1, 1.6))

ggsave('results/mm_figures/Fig02.jpg',
       plot = pNeg_se,
       units = 'cm',
       dpi = 300,
       width = 15,
       height = 8,
       scale = 2)

pNeg_be = ggarrange(m_pn, co_pnVax_ce_plt$pbe_neg1,
                    nrow = 2,
                    labels = c('', 'c'),
                    heights = c(1, 1.6))

ggsave('results/mm_figures/Fig02b.jpg',
       plot = pNeg_be,
       units = 'cm',
       dpi = 300,
       width = 15,
       height = 7,
       scale = 2)
