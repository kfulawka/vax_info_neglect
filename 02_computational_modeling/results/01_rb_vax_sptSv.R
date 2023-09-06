rm(list = ls())

# libraries
library(ggplot2)
library(patchwork)
library(data.table)
library(viridis)
library(ggdist)

# modeling results
load("02_computational_modeling/posteriors/01_ispt_vc_irb_ct.RData")

#
att_g = c('anti', 'neu', 'pro')
att_n = c('Anti', 'Neutral', 'Pro')
names(att_n) = att_g
names(att_g) = att_g


# ispt values -------------------------------------------------------------

source('02_computational_modeling/00_functions/spt.R')

pt_sv = lapply(att_g, function(at) {
  
  x = ispt[[at]]
  
  # get individual posterior-mean parameters
  ip = sapply(x$pars$i_pars, colMeans)
  
  # get alpha and phi 
  ap = sapply(x$pars$p_pars[c('alpha', 'phi')], mean)
  
  # data
  d = x$sampling_info$data
  
  # container for subjective values
  sv = c()
  
  # for each choice 
  for(i in 1:d$n) {
    
    sv[i] = spt(x = c(d$se[i, ], d$be[i,]),
                p = c(d$pse[i, ], d$pbe[i,]),
                a = ap['alpha'],
                l = ip[ d$sub[i] ,'ibeta'],
                g = ip[ d$sub[i] ,'igam'],
                phi = ap['phi'])
    
  }
  
  # # transform to p(accept)
  # sv = 1/( 1 + exp(-sv) )
  
  # combine the svs with choice
  r = data.frame(sv = sv,
                 choice = factor(ifelse(d$co, 'Accept', 'Refuse'),
                                 levels = c('Refuse', 'Accept'),
                                 ordered = T),
                 at = at,
                 sub = rep(1:d$N, each = 8))
  
  # output
  return(r)
  
}); pt_sv = data.frame( rbindlist(pt_sv))

# 

# rb parameters -----------------------------------------------------------

rb_pars = lapply(att_g, function(at) {
  
  x = ispt[[at]]
  
  # get individual posterior-mean parameters
  irb = data.frame(irb = colMeans(x$pars$i_pars$irb),
                   par = 'irb',
                   at = at)
  
  # transform to p(accept)
  # irb$irb = 1 / ( 1 + exp(-irb$irb) )
  
  # get rb and vax pars 
  rb_vax = data.frame(cbind(x$pars$p_pars$vax_b, x$pars$p_pars$rb) )
  rb_vax$ii = 1:nrow(rb_vax)
  
  colnames(rb_vax)[1:5] = c('coo1', 'coo2', 'vt1', 'vt2', 'rb')
  
  # add the third coef
  rb_vax$coo3 = -rb_vax$coo1 - rb_vax$coo2
  rb_vax$vt3 = -rb_vax$vt1 - rb_vax$vt2

  # data in long format
  rb_vax = melt( data.table(rb_vax),
                 id.vars = 'ii')
  rb_vax = data.frame(rb_vax)
  rb_vax$at = at
  
  # output
  return(list(irb = irb, rb_vax = rb_vax))
  
}); 

irbs = data.frame(rbindlist( lapply(rb_pars, function(x) x$irb) ))
rb_vax = data.frame(rbindlist( lapply(rb_pars, function(x) x$rb_vax) ))

# rb_vax$value = 1 / ( 1 + exp(-rb_vax$value) )

# figures -----------------------------------------------------------------

# subjective vax values
sv_plt = ggplot(pt_sv, 
                mapping = aes(x = at,
                              y = sv)) +
  # stat_slab(aes(color = choice),
  #           fill = NA) +
  stat_histinterval(aes(fill = choice),
                    breaks = 'scott',
                    outline_bars = F,
                    slab_color = NA,
                    normalize = 'all',
                    # position = position_dodge(1),
                    point_color = NA,
                    interval_color = NA) +
  geom_hline(yintercept = 0, col = rgb(.5, .5, .5, .7), lty = 2) +
  scale_fill_manual(name = 'Decision',
                    values = c(rgb(.7, .3, .1, .5), rgb(.1, .3, .7, .5))) +
  scale_y_continuous(bquote(phi * 'V'[iv]),
                     breaks = seq(-6, 6, 2),
                     limits = c(-6, 5)
                     ) +
  geom_text(data = data.frame(x = 3.8,
                              y = seq(-4, 4, 2),
                              l = as.character( round(softmax(seq(-4, 4, 2)), 2)) ),
            mapping = aes(x = x, y = y, label = l),
            size = 2.5) +
  scale_x_discrete(name = 'Vaccination attitude',
                   labels = c('Anti', 'Neutral', 'Pro'),
                   expand = c(.1, 0)
  ) +
  ggtitle('Individual vaccine valuation') +
  theme_bw() +
  theme(legend.position = c(.55, .8),
        strip.background = element_blank(),
        plot.title = element_text(hjust = .5),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(5, 'pt'),
        legend.title = element_text(size = 10),
        legend.background = element_rect(fill = NA),
        strip.text = element_blank())

# figure with the rb-vax posteriors
rb_vax$gg = ifelse(rb_vax$variable %in% paste0('coo', 1:3), 'co', 'vt')
rb_vax$variable = factor(rb_vax$variable,
                         levels = c(paste0('coo', 1:3), paste0('vt', 1:3),'rb'),
                         labels = c('USA', 'China', 'Other', 'Vector', 'mRNA', 'Other', 'rb'),
                         ordered = T)

vax_plt = ggplot(rb_vax[rb_vax$variable != 'rb', ],
                 mapping = aes(x = variable,
                               y = value)) +
  stat_pointinterval(mapping = aes(color = at,
                                   fill = at),
                     position = 'dodge',
                     point_color = NA) +
  scale_fill_manual(name = 'Vax attitude', 
                    values = viridis(3, .8),
                    labels = c('Anti', 'Neutral', 'Pro')) +
  scale_color_manual(name = 'Vaccination attitude', 
                     values = viridis(3, .8),
                     labels = c('Anti', 'Neutral', 'Pro')) +
  scale_y_continuous(bquote(beta['v']),
                     breaks = seq(-6, 6, 2),
                     limits = c(-6, 5)) +
  geom_hline(yintercept = 0, col = rgb(.5, .5, .5, .7), lty = 2) +
  scale_x_discrete(name = 'Coefficient',
                   expand = c(.1, 0)) +
  ggtitle('Vaccine effects') +
  guides(fill = 'none') +
  facet_wrap(~gg,
             scales = 'free_x') +
  theme_bw() +
  theme(legend.position = 'none',
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        plot.title = element_text(hjust = .5),
        strip.background = element_blank(),
        strip.text = element_blank()
        # axis.text.y = element_blank()
  )

# individual refuse--accept bias
rb_plt = ggplot(data = irbs) +
  stat_pointinterval(data = rb_vax[rb_vax$variable == 'rb', ],
                     mapping = aes(x = at,
                                   y = value,
                                   color = at,
                                   fill = at),
                     position = position_nudge(x = -.1),
                     point_color = NA) +
  stat_histinterval(mapping = aes(x = at,
                                  y = irb,
                                  fill = at),
                    breaks = 'scott',
                    outline_bars = F,
                    slab_color = NA,
                    normalize = 'all',
                    position = position_dodge(1),
                    point_color = NA,
                    interval_color = NA,
                    alpha = .4) +
  geom_hline(yintercept = 0, col = rgb(.5, .5, .5, .7), lty = 2) +
  scale_x_discrete(name = 'Vaccination attitude',
                   labels = c('Anti', 'Neutral', 'Pro'),
                   expand = c(.05, 0)) +
  scale_fill_manual(values = viridis(3, .7)) +
  scale_color_manual(values = viridis(3, .7)) +
  scale_y_continuous(bquote(beta[i]),
                     breaks = seq(-6, 6, 2),
                     labels = c('', 'refuse', '', 'indifference', '', 'accept', ''),
                     limits = c(-6, 5)
                     # , sec.axis = dup_axis(name = 'p(vax accept)')
                     ) +
  ggtitle('Decision bias') +
  theme_bw() +
  theme(legend.position = 'none',
        # axis.title.x = element_blank(),
        axis.text.y.right = element_blank(),
        axis.text.y.left = element_text(angle = 90, hjust = .5, vjust = .5),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(hjust = .5))

# final figures -----------------------------------------------------------

rm(list = setdiff(ls(), c('sv_plt', 'vax_plt', 'rb_plt')))
save.image("03_results/Fig06_top.RData")