library(ggplot2)
library(patchwork)
library(cowplot)

rm(list = ls())

ps = plot_spacer()

# arrows ------------------------------------------------------------------

# timeline
time_plt = ggplot() +
  geom_segment(aes(x = 0, y = .5, xend = 1, yend = .5),
               arrow = arrow(length = unit(.25, "cm"),
                             type = 'closed'),
               col = 'darkviolet') +
  geom_text(aes(x = c(0, .35),
                y = .7,
                label = c('Information presentation',
                          'Cognitive processing')),
            hjust = c(0, 0),
            size = 5,
            col = 'black') +
  geom_text(aes(x = c(0, 1),
                y = .25,
                label = c('0', 'Time')),
            hjust = c(0, 1),
            size = 5,
            col = 'darkviolet') +
  scale_y_continuous(limits = 0:1,
                     expand = c(0, 0)) +
  scale_x_continuous(limits = 0:1,
                     expand = c(0, 0)) +
  theme_nothing()

# arrow 1
arrow1_plt = ggplot() +
  geom_segment(aes(x = 0, y = .63, xend = 1, yend = .77),
               arrow = arrow(length = unit(.25, "cm"),
                             type = 'closed'),
               col = 'darkviolet') +
  geom_segment(aes(x = 0, y = .25, xend = 1, yend = .75),
               arrow = arrow(length = unit(.25, "cm"),
                             type = 'closed'),
               col = 'darkviolet') +
  geom_segment(aes(x = 0, y = .23, xend = 1, yend = .23),
               arrow = arrow(length = unit(.25, "cm"),
                             type = 'closed'),
               col = 'darkviolet') +
  geom_text(aes(x = c(.5, .5, .5),
                y = c(.74, .56, .26),
                label = c('b', 'c', 'd')),
                size = 5,
                col = 'darkviolet',
            fontface = 'bold') +
  scale_y_continuous(limits = 0:1,
                     expand = c(0, 0)) +
  scale_x_continuous(limits = 0:1,
                     expand = c(0, 0)) +
  theme_nothing()

# arrow 2
V_v = expression(paste(italic(V)[italic(iv)]) == sum('w(p)v(x)'))

arrow2_plt = ggplot() +
  geom_segment(aes(x = 0, y = .76, xend = 1, yend = .25),
               arrow = arrow(length = unit(.25, "cm"),
                             type = 'closed',
                             ends = 'both'),
               col = 'darkviolet') +
  geom_segment(aes(x = 0, y = .23, xend = 1, yend = .23),
               arrow = arrow(length = unit(.25, "cm"),
                             type = 'closed',
                             ends = 'both'),
               col = 'darkviolet') +
  annotate('text',
           x = .5, y = .15,
           label = as.character(V_v),
           parse = T,
           size = 5,
           col = rgb(.1, .1, .1, .7)) +
  scale_y_continuous(limits = 0:1,
                     expand = c(0, 0)) +
  scale_x_continuous(limits = 0:1,
                     expand = c(0, 0)) +
  theme_nothing()

# arrow 3
p_accept_beta = expression(paste("P(accept)" %prop% beta[italic(i)] + beta[italic(v)] ))

arrow3_plt = ggplot() +
  geom_segment(aes(x = 0, y = .4, xend = 1, yend = .4),
               arrow = arrow(length = unit(.25, "cm"),
                             type = 'closed'),
               col = 'darkviolet') +
  annotate('text',
           x = .5, y = .6,
           label = as.character(p_accept_beta),
           parse = T,
           size = 5,
           col = rgb(.1, .1, .1, .7)) +
  geom_text(aes(x = c(.1),
                y = c(.53),
                label = c('a')),
            size = 5,
            col = 'darkviolet',
            fontface = 'bold') +
  scale_y_continuous(limits = 0:1,
                     expand = c(0, 0)) +
  scale_x_continuous(limits = 0:1,
                     expand = c(0, 0)) +
  theme_nothing()

# arrow 4
p_accept = expression(paste("P(accept)" %prop% beta[italic(i)] + beta[italic(v)] + italic(V)[italic(iv)]))

arrow4_plt = ggplot() +
  geom_segment(aes(x = .43, y = 0, xend = .5, yend = .95),
               arrow = arrow(length = unit(.25, "cm"),
                             type = 'closed'),
               col = 'darkviolet') +
  annotate('text',
           x = .5, y = .5,
           label = as.character( p_accept ),
           parse = T,
           size = 5,
           col = rgb(.1, .1, .1, .7)) +
  scale_y_continuous(limits = 0:1,
                     expand = c(0, 0)) +
  scale_x_continuous(limits = 0:1,
                     expand = c(0, 0)) +
  theme_nothing()

# info pres ---------------------------------------------------------------

vax_info_plt = function(t = 'Vaccine info presentation',
                        st = NULL,
                        xp = c('Outcome (x)', '??', '??',
                               'Probability (p)', '??', '??'),
                        x = rep(c(.25, .75), each = 3),
                        y = rep(c(5/6, 3/6, 1/6), times = 2)) {
  
  d = data.frame(xp = xp,
                 x = x,
                 y = y)
  
  p = ggplot() +
    geom_text(data = d[c(1, 4), ],
              mapping = aes(x = x, 
                            y = y,
                            label = xp,
                            fontface = 'bold'),
              size = 3,
              hjust = .55) +
    geom_text(data = d[-c(1, 4), ],
              mapping = aes(x = x, 
                            y = y,
                            label = xp),
              size = 3,
              hjust = .55,
              color = rep(c('blue', 'red'), times = 2)) +
    ggtitle(t, st) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = 'white'),
          plot.title = element_text(size = 12, 
                                    colour = 'black'),
          plot.subtitle = element_text(size = 12, 
                                    colour = 'black'),
          panel.border = element_rect(colour = "black", 
                                      fill = NA,
                                      linewidth = 2)) +
    geom_hline(yintercept = c(1/3, 2/3),
               linewidth = 1) +
    geom_vline(xintercept = .5,
               linewidth = 1)
  
  return(p)
  
}

nin = vax_info_plt(t = 'No information neglect',
                        xp = c('Outcome (x)', 'Protection against\ninfection', 'Blood clots',
                               'Probability (p)', '95%', '2/1,000,000'))

apn = vax_info_plt(t = 'Selective information neglect',
                   st = 'Attentional probability neglect',
                   xp = c('Outcome (x)', 'Protection against\ninfection', 'Blood clots',
                          'Probability (p)', '[not read]', '[not read]'))
apn = apn + theme(plot.subtitle = element_text(size = 12, color = 'darkorange'))


tin = vax_info_plt(t = 'Complete information neglect',
                   xp = c('Outcome (x)', '[not read]', '[not read]',
                          'Probability (p)', '[not read]', '[not read]'))

# PWFs --------------------------------------------------------------------

pwf = function(x, d = 1, g) {
  
  exp( -d * (-log(x))^g )
  
}

# probs
p = seq(0, 1, by = .001)

dd = data.frame(p = p,
                wp = c(pwf(p, d = 1, g = 1),
                       pwf(p, d = 1, g = .3)
                       # pwf(p, d = .2, g = .2)
                       ),
                g = factor(rep(c('Objective', 'Distorted'#, 'Affective'
                                 ), 
                               each = length(p)),
                           levels = c('Objective', 'Distorted'#, 'Affective'
                                      ),
                           ordered = T)
)

# figure
pwf_plt = ggplot(data = dd,
                 mapping = aes(x = p, 
                               y = wp,
                               linetype = g)) +
  geom_line(linewidth = 1,
            color = 'darkorange',
            alpha = .9) +
  scale_linetype_manual(values = c(3, 1)) +
  scale_x_continuous('p [probability]',
                     breaks = seq(0, 1, .2)) +
  scale_y_continuous('w(p)',
                     breaks = seq(0, 1, .2)) +
  ggtitle('Probability sensitivity') +
  theme_bw() +
  theme(legend.position = c(.75, .25),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        plot.title = element_text(colour = 'darkorange', 
                                  size = 12),
        panel.border = element_rect(colour = "black", 
                                    fill = NA,
                                    linewidth = 2)) + 
  annotate("rect", xmin = 0, xmax = .1, 
           ymin = 0, ymax = 1,
           fill = rgb(.9, .1, .1, .3)) +
  annotate("rect", xmin = .5, xmax = .99, 
           ymin = 0, ymax = 1,
           fill = rgb(.1, .1, .9, .3)) 

# neglect
neg_plt = ggplot() +
  geom_segment(mapping = aes(x = 0,
                             y = 1, 
                             xend = 1, 
                             yend = 1,
                             linetype = 'Certain'),
               linewidth = 1,
               color = 'darkorange') +
  geom_segment(mapping = aes(x = 0,
                             y = .5, 
                             xend = 1, 
                             yend = .5,
                             linetype = 'Possible'),
               linewidth = 1,
               color = 'darkorange') +
  scale_x_continuous('p [probability]',
                     breaks = seq(0, 1, .2),
                     limits = 0:1) +
  scale_y_continuous('w(p)',
                     breaks = seq(0, 1, .2),
                     limits = 0:1) +
  scale_linetype_manual(name = 'Outcome seems', 
                        values = c(1, 3)) +
  ggtitle('Decisional probability neglect') +
  theme_bw() +
  theme(legend.position = c(.5, .73),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = "transparent"),
        plot.title = element_text(color = 'darkorange', 
                                  size = 12),
        panel.border = element_rect(colour = "black", 
                                    fill = NA,
                                    linewidth = 2)) + 
  annotate("rect", xmin = 0, xmax = .1, 
           ymin = 0, ymax = 1,
           fill = rgb(.9, .1, .1, .3)) +
  annotate("rect", xmin = .5, xmax = .99, 
           ymin = 0, ymax = 1,
           fill = rgb(.1, .1, .9, .3)) +
  geom_text(data = data.frame(x = c(0, .5),
                              y = c(.1, .1),
                              l = c('Probabilities of\nside effects',
                                    'Vaccine\neffectiveness')),
            mapping = aes(x = x, y = y, label = l),
            color = c(rgb(.9, .1, .1, .9),
                      rgb(.1, .1, .9, .9)),
            hjust = 0,
            size = 3)

# outcome representation --------------------------------------------------

# value function
v_fun = function(x, a, l, p = 1) {
  
  sv = sapply(x, function(y) {
    if(y > 0)  l = 1 
    
    p * sign(y) * l * abs(y)^a
    
  })
  
  return(sv)
  
}

# figure
v_plt = ggplot(data = data.frame(x = c(-5:-1, 1:5),
                                 y = v_fun(c(-5:-1, 1:5), 1, 2),
                                 og = rep(c('Side effects', 'Effectiveness'), 
                                          each = 5)),
               mapping = aes(x = x, 
                             y = y,
                             col = og)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1,
              col = 'grey') +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_color_manual(name = "Outcome",
                     values = c(rgb(.1, .1, .9, .5),
                                rgb(.9, .1, .1, .5))) +
  scale_x_continuous('a [affect rating]',
                     breaks = -5:5) +
  scale_y_continuous('v(a)',
                     breaks = seq(-10, 10, 2),
                     limits = c(-10, 10)) +
  ggtitle('Loss aversion') +
  theme_bw() +
  theme(legend.position = c(.25, .8),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.title = element_blank(),
        plot.title = element_text(colour = 'black', size = 12),
        panel.border = element_rect(colour = "black", 
                                    fill = NA,
                                    linewidth = 2))


# choice ------------------------------------------------------------------

choice_plt = ggplot() +
  geom_text(aes(x = .5, 
                y = .5, 
                label = 'DECISION\n to accept or refuse\nthe vaccine'),
            size = 5,
            color = 'darkviolet') +
  ylim(0, 1) +
  xlim(0, 1) +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", 
                                    fill = NA,
                                    linewidth = 2))

# figure ------------------------------------------------------------------

f0a = (tin / ps / apn / ps / nin) +
  plot_layout(heights = c(1, .15, 1.1, .15, 1))

f0b = ((arrow1_plt | (neg_plt / pwf_plt) | arrow2_plt )) + 
  plot_layout(widths = c(.45, 1, .7))

f0c = (arrow3_plt / f0b) + 
  plot_layout(heights = c(1, 5))

f0d1 = (v_plt | ps) + plot_layout(widths = c(1, .1))
        
f0d = (choice_plt / arrow4_plt / f0d1)

f0 = (ps | f0a | f0c | f0d) +
  plot_layout(4, 1, widths = c(.03, .87, 2.2, 1))

fig0 = (f0 / time_plt) +
  plot_layout(heights = c(1, .1))

ggsave('03_results/figures/Fig00.jpg',
       plot = fig0,
       units = 'cm',
       height = 10,
       width = 16,
       dpi = 700,
       scale = 1.75)