rm(list = ls())

library(ggplot2)
library(patchwork)
library(viridis)
library(brms)
library(data.table)
library(ggdist)

# modeling results --------------------------------------------------------

load("S:/arc_research/Vaccines/analyses/07_affect_ratings.RData")

# conditional effects -----------------------------------------------------

ce_dat = lapply(names(mods), function(m) {

  # posterior predicted scores
  pp = posterior_epred(mods[[m]]$m2,
                       newdata = data.frame(covid_vax_attitude = c('Against', 'Neutral', 'Pro'),
                                            event = NA),
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

  #
  return(pa)
  
}); names(ce_dat) = names(mods)

# names
ev_name = c('Extreme side effect', 'Severe side effect', 'Mild side effect', 'Benefits')
names(ev_name) = names(mods)

# figure ------------------------------------------------------------------

ar_plt = lapply(names(ce_dat), function(x) {
  
  p = ggplot(data = ce_dat[[x]],
         mapping = aes(x = value,
                       fill = variable)) + 
    geom_density(color = rgb(1, 1, 1, 0)) +
    xlim(ifelse(x == 'positive', 1, -5),
         ifelse(x == 'positive', 5, -1)) +
    scale_fill_manual(name = 'Vaccination attitude',
                      values = viridis(3, .5)) +
    ggtitle(ev_name[x]) + 
    xlab(ifelse(x == 'positive', 
                'Average positive affect',
                'Average negative affect')) +
    theme_bw() +
    theme(axis.text.y = element_blank())
  
  if(x != 'extreme') {
    
    p = p + theme(axis.title.y = element_blank())
    
  }
  
  if(x != 'positive') {
    
    p = p + theme(plot.title = element_text(color = rgb(.9, .1, .1, .7))
                  # axis.title.x = element_text(color = rgb(.9, .1, .1, .7)),
                  # axis.text.x = element_text(color = rgb(.9, .1, .1, .7))
                  )
    
  } else {
    
    p = p + theme(plot.title = element_text(color = rgb(.1, .1, .9, .7))
                  # axis.title.x = element_text(color = rgb(.1, .1, .9, .7)),
                  # axis.text.x = element_text(color = rgb(.1, .1, .9, .7))
                  )

  }
  
  return(p)
  
})

Fig05 = wrap_plots(ar_plt, nrow = 1, ncol = 4) + 
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

# save to file
ggsave('results/figures/Fig05.jpg',
       plot = Fig05,
       units = 'cm',
       height = 5,
       width = 16,
       dpi = 700,
       scale = 1.5)
