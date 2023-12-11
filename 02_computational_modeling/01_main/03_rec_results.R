rm(list = ls())

library(ggplot2)
library(data.table)
library(patchwork)

source('03_results/functions/99_fig_to_pdf.R')

# generating models
load("02_computational_modeling/posteriors/01_ispt_vc_irb_ct.RData")

# recovered parameters
load('02_computational_modeling/posteriors/01b_ispt_vc_irb_ct_recovery.RData')

# individual level recovery -----------------------------------------------

# ind-lvl parameters
ipars = c('ibeta', 'igam', 'irb'); names(ipars) = ipars
ipars_n = c('lambda_i', 'gamma_i', 'beta_i'); names(ipars_n) = ipars

# scatterplots
ip_splts = lapply(names(ispt), function(at) {
  
  # gen model
  gm = ispt[[at]]
  
  # rec model
  rm = ispt_rec[[at]]
  
  # loop over id parameters
  ip_plts = lapply(ipars, function(x) {
    
    d = data.frame(gp = colMeans(gm$pars$i_pars[[x]]),
                   rp = colMeans(rm$pars$i_pars[[x]])
    )
    
    # limits 
    if(x == 'irb') ll = c(-6, 4) else ll = c(0, 1)
    
    # plot
    plt = ggplot(d, mapping = aes(x = gp,
                                  y = rp)) + 
      geom_point(alpha = .5) +
      scale_x_continuous(paste0('Generating ', ipars_n[x]),
                         limits = ll) +
      scale_y_continuous(paste0('Recovered ', ipars_n[x]),
                         limits = ll) +
      geom_abline(slope = 1, intercept = 0, col = 'red') +
      ggtitle(at,
              paste0('r = ', round(cor(d)[1,2], 2))) +
      theme_bw()
    
  })
  
  ip_plt = wrap_plots(ip_plts, nrow = 1, ncol = 3)
  
  
});

ip_splt = wrap_plots(ip_splts, nrow = 3, ncol = 1)

pdf_save(path = '04_online_supplement/03_computational_modeling/Fig01a_individual_parameter_recovery.pdf',
         fig = ip_splt,
         height = 16, 
         width = 16,
         scale = 1.2)

# pop-lvl parameters ------------------------------------------------------

# pop-lvl parameters
ppars = c('alpha', 'beta', 'gam', 'phi', 'rb'); names(ppars) = ppars
ppars_n = c('alpha', 'lambda', 'gamma', 'phi', 'beta'); names(ppars_n) = ppars

lls = list(c(0, 6), 0:1, 0:1, 0:1, c(-5, 2))
names(lls) = ppars

# histograms
pp_hists = lapply(names(ispt), function(at) {
  
  # gen model
  gm = ispt[[at]]
  
  # rec model
  rm = ispt_rec[[at]]
  
  # loop over id parameters
  ip_plts = lapply(ppars, function(x) {
    
    d = data.frame(y = c(gm$pars$p_pars[[x]], rm$pars$p_pars[[x]]),
                   g = rep(c('generating', 'recoevered'), each = length(gm$pars$p_pars[[x]]))
    )
    
    # plot
    plt = ggplot(d, mapping = aes(x = y,
                                  col = g)) + 
      geom_density(alpha = .8) +
      scale_color_manual('Parameter',
                         values = c('blue', 'red')) +
      xlab(ppars_n[x]) +
      xlim(lls[[x]]) +
      ggtitle(at) +
      theme_bw() +
      theme(legend.position = 'top',
            axis.title.y = element_blank(),
            axis.text.y = element_blank())
    
  })
  
  ip_plt = wrap_plots(ip_plts, nrow = 1, ncol = 5)
  
  
});

pp_hist = wrap_plots(pp_hists, nrow = 3, ncol = 1) + 
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

pdf_save(path = '04_online_supplement/03_computational_modeling/Fig01b_population_parameter_recovery.pdf',
         fig = pp_hist,
         height = 12, 
         width = 16,
         scale = 1.2)