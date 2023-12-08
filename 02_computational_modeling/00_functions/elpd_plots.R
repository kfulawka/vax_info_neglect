# this function plots 
# 1. total model performance 
# 2. model comparisons within each group
# input is a list of loos returned by 01_extract_loos function

library(ggplot2)
library(ggpubr)
library(data.table)
library(viridis)

source('02_computational_modeling/00_functions/extract_loos.R')
source('03_results/functions/99_fig_to_pdf.R')

tot_elpd_plot = function(M, 
                         g = c('anti', 'neu', 'pro'),
                         m_names = names(M),
                         comp_list = list(),
                         comp_names = names(comp_list),
                         # loo_weights = F,
                         write = F,
                         save_path = 'results/Fig_ELPDS.jpg') {
  
  # useful variables --------------------------------------------------------
  
  # group names
  vax_g_n = names(M[[1]])
  vax_g_names = g
  names(vax_g_names) = vax_g_n
  
  # vax groups colors
  vax_col = viridis(3, .9)
  names(vax_col) = vax_g_n
  
  # model symbols
  mod_sym = 1:length( m_names )
  names(mod_sym) = m_names
  
  
  # extract loos ------------------------------------------------------------
  lls = extract_loos(M,
                     m_names = m_names,
                     g_names = vax_g_n)
  
  # elpd plot ---------------------------------------------------------------
  
  # total elpds
  elpd_dat = lapply(lls, elpds_plot, relative = T)
  elpd_dat = data.frame( rbindlist(elpd_dat) )
  elpd_dat$covid_vax_attitude = rep(vax_g_n, each = length(lls$anti))
  
  elpd_dat$model = factor(elpd_dat$model,
                          levels = m_names,
                          ordered = T)
  
  # ggplot
  tot_loo_plt = ggplot(data = elpd_dat,
                       mapping = aes(x = model,
                                     y = elpd,
                                     ymin = li,
                                     ymax = ui,
                                     # shape = model,
                                     col = covid_vax_attitude)) +
    geom_point() +
    geom_errorbar(width = .2) +
    facet_wrap(~covid_vax_attitude,
               # scales = 'fixed',
               labeller = labeller(covid_vax_attitude = vax_g_names)) +
    scale_color_manual(values = vax_col) +
    xlab('Model') +
    scale_y_continuous('exp(elpd/n)',
                       breaks = seq(.5, 1, .05),
                       minor_breaks = seq(.5, 1, .01),
                       limits = c(.5, .8)) +
    geom_hline(yintercept = seq(.5, .8, .05),
               lty = 2, 
               colour = rgb(0, 0, 0, .3)
    ) +
    ggtitle('Approximate out-of-sample model performance') +
    theme_bw() +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 0))
  
  
  # elpd differences plot ---------------------------------------------------
  
  diff_loo = lapply(lls, elpd_diffs, comp_list = comp_list)
  diff_loo = data.frame( rbindlist(diff_loo) )
  diff_loo$covid_vax_attitude = rep(vax_g_n, each = length(comp_list))
  
  # figure
  diff_loo_plt = ggplot(data = diff_loo,
                        mapping = aes(x = model_comp,
                                      y = elpd_d,
                                      col = covid_vax_attitude)) +
    geom_pointrange(aes(ymin = li1,
                        ymax = ui1),
                    lwd = 1.5,
                    fill = 'white',
                    alpha = .2) + 
    geom_linerange(aes(ymin = li2,
                       ymax = ui2),
                   lwd = 1,
                   alpha = .8) +  
    geom_linerange(aes(ymin = li3,
                       ymax = ui3),
                   lwd = .5,
                   alpha = .8) +  
    facet_wrap(~covid_vax_attitude,
               scales = 'fixed',
               labeller = labeller(covid_vax_attitude = vax_g_names)) +
    scale_color_manual(values = vax_col) +
    ylab('elpd difference') +
    scale_x_discrete(name = 'Comparison of model performance'
                     # labels = comp_names
    ) +
    geom_hline(yintercept = 0,
               colour = rgb(0, 0, 0, .7),
               lty = 2) +
    ggtitle('Model comparison') +
    theme_bw() +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 0))
  
  
  # # loo weights plot --------------------------------------------------------
  # if(loo_weights) { # optional
  #   
  #   ll_w = lapply(lls, elpd_weights)
  #   
  #   # figure
  #   weights_loo_plt = ggplot(data = ll_w,
  #                            mapping = aes(x = model,
  #                                          y = loo_weight,
  #                                          fill = group)) +
  #     geom_bar(stat = 'identity') +
  #     facet_wrap(~group,
  #                scales = 'fixed',
  #                labeller = labeller(group = vax_g_names)) +
  #     scale_fill_manual(values = vax_col) +
  #     ylab('stacking weight') +
  #     scale_x_discrete(name = 'Model') +
  #     ggtitle('Model comparison',
  #             'Loo stacking weights (optimized prediction weights)') +
  #     theme_bw() +
  #     theme(legend.position = 'none',
  #           axis.text.x = element_text(angle = 0))
  #   
  #   # final plot list
  #   plt_list = list(tot_loo_plt, diff_loo_plt, weights_loo_plt)
  #   
  # } else {
  #   
  #   plt_list = list(tot_loo_plt, diff_loo_plt)
  #   
  # }
  
  # final figure ------------------------------------------------------------
  
  plt_list = list(tot_loo_plt, diff_loo_plt)
  
  elpd_plt = ggarrange(plotlist = plt_list,
                       nrow = length(plt_list))
  
  # write into file if requested
  if(write) {
    
    # save to file
    pdf_save(path = save_path,
             fig = elpd_plt,
             height = 7,
             width = 12,
             scale = 1.5)
  }
  
  # return ggpupr object
  return(elpd_plt)
  
}