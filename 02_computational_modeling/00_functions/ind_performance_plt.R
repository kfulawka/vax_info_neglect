# necessary functions
source('02_computational_modeling/00_functions/p_accept.R')
source('02_computational_modeling/00_functions/ind_elpd.R')

# function for plotting model performance evaluation on individual level
ind_performance_plt = function(fit,
                               at_col = rgb(.8, .1, .1, .5),
                               data_col = rgb(.5, .5, .5, .8),
                               vax_att = NULL,
                               ind_elpd = F) {
  
  # fit is a model fit object returned by my fitting script
  # for a single group!!
  
  # observed choices
  y = fit$sampling_info$data$co
  
  # loo object
  ll = fit$looE
  
  # individual elpd_loo
  ill = ind_elpd(ll$pointwise[,'elpd_loo'])
  
  # p(accept) model_loo & observed
  pa = p_accept(ll$pointwise[,'elpd_loo'],
                y = y)
  
  
  # PANEL A: P(ACCEPT|VAX) --------------------------------------------------
  
  # vaccines
  vax_n = c("Astra", "Bharat", "Pfizer", "CanSino", "J&J", "Moderna", "Novavax", "Sinovac" )
  
  pa_vax = pa$p_accept_vax
  pa_vax$vax = vax_n
  
  # order
  pa_vax = pa_vax[order(pa_vax$observed), ]
  pa_vax$ord = 1:8
  
  # into long
  pa_vax = data.frame( melt( data.table(pa_vax),
                             id.vars = c('vax', 'ord')) )
  
  # figure
  fig_a = ggplot(data = pa_vax,
                 mapping = aes(x = ord,
                               y = value,
                               color = variable)) +
    geom_point() +
    scale_y_continuous('p(accept)',
                       breaks = seq(0, 1, .2),
                       limits = 0:1) +
    scale_x_continuous('vaccine', 
                       breaks = 1:8,
                       labels = pa_vax$vax[1:8],
                       minor_breaks = NULL) +
    scale_color_manual(name = '',
                       values = c(at_col, data_col),
                       labels = c('Model', 'Data')) +
    ggtitle(vax_att) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30),
          legend.position = c(.15, .9),
          legend.background = element_rect(fill = NA),
          legend.key = element_rect(fill = NA))
  
  
  # PANEL B: P(ACCEPT|IND) --------------------------------------------------
  
  pa_ind = pa$p_accept_ind
  
  # proportion of 0-8 accepts
  prop_accept = prop.table( table(factor(pa_ind$observed,
                                         levels = c(0:8/8),
                                         ordered = T) ) )
  prop_accept = data.frame(prop_accept)
  prop_accept$Var1 = as.numeric(as.character(prop_accept$Var1))
  prop_accept$Freq = round(prop_accept$Freq, 2)
  
  # order
  pa_ind = pa_ind[order(pa_ind$observed, pa_ind$model_loo), ]
  pa_ind$ord = 1:nrow(pa_ind)
  
  # # figure
  # fig_b1 = ggplot(data = pa_ind,
  #                mapping = aes(x = ord,
  #                              y = observed)) +
  #   geom_point(col = data_col) +
  #   geom_point(aes(y = model_loo),
  #              col = at_col) +
  #   ylim(0, 1) +
  #   scale_x_continuous('participant (ordered)', 
  #                      breaks = seq(1, nrow(pa_ind), 10),
  #                      minor_breaks = NULL,
  #                      expand = c(.01, .01)) +
  #   scale_y_continuous(breaks = c(0, 1:8/8),
  #                      minor_breaks = NULL) +
  #   theme_bw()
  
  fig_b = ggplot(data = pa_ind,
                 mapping = aes(x = observed,
                               y = model_loo)) +
    geom_line(data = data.frame(x = 0:1, y = 0:1),
              mapping = aes(x = x,
                            y = y),
              lty = 2) +
    geom_boxplot(aes(group = observed),
                 fill = NA,
                 outlier.colour = NA,
                 varwidth = T) +
    geom_point(col = at_col) +
    geom_text(data = prop_accept,
              mapping = aes(x = Var1,
                            y = 1,
                            label = Freq),
              size = 2.5) +
    scale_x_continuous('observed p(accept)',
                       breaks = c(0, 1:8/8),
                       # labels = round(c(0, 1:8/8), 2),
                       minor_breaks = NULL) +
    scale_y_continuous('predicted p(accept)',
                       breaks = c(0, 1:8/8),
                       # labels = round(c(0, 1:8/8), 2),
                       minor_breaks = NULL,
                       limits = 0:1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30))
  
  
  
  # PANEL C: P(CORRECT) -----------------------------------------------------
  
  
  ill = ill[order(ill$ev_prop_cor), ]
  ill$pc_ord = 1:nrow(ill)
  
  fig_c = ggplot(data = ill,
                 mapping = aes(pc_ord, ev_prop_cor)) +
    geom_point(col = at_col) +
    scale_x_continuous('participant (ordered by y-axis)',
                       breaks = seq(1, nrow(ill), 50),
                       expand = c(.01, .01)) +
    scale_y_continuous('correct prediction probability',
                       breaks = seq(0, 1, .2),
                       limits = c(0, 1)) +
    geom_hline(yintercept = .5,
               lty = 2) +
    theme_bw()
  
  # PANEL D: IND ELPD -------------------------------------------------------
  
  if(ind_elpd) {
    
    
    ill = ill[order(ill$ill), ]
    ill$ill_ord = 1:nrow(ill)
    
    fig_d = ggplot(data = ill,
                   mapping = aes(ill_ord, ill)) +
      geom_point(col = at_col) +
      scale_x_continuous('participant (ordered by y-axis)',
                         breaks = seq(1, nrow(ill), 50),
                         expand = c(.01, .01)) +
      scale_y_continuous('individual elpd_loo',
                         limits = c(NA, 0)) +
      geom_hline(yintercept = sum(log(rep(.5, 8))),
                 lty = 2) +
      theme_bw()
    
    
    # FINAL FIGURE ------------------------------------------------------------
    
    fig = (fig_a | fig_b | fig_c | fig_d)
    
  } else {
    
    # FINAL FIGURE ------------------------------------------------------------
    
    fig = (fig_a | fig_b | fig_c )
    
  }

  return(fig)
  
}