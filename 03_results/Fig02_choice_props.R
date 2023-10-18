rm(list = ls())

library(ggplot2)
library(patchwork)
library(viridis)
library(brms)
library(future.apply)
# library(emmeans)

# data --------------------------------------------------------------------

# data prepared for the analyses
load('01_statistical_modeling/data_analyses.RData')
rm(list = setdiff(ls(), c('d', 'preds')))

#modeling results
load("01_statistical_modeling/01_choice_proportions.RData")

# set variables -----------------------------------------------------------

# predictor names
nn = c(vax = 'Vaccine',
       # sex = 'Sex', 
       age_c = 'Age', 
       politics = 'Politics',
       covid_vax_attitude = 'Vaccination attitude',
       # race = 'Race', 
       education = 'Education',
       income = 'Income', 
       covid_vax_no = 'Number of vaccinations',
       covid_no = 'I had COVID',
       get_covid = 'I will get COVID'
)


# predictors
mod_preds = c(preds, 'covid_vax_attitude', 'vax', 'vax:covid_vax_attitude')

# conditions
cc = data.frame(matrix(NA,
                       nrow = 1,
                       ncol = length(mod_preds)-1,
                       dimnames = list(NULL, mod_preds[-length(mod_preds)])))

# create panels -----------------------------------------------------------

source('03_results/functions/00_choice_pred_plt.R')

plan( multisession(workers = length(mod_preds)) )
plts = lapply(mod_preds, function(x) {
  
  plt_p_pred_data(m = m01_props,
                  conditions = cc,
                  pred = x,
                  add_data = T,
                  b_col = rgb(.1, .1, .8, .9))
}); names(plts) = mod_preds
plan(sequential)

# customization
plts = lapply(mod_preds, function(x) {

  if(x == 'education') {
    
    # get panel
    p = plts[[x]]
    
  } else {
   
    # get panel
    p = plts[[x]] + guides(shape = 'none')
     
  }
  
  # add proper x-axes names
  if(x != mod_preds[length(mod_preds)]) { # for all except the interaction plot
    
    p = p + xlab(nn[x]) # ggtitle(nn[[x]])
    
  } else { # if this is the interaction plot make all modifications neccessary
    
    p = p + # ggtitle(nn['vax']) + 
      guides(color = guide_legend('Vax attitude')) +
      xlab(nn['vax']) +
      theme(legend.position = c(.1, .64),
            legend.background = element_blank())
  }
  
  # tilt long level names
  if(x %in% c('education', 'politics', 'income', 'vax', 'vax:covid_vax_attitude')) {
    
    p = p + theme(axis.text.x = element_text(angle = 13))
    
  }

  # remove ylabs from inside figs
  if( !(x %in% c('age_c', 'education', 'covid_vax_attitude')) ) {
    
    p = p + theme(axis.title.y = element_blank())
    
  }
  
  # adjust x labs
  if(x == 'covid_no') p = p + scale_x_discrete(labels = c('No', 'Yes'))
  if(x == 'get_covid') p = p + scale_x_discrete(labels = c('Not likely', 'Likely'))

    # # remove ylabs from inside figs
  # if( x %in% c('vax:covid_vax_attitude', 'covid_vax_no', 'covid_vax_attitude') ) {
  #   
  #   p = p + theme(panel.border = element_rect(colour = "blue", 
  #                                             fill = NA, 
  #                                             linewidth = 1.5))
  #   
  # }
  
  return(p)
  
}); names(plts) = mod_preds

# into single panel -------------------------------------------------------

fig02_top = (plts$education | plts$politics | plts$income)

fig02_mid = (plts$age_c | plts$covid_vax_no | plts$covid_no | plts$get_covid) +
  plot_layout(nrow = 1, widths = c(4, 2.9, 2.3, 2.3)) &
  theme(plot.margin = unit(c(16.5, 5.5, 5.5, 5.5), 'pt'))

fig02_bottom = (plts$covid_vax_attitude | plts$`vax:covid_vax_attitude`) +
plot_layout(nrow = 1, widths = c(1, 3)) &
  theme(plot.margin = unit(c(16.5, 5.5, 5.5, 5.5), 'pt'))
  

fig02 = fig02_top / fig02_mid / fig02_bottom + 
  plot_layout(heights = c(1, 1, 1)) 

# save to file
ggsave('03_results/figures/Fig02.pdf',
       plot = fig02,
       device = 'pdf',
       units = 'cm',
       height = 11,
       width = 16,
       scale = 1.4)
