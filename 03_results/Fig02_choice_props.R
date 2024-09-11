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
       covid_vax_no = 'Prior C-19 vaccinations',
       covid_no = 'I had C-19',
       get_covid = 'I will get C-19'
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


# version two of attiutde x vax interaction (for npj revision) ------------

cv_vax2 = conditional_effects(m01_props,
                         effects = 'covid_vax_attitude:vax',
                         conditions = cc)[[1]]


cv_vax2$vax = factor(cv_vax2$vax, 
                      levels = c('Sinovac', 'CanSino Biologics', 'Johnson & Johnson', 'AstraZeneca', 
                                 'Novavax', 'Moderna', 'Bharat Biotech', 'BioNTech/Pfizer'), 
                      ordered = T)

# data
cv_vax2_o = aggregate(choice ~ covid_vax_attitude + vax,
                      data = d,
                      FUN = mean)

# pred post probs
cv_vax2_plt = ggplot(cv_vax2,
               mapping = aes(y = vax,
                             color = covid_vax_attitude)) +
  geom_linerange(mapping = aes(xmin = lower__,
                               xmax = upper__,),
                 linewidth = .5,
                 alpha = .7,
                 show.legend = F) +
  geom_point(mapping = aes(x = estimate__),
             shape = 3,
             show.legend = F) +
  geom_point(data = cv_vax2_o,
             mapping = aes(x = choice),
             shape = 4) +
  ylab('') +
  facet_wrap(~covid_vax_attitude) +
  scale_x_continuous('P(accept)',
                     breaks = seq(0, 1, .2),
                     limits = 0:1) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.22, .7),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA))



# into single panel -------------------------------------------------------

fig02_top = (plts$education | plts$politics | plts$income)

fig02_mid = (plts$age_c | plts$covid_vax_no | plts$covid_no | plts$get_covid) +
  plot_layout(nrow = 1, widths = c(4, 2.9, 2.3, 2.3)) &
  theme(plot.margin = unit(c(16.5, 5.5, 5.5, 5.5), 'pt'))

fig02_bottom = (plts$covid_vax_attitude | cv_vax2_plt) +
plot_layout(nrow = 1, widths = c(1, 3)) &
  theme(plot.margin = unit(c(16.5, 5.5, 5.5, 5.5), 'pt'))
  

fig02 = fig02_top / fig02_mid / fig02_bottom + 
  plot_layout(heights = c(1, 1, 1)) 

# save to file
source('03_results/functions/99_fig_to_pdf.R')
pdf_save(path = '03_results/figures/Fig02_v2.pdf',
         fig = fig02,
         height = 11,
         width = 16,
         scale = 1.4)