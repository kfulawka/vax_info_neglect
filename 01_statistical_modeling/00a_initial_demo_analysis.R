rm(list = ls())

library(ggplot2)
library(viridis)
library(brms)
library(future.apply)
library(patchwork)

source('01_statistical_modeling/functions/00_cross_tab_ggplot.R')
source('03_results/functions/99_fig_to_pdf.R')

# data --------------------------------------------------------------------

load("01_statistical_modeling/data_list.RData")

# survey data
d = dd$survey

d$covid_vax_attitude = factor(d$covid_vax_attitude,
                              levels = c('Against', 'Neutral', 'Pro'),
                              labels = c('Anti', 'Neutral', 'Pro'))

# vector with grouping variables of interest
preds = c('sex', 'race', 'education', 'politics', 'income', 
          'covid_vax_no', 'covid_no', 'get_covid', 'age_c')

preds_n = c('sex', 'race', 'education', 'politics', 'income', 
            'number of vaccinations', 'COVID-19 infections', 
            'I will get C-19', 'age'); names(preds_n) = preds

titl_xt = c(F, T, T, T, T, F, F, F, F); names(titl_xt) = preds

# dv
y = "covid_vax_attitude"

# basic stats -------------------------------------------------------------

mean(as.numeric(d$age), na.rm = T); sd( as.numeric(d$age))
table(d$sex)

# sample characteristic -----------------------------------------------------

# for each control variable
desc_plts = lapply(preds, function(x) {
  
  cross_tab_ggplot(x, 
                   g = 'covid_vax_attitude',
                   g_n = 'Vaccination attitude',
                   x_n = preds_n[x],
                   tilt_x_txt = titl_xt[x],
                   data = d,
                   g_cols = viridis::viridis(3, .5))
  
})

# into single panel
desc_plt = wrap_plots(plotlist = desc_plts,
                      ncol = 3,
                      nrow = 3,
                      guides = 'collect') & theme(legend.position = 'bottom')

# save to file
pdf_save(path = '04_online_supplement/01_study_sample/Fig_initial_sample_characteristic_desc.pdf',
         fig = desc_plt,
         height = 10,
         width = 16,
         scale = 1.5)

# # vax no characteristic -----------------------------------------------------
# 
# # for each control variable
# desc_plts = lapply(c(preds[-which(preds == 'covid_vax_no')],
#                      'covid_vax_attitude'), function(x) {
#                        
#                        cross_tab_ggplot(x,
#                                         g = 'covid_vax_no')
#                        
#                      })
# 
# # into single panel
# desc_plt = wrap_plots(plotlist = desc_plts,
#                       ncol = 3,
#                       nrow = 3,
#                       guides = 'collect') & theme(legend.position = 'bottom')
# 
# # save to file
# ggsave('04_online_supplement/01_study_sample/Fig_initial_vax_no_characteristic_descc.jpg',
#        plot = desc_plt,
#        units = 'cm',
#        height = 14,
#        width = 16,
#        dpi = 700,
#        scale = 1.75)