rm(list = ls())

library(ggplot2)
library(viridis)
library(brms)
library(future.apply)
library(patchwork)

# data --------------------------------------------------------------------

source('01_statistical_modeling/00b_recoding.R')
source('03_results/functions/99_fig_to_pdf.R')

source('01_statistical_modeling/functions/00_cross_tab_ggplot.R')

# survey data
d = dd$survey

# vector with grouping variables of interest
preds = c('sex', 'race', 'education', 'politics', 'income', 
          'covid_vax_no', 'covid_no', 'get_covid', 'age_c', 'covid_vax_attitude')

preds_n = c('sex', 'race', 'education', 'politics', 'income', 
            'number of vaccinations', 'COVID-19 infections', 
            'I will get C-19', 'age', 'Vaccination attitude'); names(preds_n) = preds

titl_xt = c(F, F, T, T, T, F, F, F, F, F); names(titl_xt) = preds

# sample characteristic -----------------------------------------------------

source('01_statistical_modeling/functions/00_cross_tab_ggplot.R')

# for each control variable
desc_plts = lapply(preds[-10], function(x) {
  
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
pdf_save(path = '04_online_supplement/01_study_sample/Fig_recoded_sample_characteristic_desc.pdf',
         fig = desc_plt,
         height = 10,
         width = 16,
         scale = 1.5)

# vax no characteristic -----------------------------------------------------

# recode vax into binary
d$covid_vax_no2 = factor(d$covid_vax_no,
                         levels = levels(d$covid_vax_no),
                         labels = c('0', '1-4', '1-4'),
                         ordered = T)

# for each control variable
desc_plts = lapply(preds[-6], function(x) {
  
  cross_tab_ggplot(x, 
                   g = 'covid_vax_no2',
                   g_n = 'Prior C-19 vaccinations',
                   x_n = preds_n[x],
                   tilt_x_txt = titl_xt[x],
                   data = d,
                   proportion = T,
                   g_cols = viridis::inferno(2, .8))
  
})

# into single panel
desc_plt = wrap_plots(plotlist = desc_plts,
                      ncol = 3,
                      nrow = 3,
                      guides = 'collect') & theme(legend.position = 'bottom')

# save to file
pdf_save(path = '04_online_supplement/01_study_sample/Fig_recoded_sample_prop_desc_VaxNo.pdf',
         fig = desc_plt,
         height = 10,
         width = 16,
         scale = 1.5)