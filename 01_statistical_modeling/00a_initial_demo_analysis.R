rm(list = ls())

library(ggplot2)
library(viridis)
library(brms)
library(future.apply)
library(patchwork)

source('01_statistical_modeling/functions/00_cross_tab_ggplot.R')

# data --------------------------------------------------------------------

load("01_statistical_modeling/data_list.RData")

# survey data
d = dd$survey

# vector with grouping variables of interest
preds = c('sex', 'race', 'education', 'politics', 'income', 
          'covid_vax_no', 'covid_no', 'get_covid', 'age_c')

# dv
y = "covid_vax_attitude"

# basic stats -------------------------------------------------------------

mean(as.numeric(d$age), na.rm = T); sd( as.numeric(d$age))
table(d$sex)

# sample characteristic -----------------------------------------------------

# for each control variable
desc_plts = lapply(preds, function(x) {
  
  cross_tab_ggplot(x,
                   g = 'covid_vax_attitude')
  
})

# into single panel
desc_plt = wrap_plots(plotlist = desc_plts,
                      ncol = 3,
                      nrow = 3,
                      guides = 'collect') & theme(legend.position = 'bottom')

# save to file
ggsave('04_online_supplement/01_study_sample/Fig_initial_sample_characteristic_desc.jpg',
       plot = desc_plt,
       units = 'cm',
       height = 14,
       width = 16,
       dpi = 700,
       scale = 1.75)

# vax no characteristic -----------------------------------------------------

# for each control variable
desc_plts = lapply(c(preds[-which(preds == 'covid_vax_no')],
                     'covid_vax_attitude'), function(x) {
                       
                       cross_tab_ggplot(x,
                                        g = 'covid_vax_no')
                       
                     })

# into single panel
desc_plt = wrap_plots(plotlist = desc_plts,
                      ncol = 3,
                      nrow = 3,
                      guides = 'collect') & theme(legend.position = 'bottom')

# save to file
ggsave('04_online_supplement/01_study_sample/Fig_initial_vax_no_characteristic_descc.jpg',
       plot = desc_plt,
       units = 'cm',
       height = 14,
       width = 16,
       dpi = 700,
       scale = 1.75)