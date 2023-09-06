rm(list = ls(all.names = T))

library(loo)
library(ggplot2)
library(patchwork)

# modeling results --------------------------------------------------------

load("02_computational_modeling/posteriors/01_ispt_vc_irb_ct.RData")
load("02_computational_modeling/posteriors/02_irb_ct.RData")
load("02_computational_modeling/posteriors/03_ioh_rb_ct.RData")
load("02_computational_modeling/posteriors/00_ispt_regular.RData")

# 
mods = list(pt_reg = ispt_reg,
            db = irb,
            oh = ioh,
            pt = ispt)


# elpd comparison ---------------------------------------------------------

source('02_computational_modeling/00_functions/elpd_plots.R')

pp = tot_elpd_plot(mods,
                   g = c('anti', 'neu', 'pro'),
                   m_names = names(mods),
                   comp_list = list(oh_db = c(3, 2), pt_oh = c(4, 3), pt_db = c(4, 2)),
                   # comp_names = names(comp_list),
                   # loo_weights = F,
                   write = T,
                   save_path = '04_online_supplement/03_computational_modeling/Fig03a_performance_comparison.jpg')
