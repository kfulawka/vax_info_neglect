library(loo)
library(ggplot2)
library(patchwork)
library(data.table)

rm(list = ls())

source('03_results/functions/99_fig_to_pdf.R')

# model fit ---------------------------------------------------------------

load("02_computational_modeling/posteriors/01_ispt_vc_irb_ct.RData")

# performance -------------------------------------------------------------

# balancced accuracy
lapply(ispt, function(a) a$balanced_acc$ba)

lapply(ispt, function(a) a$looE)

# figure ------------------------------------------------------------------

source('02_computational_modeling/00_functions/ind_performance_plt.R')

# colors
att_col = viridis::viridis(3, .7)

# get panels
fig_by_att = lapply(1:3, function(i) {
  
  ind_performance_plt(ispt[[i]],
                      at_col = att_col[i],
                      vax_att = names(ispt)[i],
                      ind_elpd = F)
  
})

main_id_per = wrap_plots(fig_by_att, nrow = 3, ncol = 1)

pdf_save(path = '04_online_supplement/03_computational_modeling/Fig02a_pt_ct_rb_ind_performance.pdf',
         fig = main_id_per,
         height = 13, 
         width = 14,
         scale = 1.6)


# regular spt for comparison ----------------------------------------------

load("02_computational_modeling/posteriors/00_ispt_regular.RData")

# analogous figure fo the 'regular' ispt
fig_by_att = lapply(1:3, function(i) {
  
  ind_performance_plt(ispt_reg[[i]],
                      at_col = att_col[i],
                      vax_att = names(ispt)[i],
                      ind_elpd = F)
  
})

main_id_per = wrap_plots(fig_by_att, nrow = 3, ncol = 1)

pdf_save(path = '04_online_supplement/03_computational_modeling/Fig02b_pt_standard_ind_performance.pdf',
         fig = main_id_per,
         height = 13, 
         width = 14,
         scale = 1.6)