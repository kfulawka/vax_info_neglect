library(brms)
library(loo)
library(ggplot2)
library(patchwork)

# models for evaluation ---------------------------------------------------

# choice proportions
load("01_statistical_modeling/01_choice_proportions.RData")

# info neglect
load("01_statistical_modeling/02_information_neglect.RData")

# choice and info neglect
load("01_statistical_modeling/03_choice_info_neglect.RData")

# affect ratings
load("01_statistical_modeling/04_affect_ratings.RData")

# for reproducibility
set.seed(12345)

# ELPD_loo ----------------------------------------------------------------

source('01_statistical_modeling/functions/elpd_loo_comparison.R')

# choice proportion models
m03_loos = list(m1 = loo(m03_co_in$full_v2, moment_match = TRUE),
                m0 = loo(m00_props, moment_match = TRUE))

# info neglect results
m02_loos = lapply(names(m02_in_null), function(x) {
  
  # 
  if(x == 'ineg') {
    
    res = list(m1 = loo(m02_in$ineg$full, moment_match = TRUE),
               m0 = loo(m02_in_null$ineg, moment_match = TRUE))
    
  } else {
    
    res = list(m1 = loo(m02_in[[x]]$main, moment_match = TRUE),
               m0 = loo(m02_in_null[[x]], moment_match = TRUE))    
  }
  
  return(res)
  
}); names(m02_loos) = names(m02_in_null)

# affect ratings models
# info neglect results
m04_loos = lapply(names(m04_ar), function(x) {
  
    res = list(m1 = loo(m04_ar[[x]], moment_match = TRUE),
               m0 = loo(m04_ar_null[[x]], moment_match = TRUE))    
  
  return(res)
  
}); names(m04_loos) = names(m04_ar)


# Figs
m03_elpd_fig = elpd_comp_fig(elpd_loo_comparison(m03_loos),
                             outcome_name = 'P(vax accept) [Fig. 4b, 5b-c]')

m02_ineg_fig = elpd_comp_fig(elpd_loo_comparison(m02_loos$ineg),
                             outcome_name = 'Info neglect [Fig. 4a]')

m02_apn_se_fig = elpd_comp_fig(elpd_loo_comparison(m02_loos$se_pn),
                               outcome_name = 'APN side effects [Fig. 5a]')

m02_apn_b_fig = elpd_comp_fig(elpd_loo_comparison(m02_loos$b_pn),
                              outcome_name = 'APN benefits [Fig. 5a]')

m04_extreme_se_fig = elpd_comp_fig(elpd_loo_comparison(m04_loos$extreme),
                                   outcome_name = 'AR extreme SE [Fig. 6]')

m04_severe_se_fig = elpd_comp_fig(elpd_loo_comparison(m04_loos$severe),
                                   outcome_name = 'AR severe SE [Fig. 6]')

m04_mild_se_fig = elpd_comp_fig(elpd_loo_comparison(m04_loos$mild),
                                   outcome_name = 'AR mild SE [Fig. 6]')

m04_benefits_fig = elpd_comp_fig(elpd_loo_comparison(m04_loos$positive),
                                   outcome_name = 'AR benefits [Fig. 6]')

# performance fig
fig_stat_perf = wrap_plots(list(m03_elpd_fig, m02_ineg_fig, m02_apn_se_fig, m02_apn_b_fig,
                                m04_extreme_se_fig, m04_severe_se_fig, m04_mild_se_fig, m04_benefits_fig),
                           nrow = 2, ncol = 4)

ggsave('04_online_supplement/02_statistical_modeling/model_evaluation/Fig_elpd_loo_performance.jpg',
       plot = fig_stat_perf,
       scale = 2,
       units = 'cm',
       width = 16,
       height = 8,
       dpi = 700)

# Accuracy ----------------------------------------------------------------

source('01_statistical_modeling/functions/accuracy_loo.R')

m03_main_acc = accuracy_loo(m03_co_in$main_v2)
m03_full_acc = accuracy_loo(m03_co_in$full_v2)

m02_main_acc = lapply(names(m02_in), function(x) accuracy_loo(m02_in[[x]]$main, 
                                                              type = ifelse(x == 'ineg', 'ordinal', 'binary')))
names(m02_main_acc) = names(m02_in)

m02_full_acc = lapply(names(m02_in), function(x) accuracy_loo(m02_in[[x]]$full, 
                                                              type = ifelse(x == 'ineg', 'ordinal', 'binary')))
names(m02_full_acc) = names(m02_full_acc)

m04_acc = lapply(m04_ar, function(x) accuracy_loo(x, type = 'ordinal'))

rm( list = setdiff(ls(), grep('_acc', ls(), value = T)) )
save.image("01_statistical_modeling/balanced_accuracy_loo.RData")