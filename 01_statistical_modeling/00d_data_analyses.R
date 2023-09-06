rm(list = ls())

library(future.apply)
library(data.table)

# data --------------------------------------------------------------------

source('01_statistical_modeling/00b_recoding.R')

# demographic predictors of interest
preds = c('education', 'politics', 'income', 
          'covid_vax_no', 'covid_no', 'get_covid', 'age_c'
          # 'sex', 'race'
          )

# mouse lab data
ml_d = dd$ml

# survey data
s_d = dd$survey

# vax parameters (including SE categories)
vax_d = read.csv('00_data/input.csv')
vax_d = vax_d[order(vax_d$vax), ]

# correct ml_d factor levels
ml_d$vax = factor(ml_d$vax,
                  levels = levels(ml_d$vax),
                  labels = vax_d$vax)

# trial level choices and demographics ------------------------------------

# extract choices
choice_d = unique(ml_d[,c('sub', 'vax', 'choice', 'covid_vax_attitude')])

# combine survey data with ml data
d = merge(choice_d, s_d[,c('sub', preds)], by = c('sub'))
rm(choice_d)

# # SPUDM workshop data -----------------------------------------------------
# 
# choices = d[,c('sub', 'vax', 'choice', 'covid_vax_attitude')]
# text_data = dd$oe[,1:3]
# 
# write.table(choices, 'choices.txt', sep ='\t', row.names = F)
# write.table(text_data, 'text_responses.txt', sep ='\t', row.names = F)
# 

# process data ------------------------------------------------------------

source('01_statistical_modeling/functions/search_extract.R')

# function that takes ML data and returns no of acquisitions per event:vax:subject
# creates the index of info neglect
# and returns probability neglects for each probability
plan(multisession)
pr_d = future_lapply(1:max(s_d$sub), function(x) {
  
  #
  s = ml_d[ml_d$sub == x, ]
  
  # extract acquisition counts and neglect data
  m = search_extract(s)
  
  # add classes of probability neglects -------------------------------------
  
  # neglect of an any side effect
  m$se_pn = apply(m[, paste0('P_SE', 1:3, '_pn') ], 1, function(y) any(y == 1, na.rm = T) )
  m$se_pn = as.numeric(m$se_pn)
  
  # neglect of any benefit
  m$b_pn = apply(m[, paste0('P_CRE', 1:3, '_pn') ], 1, function(y) any(y == 1, na.rm = T) )
  m$b_pn = as.numeric(m$b_pn)
  
  # neglects of specific se classes
  m$se_ext_pn = NA; m$se_sev_pn = NA; m$se_mild_pn = NA
  nn = c('se_ext_pn', 'se_sev_pn', 'se_mild_pn')
  
  # for each se category
  for(j in 1:3) {
    
    # for each vax
    for(i in 1:8) {
      
      # check if any of the SE belongs to cat j 
      # & if anyfor any of the SE probability neglect occured
      if( any( (vax_d[i, paste0('SE', 1:3, '_cat')] == j) & m[i, paste0('P_SE', 1:3, '_pn')] ) ) {
        
        m[i, nn[j]] = 1
        
      }
      
    }
    
    # set to 0 if no pn of a given class occured
    m[ is.na(m[,nn[j]]) , nn[j]] = 0
    
  }
  
  # output
  return(m)
  
})
plan(sequential)

# into data frame
pr_d = data.frame( rbindlist(pr_d) )

# affect rating data ------------------------------------------------------

ar = dd$ar

# modeling data -----------------------------------------------------------

# function to combine choice data with ar and pr
source('01_statistical_modeling/functions/modeling_dat.R')

plan(multisession)
md = future_lapply(1:max(s_d$sub), function(x) {
  
  #
  pr_ds = pr_d[pr_d$sub == x, ]
  
  #
  ar_ds = ar[ar$sub == x, ]
  
  # extract acquisition counts and neglect data
  m = modeling_dat(pr_ds, ar_ds, vax_d)
  
  # output
  return(m)
  
})
plan(sequential)

# into data frame
md = data.frame( rbindlist(md) ) 

# # merge with oe data
# load("00_data/oe_dat.RData")
# 
# md = merge(md, oe[,c('sub', 'prob', 'se', 'be', 'all')], by = 'sub')

# output ------------------------------------------------------------------

rm(list = setdiff(ls(), c('d', 'md', 'pr_d', 'ar', 'vax_d', 'preds')))

save.image("01_statistical_modeling/data_analyses.RData")