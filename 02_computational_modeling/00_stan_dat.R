rm(list = ls())

# data --------------------------------------------------------------------

# data prepared for the analyses
load("01_statistical_modeling/data_analyses.RData")

# data for modeling
rm(list = setdiff(ls(), c('md', 'pr_d', 'vax_d')))

# main variables recoding -------------------------------------------------

vax_n = sort(unique(md$vax))
md$vax = as.numeric(as.factor(md$vax))
md$covid_vax_attitude = as.numeric(as.factor(md$covid_vax_attitude))


# # individually reported info usage ----------------------------------------
# 
# # add PS groups
# md$PS = factor(md$prob, 
#                levels = c(0, 1, 99),
#                labels = c(2, 1, 3))
# md$PS = as.numeric(as.character(md$PS))
# 
# # add LA groups
# md$LA = ifelse(md$be == 0 & md$se == 1 & md$prob == 0, 1, 
#                ifelse(md$se == 99, 3, 2))

# data list for STAN ------------------------------------------------------

# order again
md = md[order(md$sub, md$vax), ]

#

# list with data
dl = list(anti = list(),
          neu = list(),
          pro = list())

for(i in 1:3) {
  
  mda = md[md$covid_vax_attitude == i, ]
  
  # observed data
  dl[[i]][['se']] = mda[, paste0('SE', 1:3)] 
  dl[[i]][['be']] = mda[, paste0('CRE', 1:3)]
  dl[[i]][['pse']] = mda[, paste0('P_SE', 1:3)] 
  dl[[i]][['pbe']] = mda[, paste0('P_CRE', 1:3)]
  dl[[i]][['co']] = mda$choice
  dl[[i]][['vax']] = mda$vax
  
  # subjects
  dl[[i]][['sub']] = as.numeric(as.factor(mda$sub))
  
  # # individual differences in search strategies
  # dl[[i]][['LA']] = mda[ rep( c(T, rep(F, 7)), times = nrow(mda)/8 ), 'LA']
  # dl[[i]][['PS']] = mda[ rep( c(T, rep(F, 7)), times = nrow(mda)/8 ), 'PS']

  #model matrix for vax coefs
  dl[[i]][['vax_x']] = cbind(c(-1, -1, 1, 0, -1, 1, 1, 0),
                             c(-1, -1, 0, 1, -1, 0, 0, 1),
                             c(1, -1, 0, 1, 1, 0, -1, -1),
                             c(0, -1, 1, 0, 0, 1, -1, -1))
  
  # number of participants
  dl[[i]][['N']] = max(dl[[i]][['sub']])
  
  # number of data points
  dl[[i]][['n']] = length(dl[[i]][['co']])
  
}

rm( list = setdiff(ls(), c('dl', 'md', 'vax_n')) )