rm(list = ls())

# this script recodes predictors based on the sample characteristic results
# also cleans a bit

# data --------------------------------------------------------------------

load("01_statistical_modeling/data_list.RData")

# survey recoding ---------------------------------------------------------

# recoding and grouping
dd$survey$covid_vax_no = factor(dd$survey$covid_vax_no,
                                levels = c('0', '1', '2', '3', '4'),
                                labels = c('0', '1\u20132', '1\u20132', '3\u20134', '3\u20134'),
                                ordered = T)

dd$survey$covid_no = factor(dd$survey$covid_no,
                            levels = sort(unique(dd$survey$covid_no)),
                            labels = c('no', 'yes', 'yes'),
                            ordered = T)

dd$survey$race = factor(ifelse(dd$survey$race == 'White', 'White', 'Non\u2013white'))

dd$survey$sex[dd$survey$sex == 'Other'] = ifelse(rbinom(17, 1, .5) == 1, 'Male', 'Female')
dd$survey$sex = factor(dd$survey$sex)

dd$survey$get_covid = factor(dd$survey$get_covid,
                             levels = c('0', '1', '2', '3'),
                             labels = c('not likely', 'not likely', 'likely', 'likely'),
                             ordered = T)

dd$survey$get_sev_covid = factor(dd$survey$get_sev_covid,
                                 levels = c('0', '1', '2', '3'),
                                 labels = c('0', '1\u20133', '1\u20133', '1\u20133'),
                                 ordered = T)

dd$survey$get_death_covid = factor(dd$survey$get_death_covid,
                                   levels = c('0', '1', '2', '3'),
                                   labels = c('0', '1\u20133', '1\u20133', '1\u20133'),
                                   ordered = T)

# choice ------------------------------------------------------------------

dd$ml$choice = ifelse(dd$ml$choice == 'yes', 1, 0)

# covid vax attitude levels ----------------------------------------------

dd = lapply(dd, function(x) {
  
  if('covid_vax_attitude' %in% colnames(x)) {
    
    x[,'covid_vax_attitude'] = factor(x[,'covid_vax_attitude'],
                                      levels = c("Against", "Neutral", "Pro"),
                                      labels = c("Anti", "Neutral", "Pro"),
                                      ordered = T)
    
  }
  
  return(x)
  
})
