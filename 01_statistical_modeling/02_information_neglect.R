rm(list = ls())

library(brms)

# for reproducibility
set.seed(12345)

# data --------------------------------------------------------------------

# data prepared for the analyses
load('01_statistical_modeling/data_analyses.RData')

# merge process data with demographics
d = merge(pr_d[, c('sub', 'covid_vax_attitude', 'vax', 'choice', 
                    'ineg', 'se_pn', 'b_pn' #, 'se_ext_pn', 'se_sev_pn', 'se_mild_pn'
                    )],
           d[, c('sub', 'covid_vax_attitude', 'vax', 'choice', preds)],
           by = c('sub', 'covid_vax_attitude', 'vax', 'choice'))

# remove unused data
rm(ar, pr_d, md, vax_d)

# modeling ----------------------------------------------------------------

# regression predictors
r_preds = c(preds, 'covid_vax_attitude', 'vax')

# set contrasts
for(i in r_preds) {
  
  d[,i] = as.factor(d[,i])
  contrasts(d[,i]) = contr.sum(nlevels(d[,i]))
  
}
rm(i)

# dependent variables
d$ineg = factor(d$ineg,
                levels = c('full', 'selective', 'no'),
                ordered = T)

y = c(ineg = 'ineg', se_pn = 'se_pn', b_pn = 'b_pn')

# regression formulas to fit
ff = lapply(y, function(yy) {
  
  # main model
  list(main = bf( paste0(yy, ' ~ vax + covid_vax_attitude + (1|gr(sub, by = covid_vax_attitude))' ),
                  family = ifelse(yy == 'ineg', sratio, bernoulli )),
       full = bf( paste0(yy, ' ~ ', paste(r_preds, collapse = '+'), ' + (1|gr(sub, by = covid_vax_attitude))' ),
                  family = ifelse(yy == 'ineg', sratio, bernoulli ))
       )
  
})

# fit the models
m02_in = lapply(names(ff), function(y) {
  
  if(y == 'ineg') {
    
    priors = set_prior('student_t(3, 0, 2.5)', class = 'b')
    
  } else {
    
    priors = c(set_prior('student_t(3, 0, 2.5)', class = 'b'),
               set_prior('normal(0, .5)', class = 'Intercept'))
    
  }
  
  m = lapply(ff[[y]], function(f) {
    
    brm(formula = f,
        data = d,
        cores = 4,
        chains = 4,
        thin = 2,
        iter = 4e3,
        prior = priors,
        save_pars = save_pars(all = T)
    )
    
  })

  return(m)
  
}); names(m02_in) = names(ff)

# null models -------------------------------------------------------------

# regression formulas to fit
fn = lapply(y, function(yy) {
  
  # null model
  null = bf( paste0(yy, ' ~ 1 + (1|gr(sub, by = covid_vax_attitude))' ),
                  family = ifelse(yy == 'ineg', sratio, bernoulli ))
  
})

# fit the models
m02_in_null = lapply(fn, function(y) {
  
  m = brm(formula = y,
          data = d,
          cores = 4,
          chains = 4,
          thin = 2,
          iter = 4e3,
          save_pars = save_pars(all = T))
  
  return(m)
  
})

rm(list = setdiff(ls(), c('m02_in', 'm02_in_null')))
save.image('01_statistical_modeling/02_information_neglect.RData')