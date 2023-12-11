rm(list = ls())

library(brms)

# for reproducibility
set.seed(12345)

# data --------------------------------------------------------------------

# data prepared for the analyses
load('01_statistical_modeling//data_analyses.RData')

# merge process data with demographics
d = merge(pr_d[, c('sub', 'covid_vax_attitude', 'vax', 'choice', 
                   'ineg', 'se_pn', 'b_pn' , 'se_ext_pn', 'se_sev_pn', 'se_mild_pn'
)],
d[, c('sub', 'covid_vax_attitude', 'vax', 'choice', preds)],
by = c('sub', 'covid_vax_attitude', 'vax', 'choice'))

# remove unused data
rm(ar, pr_d, md, vax_d)

# set levels of info neglect
d$ineg = factor(d$ineg,
                levels = c('full', 'selective', 'no'),
                ordered = T)

# in predictors
in_preds = c('ineg', 'se_pn', 'b_pn' , 'se_ext_pn', 'se_sev_pn', 'se_mild_pn')

# regression predictors
r_preds = c(preds, 'covid_vax_attitude', 'vax')

# set contrasts
for(i in c(in_preds, r_preds)) {
  
  d[,i] = as.factor(d[,i])
  contrasts(d[,i]) = contr.sum(nlevels(d[,i]))
  
}
rm(i)

contrasts(d$ineg) = contrasts(d$ineg) * -1

# main analyses -----------------------------------------------------------

# regression formulas to fit
ff = list(main_v1 = bf( paste0('choice ~ ', 
                               paste(in_preds[1:3], collapse = '+') ,
                               ' + vax * covid_vax_attitude + (1|gr(sub, by = covid_vax_attitude))' )),
          main_v2 = bf( paste0('choice ~ ', 
                               paste(in_preds[c(1,3:6)], collapse = '+') ,
                               ' + vax * covid_vax_attitude + (1|gr(sub, by = covid_vax_attitude))' )),
          full_v1 = bf( paste0('choice ~ ', 
                               paste(c(in_preds[1:3], preds), collapse = '+'),
                               ' + vax * covid_vax_attitude + (1|gr(sub, by = covid_vax_attitude))' )),
          full_v2 = bf( paste0('choice ~ ', 
                               paste(c(in_preds[c(1,3:6)], preds), collapse = '+') ,
                               ' + vax * covid_vax_attitude + (1|gr(sub, by = covid_vax_attitude))' )))


# estimate posteriors
m03_co_in = lapply(ff[4], function(f) {
  
  brm(formula = f,
      family = bernoulli(),
      data = d,
      cores = 4,
      chains = 4,
      thin = 2,
      iter = 4e3,
      prior = set_prior('student_t(3, 0, 2.5)', class = 'b'),
      save_pars = save_pars(all = T)
  )
  
})

rm(list = setdiff(ls(), 'm03_co_in'))
save.image('01_statistical_modeling/03_choice_info_neglect.RData')

# by attitude analyses ----------------------------------------------------

# regression formulas to fit
ff = list(main = bf( paste0('choice ~ ', 
                            paste(in_preds[c(1,3:6)], collapse = '+') ,
                            ' + vax + (1|sub)' )),
          full = bf( paste0('choice ~ ', 
                            paste(c(in_preds[c(1,3:6)], preds), collapse = '+') ,
                            ' + vax + (1|sub)' )))

# attitude vector to loop over
att = c(anti = 'Anti', neu = 'Neutral', pro = 'Pro')

# estimate posteriors separately for each attitude
m03_co_in_sep = lapply(att, function(a) {
  
  mm = lapply(ff, function(f) {
    
    brm(formula = f,
        family = bernoulli(),
        data = d[d$covid_vax_attitude == a, ],
        cores = 4,
        chains = 4,
        thin = 2,
        iter = 4e3,
        prior = set_prior('student_t(3, 0, 2.5)', class = 'b'),
        save_pars = save_pars(all = T))
    
  })
  
})

rm(list = setdiff(ls(), 'm03_co_in_sep'))
save.image('01_statistical_modeling/03_choice_info_neglect_sep.RData')

# specification curve  ----------------------------------------------------

# all possible regressions
in_regs = sapply(1:5, function(x) combn(in_preds[-2], x))

# into bf formulas
ff = lapply(in_regs, function(x) {
  
  f = lapply(1:ncol(x), function(i) {
    
    paste0('choice ~ ', paste(x[,i], collapse = ' + '),
           ' + vax * covid_vax_attitude + covid_vax_no + (1|gr(sub, by = covid_vax_attitude))' )
    
  }); f = unlist(f)
  
}); ff = unlist(ff)
names(ff) = paste0('m', 1:length(ff))

# estimate posteriors
m03_co_in_spec_curve = lapply(ff, function(f) {
  
  brm(formula = f,
      family = bernoulli(),
      data = d,
      cores = 4,
      chains = 4,
      thin = 2,
      iter = 4e3,
      prior = set_prior('student_t(3, 0, 2.5)', class = 'b'),
      save_pars = save_pars(all = T)
  )
  
})

rm(list = setdiff(ls(), 'm03_co_in_spec_curve'))
save.image('01_statistical_modeling/03_choice_info_neglect_spec_curve2.RData')

# univariate effects ------------------------------------------------------

# into bf formulas
ff = lapply(in_preds, function(x) {
  
  vax = bf( paste0('choice ~ ', x,' + vax + (1|sub)' ) )
  vax_att = bf( paste0('choice ~ ', x,' + covid_vax_attitude * vax + (1|gr(sub, by = covid_vax_attitude))' ) )

  return(list(vax = vax, vax_att = vax_att))
  
}); names(ff) = in_preds

# estimate posteriors
m03_co_in_uni = lapply(ff, function(f) {
  
  lapply(f, function(fv) {
    
    brm(formula = fv,
        family = bernoulli(),
        data = d,
        cores = 4,
        chains = 4,
        thin = 2,
        iter = 4e3,
        prior = set_prior('student_t(3, 0, 2.5)', class = 'b'),
        save_pars = save_pars(all = T)
    )
    
  })
  
})

rm(list = setdiff(ls(), 'm03_co_in_uni'))
save.image('01_statistical_modeling/03_choice_info_neglect_uni.RData')

#' # id-lvl efefcts ----------------------------------------------------------
#' 
#' names(in_preds) = in_preds
#' 
#' ff = lapply(in_preds[c(1, 4, 6)], function(ine) {
#'   
#'   bf( paste0('choice ~ ', 
#'              paste(in_preds[-2], collapse = '+') ,
#'              #' + vax * covid_vax_attitude + (1 + ', ine, '|| gr(sub, by = covid_vax_attitude))'
#'               ' + vax * covid_vax_attitude + (1 + ', ine, '||sub)' 
#'              )
#'       )
#' });
#' 
#' # estimate posteriors
#' m03_co_in_id = lapply(ff, function(f) {
#'   
#'   brm(formula = f,
#'       family = bernoulli(),
#'       data = d,
#'       cores = 4,
#'       chains = 4,
#'       thin = 2,
#'       iter = 4e3,
#'       prior = set_prior('student_t(3, 0, 2.5)', class = 'b'),
#'       save_pars = save_pars(all = T)
#'   )
#'   
#' })
#' 
#' rm(list = setdiff(ls(), 'm03_co_in_id'))
#' save.image('analyses/03_choice_info_neglect_id.RData')
#' 
