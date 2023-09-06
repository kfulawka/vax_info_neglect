rm(list = ls())

library(brms)

# for reproducibility
set.seed(12345)

# data --------------------------------------------------------------------

# data prepared for the analyses
load('01_statistical_modeling/data_analyses.RData')

# remove unused columns
ar = ar[,-c(4, 6)]

rm(list = setdiff(ls(), 'ar'))

contrasts(ar$covid_vax_attitude) = contr.sum(3)

# remove zero and negative signs from the ratings
ar$ar = as.integer( abs(ar$ar) + 1 )

# group events
events = as.character( sort(unique(ar$event)) )
ev_g = list(extreme = c(4, 5, 9, 10, 12, 13),
            severe = 14:18,
            mild = c(6:8, 11),
            positive = 1:3) 

# create separate data bases
dl = list(
  extreme = ar[ar$event %in% events[ev_g$extreme], ],
  severe = ar[ar$event %in% events[ev_g$severe], ],
  mild = ar[ar$event %in% events[ev_g$mild], ],
  positive = ar[ar$event %in% events[ev_g$positive], ]
)

# modeling ----------------------------------------------------------------

# brms models
# f = bf( ar ~ cs(covid_vax_attitude) + event + (1|gr(sub, by = covid_vax_attitude)) )
f = bf( ar ~ covid_vax_attitude + event + (1|gr(sub, by = covid_vax_attitude)) )

# modeling
m04_ar = lapply(dl, function(d) {
  
  d$event = factor(d$event)
  contrasts(d$event) = contr.sum(nlevels(d$event))
  
  m = brm(formula = f,
          data = d,
          family = acat(),
          cores = 4,
          chains = 4,
          thin = 2,
          iter = 4e3,
          prior = set_prior('student_t(3, 0, 2.5)', class = 'b'),
          save_pars = save_pars(all = T))
  
  return(m)
  
})

rm(list = setdiff(ls(), 'm04_ar'))
save.image("01_statistical_modeling/04_affect_ratings.RData")

# null models -------------------------------------------------------------

# brms models
fn = bf( ar ~ 1 + (1|gr(sub, by = covid_vax_attitude)) )

# modeling
m04_ar_null = lapply(dl, function(d) {
  
  m = brm(formula = fn,
          data = d,
          family = acat(),
          cores = 4,
          chains = 4,
          thin = 2,
          iter = 4e3,
          save_pars = save_pars(all = T))
  
  return(m)
  
})

rm(list = setdiff(ls(), 'm04_ar_null'))
save.image("01_statistical_modeling/04_affect_ratings_null.RData")