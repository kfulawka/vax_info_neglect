rm(list = ls())

library(brms)

# for reproducibility
set.seed(12345)

# data --------------------------------------------------------------------

# data prepared for the analyses
load('01_statistical_modeling/data_analyses.RData')

# remove unuesd data
rm(ar, md, pr_d, vax_d)

# total rejection rates ---------------------------------------------------

# aa = aggregate(choice ~ sub + covid_vax_attitude,
#                data = d,
#                FUN = sum)
# 
# table(aa$choice == 0, aa$covid_vax_attitude)
# prop.table(table(aa$choice == 0, aa$covid_vax_attitude), 2)

# modeling ----------------------------------------------------------------

# regression predictors
r_preds = c(preds, 'covid_vax_attitude', 'vax')

# set contrasts
for(i in r_preds) {
  
  contrasts(d[,i]) = contr.sum(nlevels(d[,i]))
  
}
rm(i)

# regression formula to fit
ff = bf( paste0( 'choice ~', paste(r_preds, collapse = '+'), 
                 ' + vax:covid_vax_attitude + (1|gr(sub, by = covid_vax_attitude))' ) )

# estimate the posterior
m01_props = brm(formula = ff,
                data = d,
                family = bernoulli(),
                cores = 4,
                chains = 4,
                thin = 2,
                iter = 4e3,
                prior = c(set_prior('student_t(3, 0, 2.5)', class = 'b')),
                save_pars = save_pars(all = T)
                )


# null model --------------------------------------------------------------

# estimate the posterior
m00_props = brm(formula = choice ~ 1 + (1|gr(sub, by = covid_vax_attitude)),
                data = d,
                family = bernoulli(),
                cores = 4,
                chains = 4,
                thin = 2,
                iter = 4e3,
                save_pars = save_pars(all = T))

rm(list = setdiff(ls(), c('m01_props', 'm00_props')))
save.image("01_statistical_modeling/01_choice_proportions.RData")