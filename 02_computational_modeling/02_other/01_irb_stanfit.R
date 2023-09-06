rm(list = ls(all.names = T))

# function to fit the model
# source('analyses/modeling/functions/00_stan_estimate.R')

library(rstan)
library(loo)
library(bayesplot)

# data --------------------------------------------------------------------

source("02_computational_modeling/00_stan_dat.R")

# for reproducibility
set.seed(12345)

# sampler settings --------------------------------------------------------

# sampler parameters
n_chains = 4
n_iter = 5e3
n_burn = 2e3
n_thin = 2
n_cores = n_chains

# parameters to monitor
pars = c('vax_b', 'rb', 'rb_sigma', 'irb')

# initial values
f_inits = function(chain_id = 1) {
  
  list(
    vax_b = rnorm(4, 0, .1),
    rb = rnorm(1, 0, .1),
    rb_sigma = rnorm(1, 1, .01)
  )
  
}

# compile stan model ------------------------------------------------------

trans = stanc(file = '02_computational_modeling/02_other/01_rb_ct.stan')

compiled = stan_model(stanc_ret = trans, verbose = F)

# estimate  ---------------------------------------------------------------

source('02_computational_modeling/00_functions/stan_diags.R')
source('02_computational_modeling/00_functions/binary_accuracy_loo.R')

# list for storing the results
irb = list()

# loop
for(i in names(dl)) {
  
  # data
  stan_data = dl[[i]]
  
  # sample from posterior
  stanfit = sampling(object = compiled,
                     data = stan_data,
                     pars = c(pars, 'log_lik'),
                     init = f_inits,
                     chains = n_chains,
                     iter = n_iter,
                     warmup = n_burn,
                     thin = n_thin,
                     cores = n_chains,
                     control = NULL)
  
  # model performance -------------------------------------------------------
  
  # approximate loo 
  looE = rstan::loo(stanfit,
                    moment_match = F)
  
  # loo balanced accuracy
  ba = binary_accuracy_loo(stanfit,
                           dl[[i]]$co,
                           binary_cutoff = 'optimal')
  
  # output ------------------------------------------------------------------
  
  # parameters to save
  gp_s = pars[ which( !(pars %in% c('irb'))) ]
  ip_s = pars[ which( pars %in% c('irb')) ]
  
  # posterior samples
  p_pars = extract(stanfit)[c(gp_s)]
  i_pars = extract(stanfit)[c(ip_s)]
  
  # summary table
  fit_summary = summary(stanfit,
                        pars = c(gp_s, 'lp__'))[[1]]
  fit_summary = round(fit_summary, 3)

  # print diagnostic plots
  try(stan_diags(stanfit = stanfit,
                 N = stan_data$N,
                 ind_p = ip_s,
                 group_p = c(gp_s[ -which(gp_s == 'vax_b') ],
                             paste0('vax_b[', 1:4, ']')),
                 pairs_p = c(gp_s[ -which(gp_s == 'vax_b') ],
                             paste0('vax_b[', 1:4, ']')),
                 write_path = paste0('02_computational_modeling/stan_diags/irb_ct_', i)))
  
  # sampling info
  sampling_info = list(
    data = stan_data,
    pars = pars,
    chains = n_chains,
    iter = n_iter,
    warmup = n_burn,
    thin = n_thin,
    control = NULL,
    model = compiled
  )
  
  
  # list with results
  irb[[i]] = list(pars = list(p_pars = p_pars,
                              i_pars = i_pars),
                  fit_summary = fit_summary,
                  looE = looE,
                  balanced_acc = ba,
                  sampling_info = sampling_info)
  
  # clean the env
  rm(stanfit, looE, sampling_info, fit_summary,
     p_pars, i_pars, ip_s, gp_s)
  
  print(i)
  
}

rm(list = setdiff(ls(), 'irb'))
save.image('02_computational_modeling/posteriors/02_irb_ct.RData')