rm(list = ls(all.names = T))

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
pars = c( 'alpha', 'beta', 'gam', 'phi',
          'ialpha', 'ibeta', 'igam', 'iphi',
          'alpha_sigma', 'beta_sigma', 'gam_sigma', 'phi_sigma'
          # 'alp_la_rho', 'alp_ps_rho', 'alp_phi_rho',
          # 'la_ps_rho', 'la_phi_rho', 'ps_phi_rho'
          )

# initial values
f_inits = function(chain_id = 1) {
  
  list(
    alpha_exp = rnorm(1, 0, .01),
    beta_phi = rnorm(1, 0, .01),
    gam_phi = rnorm(1, 0, .01),
    phi_phi = rnorm(1, 0, .01),
    id_sig = rnorm(4, .5, .01)
    
  )
  
}

# compile stan model ------------------------------------------------------

trans = stanc(file = '02_computational_modeling/02_other/00a_ispt.stan')
compiled = stan_model(stanc_ret = trans, verbose = F)

# estimate  ---------------------------------------------------------------

source('02_computational_modeling/00_functions/00_stan_diags.R')
source('02_computational_modeling/00_functions/binary_accuracy_loo.R')

# list for storing the results
ispt_reg = list()

# loop
for(i in names(dl)) {
  
  # sample from posterior
  stanfit = sampling(object = compiled,
                     data = dl[[i]],
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
  looE = loo(stanfit)
  
  # loo balanced accuracy
  ba = binary_accuracy_loo(stanfit,
                           dl[[i]]$co,
                           binary_cutoff = 'optimal')
  
  # output ------------------------------------------------------------------
  
  # parameters to save
  gp_s = pars[ -which(pars %in% c('ialpha', 'ibeta', 'igam', 'iphi')) ]
  ip_s = pars[ which(pars %in% c('ialpha', 'ibeta', 'igam', 'iphi')) ]
  
  # posterior samples
  p_pars = extract(stanfit)[c(gp_s)]
  i_pars = extract(stanfit)[c(ip_s)]
  
  # summary table
  fit_summary = summary(stanfit,
                        pars = c(gp_s, 'lp__'))[[1]]
  fit_summary = round(fit_summary, 3)
  
  gp_diag = gp_s
  
  try(stan_diags(stanfit = stanfit,
                 N = dl[[i]]$N,
                 ind_p = ip_s,
                 group_p = gp_diag,
                 pairs_p = list(p1 = c(gp_diag[grepl('beta|gam|alpha|phi|lp__', gp_diag)])
                                # p2 = c(gp_diag[grepl('rho|sigma', gp_diag)])
                 ),
                 write_path = paste0('02_computational_modeling/stan_diags/ispt_', i))
  )
  
  # sampling info
  sampling_info = list(
    data = dl[[i]],
    pars = pars,
    chains = n_chains,
    iter = n_iter,
    warmup = n_burn,
    thin = n_thin,
    control = NULL,
    model = compiled
  )
  
  
  # list with results
  ispt_reg[[i]] = list(pars = list(p_pars = p_pars,
                               i_pars = i_pars),
                   fit_summary = fit_summary,
                   looE = looE,
                   balanced_acc = ba,
                   sampling_info = sampling_info)
  
  # clean the env
  rm(stanfit, looE, sampling_info, fit_summary, ba,
     p_pars, i_pars, ip_s, gp_s)
  
  print(i)
  
}

rm(list = setdiff(ls(), 'ispt_reg'))
save.image('02_computational_modeling/posteriors/00_ispt_regular.RData')