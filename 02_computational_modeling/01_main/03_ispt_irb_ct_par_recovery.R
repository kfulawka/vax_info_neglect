rm(list = ls(all.names = T))

library(rstan)
library(loo)
library(bayesplot)

# data --------------------------------------------------------------------

source("02_computational_modeling/00_stan_dat.R")

# modeling results --------------------------------------------------------

load("02_computational_modeling/posteriors/01_ispt_vc_irb_ct.RData")

# for reproducibility
set.seed(12345)

# mean par estimates for recovery analysis
me_pars = lapply(ispt, function(x) {
  
  av_fun = mean
  
  # ind resp biases (rep 8 times, for each trial)
  irb = apply(x$pars$i_pars$irb, 2, av_fun)
  ibeta = apply(x$pars$i_pars$ibeta, 2, av_fun)
  igam = apply(x$pars$i_pars$igam, 2, av_fun)
  
  
  # pop-level pars
  plp = lapply(x$pars$p_pars[c('alpha', 'phi', 'vax_b')], function(p) {
    
    if (length(dim(p)) == 1) return( av_fun(p) )
    if (length(dim(p)) == 2) return( apply(p, 2, av_fun) )
    
  })
  
  return( list(irb = irb, ibeta = ibeta, igam = igam,
               alpha = plp$alpha, phi = plp$phi, vax_b = plp$vax_b) )
  
})

# generate data from each model -------------------------------------------

source('02_computational_modeling/00_functions/resp_sim.R')
source('02_computational_modeling/00_functions/spt.R')

vax_at = names(ispt); names(vax_at) = vax_at

sim_data = lapply(vax_at, function(at) {
  
  # data for the at
  d_at = dl[[at]]
  
  # estimated pars
  plp = me_pars[[at]]
  
  co_sim = sapply(1:d_at$n, function(i) { #
    
    # sub no
    sub = d_at$sub[i]
    
    pa_sim = sim_resp(alpha = plp$alpha, 
                      beta = plp$ibeta[sub], 
                      gamma = plp$igam[sub], 
                      phi = plp$phi,
                      rb = plp$irb[sub], 
                      vax_b = plp$vax_b,
                      se = d_at$se[i,], be = d_at$be[i,], pse = d_at$pse[i,], pbe = d_at$pbe[i,],
                      vax = d_at$vax[i],
                      model = 'pt')
    
    # turn this into simulated choice
    return( rbinom(1, 1, pa_sim) )
    
  })

  print( diag(prop.table(table(d_at$co, co_sim), 1)) )
  
  return(co_sim)
  
})

# model fit ---------------------------------------------------------------

# sampler parameters
n_chains = 4
n_iter = 5e3
n_burn = 2e3
n_thin = 2
n_cores = n_chains

# parameters to monitor
pars = c( 'alpha', 'beta', 'gam', 'phi',
          'vax_b',
          'rb', 
          'irb', 
          'ibeta', 'igam',
          'rb_sigma', 
          'beta_sigma', 'gam_sigma',
          'rb_la_rho', 'rb_ps_rho',
          'la_ps_rho'
)

# initial values
f_inits = function(chain_id = 1) {
  
  list(
    alpha_exp = rnorm(1, 0, .01),
    beta_phi = rnorm(1, 0, .01),
    gam_phi = rnorm(1, 0, .01),
    phi_phi = rnorm(1, 0, .01),
    vax_b = rnorm(4, 0, .01),
    rb = rnorm(1, 0, .01),
    id_sig = rnorm(3, c(1, .5, .5), .01)
    
  )
  
}

# stan model
trans = stanc(file = '02_computational_modeling/01_main/01_ispt_irb_ct.stan')
compiled = stan_model(stanc_ret = trans, verbose = F)

# performance (just out of curiosity)
source('02_computational_modeling/00_functions/stan_diags.R')
source('02_computational_modeling/00_functions/binary_accuracy_loo.R')

# list for storing the results
ispt_rec = list()

# loop
for(i in names(dl)) {
  
  # substitute the observed choices with the simulated ones
  dl[[i]]$co = sim_data[[i]]
  
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
  looE = rstan::loo(stanfit,
                    moment_match = F)
  
  # loo balanced accuracy
  ba = binary_accuracy_loo(stanfit,
                           dl[[i]]$co,
                           binary_cutoff = 'optimal')
  
  # output ------------------------------------------------------------------
  
  # parameters to save
  gp_s = pars[ -which(pars %in% c('irb', 'ibeta', 'igam')) ]
  ip_s = pars[ which(pars %in% c('irb', 'ibeta', 'igam')) ]
  
  # posterior samples
  p_pars = extract(stanfit)[c(gp_s)]
  i_pars = extract(stanfit)[c(ip_s)]
  
  # summary table
  fit_summary = summary(stanfit,
                        pars = c(gp_s, 'lp__'))[[1]]
  fit_summary = round(fit_summary, 3)
  
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
  ispt_rec[[i]] = list(pars = list(p_pars = p_pars,
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

rm(list = setdiff(ls(), 'ispt_rec'))
save.image('02_computational_modeling/posteriors/01b_ispt_vc_irb_ct_recovery.RData')