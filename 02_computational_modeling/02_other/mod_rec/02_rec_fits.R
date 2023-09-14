rm(list = ls(all.names = T))

library(rstan)
library(loo)
library(bayesplot)

# data --------------------------------------------------------------------

load("02_computational_modeling/02_other/mod_rec/sim_dat_ct.RData")

# for reproducibility
set.seed(12345)

# sampler settings --------------------------------------------------------

# sampler parameters
n_chains = 4
n_iter = 3e3
n_burn = 1e3
n_thin = 2
n_cores = n_chains

# initial values
f_inits = list(
  
  rb = function(chain_id = 1) {
    
    list(
      vax_b = rnorm(4, 0, .1),
      rb = rnorm(1, 0, .1),
      rb_sigma = rnorm(1, 1, .01)
    )
    
  },
  
  oh = function(chain_id = 1) {
    
    list(
      alpha_exp = rnorm(1, 0, .01),
      beta_phi = rnorm(1, 0, .01),
      phi_phi = rnorm(1, 0, .01),
      vax_b = rnorm(4, 0, .01),
      rb = rnorm(1, 0, .01),
      id_sig = rnorm(2, c(1, .5, .5), .01)
      
    )
    
  },
  
  pt = function(chain_id = 1) {
    
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
  
) 

# compile stan models -----------------------------------------------------

# models for translation
trans = c(paste0('02_computational_modeling/02_other/', c('01_rb_ct.stan','02_ioh_rb_ct.stan')),
          '02_computational_modeling/01_main/01_ispt_irb_ct.stan')

trans = lapply(trans, function(t) stanc(file = t))

compiled = lapply(trans, function(t) stan_model(stanc_ret = t, verbose = F))
names(compiled) = mod_n

# estimate ----------------------------------------------------------------

# function for data subsetting
source('02_computational_modeling/02_other/mod_rec/rand_sub.R')

# array for the results
L = 100
rec_res = array(NA, c(L, 3, 3), 
                dimnames = list(NULL, mod_n, vax_at))

rec_res_l = array(NA, c(L, 3, 3), 
                  dimnames = list(NULL, mod_n, vax_at))

for(l in 1:L) {
  
  # for each attitude
  for(i in vax_at) {
    
    # for each simulated data set
    for(j in mod_n) {
      
      # simulate responses according to the p(vax-accept)
      dl[[i]]$co = rbinom(length(dl[[i]]$co), 1, sim_data[[i]][,j])

      # data
      stan_data = rand_sub(dl[[i]], N = 50) # get data of 50 random subjects
      
      # loo res
      ll = list()
      
      # for each of the models to fit
      for(k in mod_n) {
        
        # sample from posterior
        stanfit = sampling(object = compiled[[k]],
                           data = stan_data,
                           pars = c('log_lik'),
                           init = f_inits[[k]],
                           chains = n_chains,
                           iter = n_iter,
                           warmup = n_burn,
                           thin = n_thin,
                           cores = n_chains,
                           control = NULL)
        
        # approximate elpd_loo 
        ll[[k]] = rstan::loo(stanfit)
        
      }
      
      # model comparison
      llc = loo_compare(ll)
      
      # note the best performing model
      rec_res[l, j, i] = rownames(llc)[1]
      
      # note by how much the best one was better than the second one
      rec_res_l[l, j, i] = llc[2,1]
      
    }
    
  }
  print(l)
}

# output
rm( list = setdiff(ls(), c('rec_res', 'rec_res_l', 'mod_n')) )
save.image('02_computational_modeling/02_other/mod_rec/rec_res.RData')