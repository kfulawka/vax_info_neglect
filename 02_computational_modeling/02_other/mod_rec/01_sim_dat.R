rm(list = ls(all.names = T))

library(rstan)
library(loo)
library(bayesplot)

# data --------------------------------------------------------------------

source("02_computational_modeling/00_stan_dat.R")

vax_at = names(dl)

# modeling results --------------------------------------------------------

load("S:/arc_research/Vaccines/02_computational_modeling/posteriors/01_ispt_vc_irb_ct.RData")
load("S:/arc_research/Vaccines/02_computational_modeling/posteriors/02_irb_ct.RData")
load("S:/arc_research/Vaccines/02_computational_modeling/posteriors/03_ioh_rb_ct.RData")

# for reproducibility
set.seed(12345)

mods = list(rb = irb, oh = ioh, pt = ispt)

mod_n = names(mods)
names(mod_n) = mod_n

# median par estimates for recovery analysis
me_pars = lapply(mods, function(m) {
  
  lapply(m, function(x) {
    
    # ind resp biases (rep 8 times, for each trial)
    ilp = sapply(x$pars$i_pars, colMeans)
    
    # pop-level pars
    plp = lapply(x$pars$p_pars, function(p) {
      
      if (length(dim(p)) == 1) return( mean(p) )
      if (length(dim(p)) == 2) return( colMeans(p) )
      
      
    })
    
    #
    return( list(ilp = ilp, plp = plp) )
    
  })
  
})

# generate data from each model -------------------------------------------

source('02_computational_modeling/00_functions/resp_sim.R')
source('02_computational_modeling/00_functions/spt.R')

sim_data = lapply(vax_at, function(at) {
  
  # data for the at
  d_at = dl[[at]]
  
  n = d_at$n
  
  # for each model generate p(vax_accept)
  r = sapply(mod_n, function(m) {
    
    # get parameters for the specific group-model setup
    ilp = me_pars[[m]][[at]]$ilp
    
    # plp pars
    plp = me_pars[[m]][[at]]$plp
    
    # use the parameters and the data
    
    # PT
    if(m == 'pt') {
      
      co_sim = sapply(1:n, function(i) {
        
        # sub no
        sub = d_at$sub[i]
        
        pa_sim = sim_resp(alpha = plp$alpha, 
                          beta = ilp[sub,'ibeta'], 
                          gamma = ilp[sub,'igam'], 
                          phi = plp$phi,
                          rb = ilp[sub,'irb'], 
                          vax_b = plp$vax_b,
                          se = d_at$se[i,], be = d_at$be[i,], pse = d_at$pse[i,], pbe = d_at$pbe[i,],
                          vax = d_at$vax[i],
                          model = m)
        
        # turn this into simulated choice
        # return( rbinom(1, 1, pa_sim))
        
      })
      
    }
    
    # OH
    if(m == 'oh') {
      
      co_sim = sapply(1:n, function(i) {
        
        # sub no
        sub = d_at$sub[i]
        
        pa_sim = sim_resp(alpha = plp$alpha, 
                          beta = ilp[sub,'ibeta'], 
                          phi = plp$phi,
                          rb = ilp[sub,'irb'],
                          vax_b = plp$vax_b,
                          se = d_at$se[i,], be = d_at$be[i,],
                          vax = d_at$vax[i],
                          model = m)
        
        # turn this into simulated choice
        # return( rbinom(1, 1, pa_sim))
        
      })
      
    }
    
    # RB
    if(m == 'rb') {
      
      co_sim = sapply(1:n, function(i) {
        
        # sub no
        sub = d_at$sub[i]
        
        pa_sim = sim_resp(rb = ilp[sub,'irb'], 
                          vax_b = plp$vax_b, 
                          vax = d_at$vax[i], 
                          model = m)
        
        # turn this into simulated choice
        # return( rbinom(1, 1, pa_sim))
        
      })
      
    }
  
  return(co_sim)
    
  })  
  
  rownames(r) = NULL

  return(r)
  
}); names(sim_data) = vax_at

# output ------------------------------------------------------------------

save.image("02_computational_modeling/02_other/mod_rec/sim_dat_ct.RData")