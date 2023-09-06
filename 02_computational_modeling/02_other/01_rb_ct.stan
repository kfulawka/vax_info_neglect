data {
  
  // n - no. of data points; N - no. of subjects
  int<lower = 0> n;
  int<lower = 0> N;
  
  // vax-brand effects
  matrix[8, 4] vax_x;
  int<lower = 1> vax[n];
  
  // subject ind vector
  int<lower = 1> sub[n];
  
  // vax-pars data
  matrix[n, 3] se;
  matrix[n, 3] be;
  int<lower = 0, upper = 1> co[n];
  
}

parameters {
  
  // individual response bias
  real rb;
  real<lower = 0> rb_sigma;
  real irb[N];
  
  // vax-brand effects
  vector[4] vax_b;
  
}


transformed parameters {
  
  // container for the linear predictor
  real lin_pred[n];
  
  // for each data point
  for(i in 1:n) {
    
    // linear predictor
    lin_pred[i] = dot_product(vax_b, vax_x[vax[i],]) + irb[sub[i]];
    
  }
  
}

model {
  
  // ind-lvl response bias
  rb ~ std_normal();
  rb_sigma ~ gamma(2, 1);
  irb ~ normal(rb, rb_sigma);
  
  // vax pars
  vax_b ~ std_normal();
  
  // likelihood
  co ~ bernoulli_logit( lin_pred );
  
}

// logliks for loo
generated quantities {
  
  real log_lik[n];
  
  for(i in 1:n) {
    
    log_lik[i] = bernoulli_logit_lpmf( co[i] | lin_pred[i] );
    
  }
  
}
