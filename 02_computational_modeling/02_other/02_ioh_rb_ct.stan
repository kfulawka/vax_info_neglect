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
  
  // OH pop-lvl parameters
  real alpha_exp; // this should be analyzed on R+ range
  real beta_phi; // this should be analyzed on 0-1 range
  real phi_phi; // this should be analyzed on 0-1 range
  
  // RB pop-lvl
  real rb;
  
  // individual LA and RB z-scale displacements
  matrix[2, N] id_z;
  
  // ind-dist variances
  vector<lower = 0>[2] id_sig;
  
  // rb-la corr matrix
  cholesky_factor_corr[2] L_omega;
  
  // vax-brand effects
  vector[4] vax_b;

}


transformed parameters {
  
  // oh sv cpmutpation
  matrix[n, 3] v_se;
  matrix[n, 3] v_be;
  real sv[n];
  
  // container for the linear predictor
  real lin_pred[n];
  
  // individual level pars on probit scale
  matrix[2, N] id_phi;
  
  // id-lvl model parameters on model scale
  vector[N] irb;
  vector[N] ibeta;

  // pop level paramters on model scape
  real<lower = 0> alpha = exp(alpha_exp);
  real<lower = 0, upper = 1> phi = Phi_approx(phi_phi);
  
  // transform individual displacements with var-cov matrix
  id_phi = diag_pre_multiply(id_sig, L_omega) * id_z;

  // id-lvl parameters on the model scale
  for(s in 1:N) {
    
    irb[s] = rb + id_phi[1, s];
    ibeta[s] = Phi_approx(beta_phi + id_phi[2, s]);

  }

  // for each data point
  for(i in 1:n) {
    
    for(j in 1:3) {
      
      // side effects
      v_se[i,j] = -pow( fabs(se[i,j]), alpha );

      // benefits
      v_be[i,j] = pow( be[i,j], alpha );

    }

    // subjective vax value
    sv[i] = ibeta[sub[i]] * sum(v_se[i,]) + (1 - ibeta[sub[i]]) * sum(v_be[i,]);
    
    // linear predictor
    lin_pred[i] = dot_product(vax_b, vax_x[vax[i],]) + irb[sub[i]] + phi *  sv[i];
    
  }
  
}

model {
  
  // OH parameters
  alpha_exp ~ normal(0, .5);
  beta_phi ~ std_normal();
  phi_phi ~ std_normal();
  
  // ind-lvl response bias
  rb ~ std_normal();
  
  // ind-lvl z-scale displacemenets
  to_vector(id_z) ~ std_normal();
  
  id_sig[1] ~ gamma(2, 1); // id vars
  id_sig[2] ~ normal(.5, .13); // id beta vars

  L_omega ~ lkj_corr_cholesky(3); // prior for the corr matrix

  // vax pars
  vax_b ~ std_normal();
  
  // likelihood
  co ~ bernoulli_logit( lin_pred );
  
}

// logliks for loo
generated quantities {
  
  // LA to monitor
  real <lower = 0, upper = 1> beta;
  real <lower = 0> beta_sigma;
  
  real <lower = 0> rb_sigma;
  
  // LA-RB correlation
  matrix[2,2] omega;
  real rb_la_rho;
  
  //logliks for loo
  real log_lik[n];
  
  // parameters to monitor
  rb_sigma = id_sig[1];

  beta = Phi_approx(beta_phi);
  beta_sigma = id_sig[2];

  // LA-RB correlation
  omega = L_omega * L_omega';
  rb_la_rho = omega[1,2];  

  for(i in 1:n) {
  
    log_lik[i] = bernoulli_logit_lpmf( co[i] | lin_pred[i] );
  
  }

}
