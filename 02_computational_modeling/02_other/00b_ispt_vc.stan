functions {
  
  real pwf(real p, real gam) {
    
    real wp;
    
    if (p > 0) { wp = exp(-pow(-log(p), gam)); } else { wp = .5; }
    
    return wp;
  }
  
}


data {
  
  // n - no. of data points; N - no. of subjects
  int<lower = 0> n;
  int<lower = 0> N;
  
  // subject ind vector
  int<lower = 1> sub[n];

  // vax-pars data
  matrix[n, 3] pse;
  matrix[n, 3] pbe;
  
  matrix[n, 3] se;
  matrix[n, 3] be;
  int<lower = 0, upper = 1> co[n];
  
}

parameters {
  
  // SPT pop-lvl parameters
  real alpha_exp;
  real beta_phi; // this should be analyzed on 0-1 range
  real gam_phi; // this should be analyzed on 0-1 range
  real phi_phi;

  // individual LA and RB z-scale displacements
  matrix[4, N] id_z;
  
  // ind-dist variances
  vector<lower = 0>[4] id_sig;
  
  // rb-la corr matrix
  cholesky_factor_corr[4] L_omega;
  
}


transformed parameters {
  
  // spt sv cpmutpation
  matrix[n, 3] w_pse;
  matrix[n, 3] v_se;
  matrix[n, 3] w_pbe;
  matrix[n, 3] v_be;
  matrix[n, 2] sv;

  // container for the linear predictor
  real lin_pred[n];

  // individual level pars on probit scale
  matrix[4, N] id_phi;
  
  // id-lvl model parameters on model scale
  vector[N] ialpha;
  vector[N] ibeta;
  vector[N] igam;
  vector[N] iphi;
  
  // transform individual displacements with var-cov matrix
  id_phi = diag_pre_multiply(id_sig, L_omega) * id_z;
  
  // id-lvl parameters on the model scale
  for(s in 1:N) {
    
    ialpha[s] = exp(alpha_exp + id_phi[1, s]);
    ibeta[s] = Phi_approx(beta_phi + id_phi[2, s]);
    igam[s] = Phi_approx(gam_phi + id_phi[3, s]);
    iphi[s] = Phi_approx(phi_phi + id_phi[4, s]);

    
  }
  
  // for each data point
  for(i in 1:n) {
    
    // for each outcome branch
    for(j in 1:3) {
      
      // side effects
      v_se[i,j] =  pow( fabs(se[i,j]), ialpha[sub[i]]);
      w_pse[i,j] = pwf( pse[i,j], igam[sub[i]] );

      
      // benefits
      v_be[i,j] = pow(be[i,j], ialpha[sub[i]]);
      w_pbe[i,j] = pwf( pbe[i,j], igam[sub[i]] );

    }
    
    // subjective vax value
    // 1: side effects branch; 2: benefits branch
    sv[i,1] = ibeta[sub[i]] * dot_product(v_se[i,], w_pse[i,]);
    sv[i,2] = (1 - ibeta[sub[i]]) * dot_product(v_be[i,], w_pbe[i,]);

    // linear predictor
    lin_pred[i] = iphi[sub[i]] * (sv[i,2] - sv[i,1]);
    
  }
  
}

model {
  
    // PT parameters
    alpha_exp ~ normal(0, .5);
    beta_phi ~ std_normal();
    gam_phi ~ std_normal();
    phi_phi ~ std_normal();

    // ind-lvl z-scale displacemenets
    to_vector(id_z) ~ std_normal();
    
    id_sig[1] ~ normal(.5, .13); // id alpha var
    id_sig[2] ~ normal(.5, .13); // id beta var
    id_sig[3] ~ normal(.5, .13); // id gam var
    id_sig[4] ~ normal(.5, .13); // id phi var
    
    L_omega ~ lkj_corr_cholesky(5); // prior for the corr matrix
    
    // likelihood
    co ~ bernoulli_logit( lin_pred );
  
}

// 
  generated quantities {
    
    // alpha to monitor
    real <lower = 0> alpha;
    real <lower = 0> alpha_sigma;
    
    // LA to monitor
    real <lower = 0, upper = 1> beta;
    real <lower = 0> beta_sigma;
    
    // PS to monitor
    real <lower = 0, upper = 1> gam;
    real <lower = 0> gam_sigma;
    
    // PS to monitor
    real <lower = 0, upper = 1> phi;
    real <lower = 0> phi_sigma;
    
    // correlations
    matrix[4,4] omega;
    real alp_la_rho;
    real alp_ps_rho;
    real alp_phi_rho;
    real la_ps_rho;
    real la_phi_rho;
    real ps_phi_rho;
    
    //logliks for loo
    real log_lik[n];
    
    // parameters to monitor
    alpha  = exp(alpha_exp);
    alpha_sigma = id_sig[1];
    
    beta  = Phi_approx(beta_phi);
    beta_sigma = id_sig[2];
    
    gam = Phi_approx(gam_phi);
    gam_sigma = id_sig[3];
    
    phi = Phi_approx(phi_phi);
    phi_sigma = id_sig[4];
    
    // correlation
    omega = L_omega * L_omega';
    alp_la_rho = omega[1,2];
    alp_ps_rho = omega[1,3];
    alp_phi_rho = omega[1,4];
    la_ps_rho = omega[2,3];
    la_phi_rho = omega[2,4];
    ps_phi_rho = omega[3,4];
  
  
    // logliks
    for(i in 1:n) {
      
      log_lik[i] = bernoulli_logit_lpmf( co[i] | lin_pred[i] );
      
    }

}