// rho-sigma-mixture model for FOF and PPC inactivation data 
functions {
  real choiceprob(real rho, real sigma, real lottery_mag, real lottery_prob, real sb_mag, real rew_multi) {
    real y;
    real u1;	// Lottery utility
    real u2;	// Surebet utility
    
    u1 = lottery_prob * (rew_multi * lottery_mag) ^ rho;
    u2 = (rew_multi * sb_mag) ^ rho;
    y = 1 - normal_cdf(0, u1 - u2, sqrt(2)*sigma);
    return y;
  }
}
data {
  int<lower=0> T; // Number of trials we have
  vector[T] lottery_mag; // Lottery value for that choice
  vector[T] lottery_prob; // Lottery probabilities for that choice
  vector[T] sb_mag; // Surebet values for that choice
  vector[T] total_rew_multi; // total reward multiplier = base_reward * rew_multi
  vector[T] which_trial; // control = 1, FOF = 2,  PPC = 3
  int n_chose_lott[T]; // number chose lottery for this trial type
  int n_trials[T]; // total number of trials for this trial type
  // below are synthetic data for out-of-sample prediction
  int<lower=0> P; // number of trial types
  vector[P] pred_lottery_mag; // Lottery value for that choice
  vector[P] pred_lottery_prob; // Lottery probabilities for that choice
  vector[P] pred_sb_mag; // Surebet values for that choice
  vector[P] pred_which_trial; // control = 1, FOF = 2,  PPC = 3
  vector[P] pred_total_rew_multi; // total reward multiplier = base_reward * rew_multi
  int pred_n_trials[P]; // total number of trials for this trial type
}
parameters {
  real rho_base; // risk preference
  real rho_delta_fof; // deviation caused by FOF inactivation
  real rho_delta_ppc; // deviation caused by PPC inactivation
  
  real sigma_base; // internal noise
  real<lower=0> sigma_fof_scale; // scale of noise by FOF inactivation
  real<lower=0> sigma_ppc_scale; // scale of noise by PPC inactivation
  
  real omega_1_base;
  real omega_1_delta_fof;
  real omega_1_delta_ppc;
  
  real omega_2_base;
  real omega_2_delta_fof;
  real omega_2_delta_ppc;
}
model {
  real p_rational; 
  
  real rho;
  real sigma;
  real omega_1;
  real omega_2;

  // draw from priors
  rho_base ~ normal(log(0.9), 0.4);
  rho_delta_fof ~ normal(0, 1);
  rho_delta_ppc ~ normal(0, 1);
    
  sigma_base ~ gamma(6, 3);
  sigma_fof_scale ~ lognormal(0, 0.1);
  sigma_ppc_scale ~ lognormal(0, 0.1);
  
  omega_1_base ~ normal(0, 2);
  omega_1_delta_fof ~ normal(0, 2);
  omega_1_delta_ppc ~ normal(0, 2);
  
  omega_2_base ~ normal(0, 2);
  omega_2_delta_fof ~ normal(0, 2);
  omega_2_delta_ppc ~ normal(0, 2);
  
  for(t in 1:T){
    if (which_trial[t] == 2){ // FOF
      rho = exp(rho_base + rho_delta_fof);
      omega_1 = inv_logit(omega_1_base + omega_1_delta_fof);
      omega_2 = inv_logit(omega_2_base + omega_2_delta_fof);
      sigma = sigma_base * sigma_fof_scale;
    } else if (which_trial[t] == 3){ // PPC
      rho = exp(rho_base + rho_delta_ppc);
      omega_1 = inv_logit(omega_1_base + omega_1_delta_ppc);
      omega_2 = inv_logit(omega_2_base + omega_2_delta_ppc);
      sigma = sigma_base * sigma_ppc_scale;
    } else { // control
      rho = exp(rho_base);
      omega_1 = inv_logit(omega_1_base);
      omega_2 = inv_logit(omega_2_base);
      sigma = sigma_base;
    }
    p_rational = choiceprob(rho, sigma, lottery_mag[t], lottery_prob[t], sb_mag[t], total_rew_multi[t]);
    n_chose_lott[t] ~ binomial(n_trials[t], omega_1*p_rational + (1 - omega_1)*omega_2);
  }
}

generated quantities{
  vector[P] pred_n_chose_lott;
  real p_rational; 
  
  real rho;
  real sigma;
  real omega_1;
  real omega_2;
  
  for(p in 1:P){
    if (pred_which_trial[p] == 2){ // FOF
      rho = exp(rho_base + rho_delta_fof);
      omega_1 = inv_logit(omega_1_base + omega_1_delta_fof);
      omega_2 = inv_logit(omega_2_base + omega_2_delta_fof);
      sigma = sigma_base * sigma_fof_scale;
    } else if (pred_which_trial[p] == 3){ // PPC
      rho = exp(rho_base + rho_delta_ppc);
      omega_1 = inv_logit(omega_1_base + omega_1_delta_ppc);
      omega_2 = inv_logit(omega_2_base + omega_2_delta_ppc);
      sigma = sigma_base * sigma_ppc_scale;
    } else { // control
      rho = exp(rho_base);
      omega_1 = inv_logit(omega_1_base);
      omega_2 = inv_logit(omega_2_base);
      sigma = sigma_base;
    }
    p_rational = choiceprob(rho, sigma, pred_lottery_mag[p], pred_lottery_prob[p], pred_sb_mag[p], pred_total_rew_multi[p]);
    pred_n_chose_lott[p]  = binomial_rng(pred_n_trials[p], omega_1*p_rational + (1 - omega_1)*omega_2);
  }
}
