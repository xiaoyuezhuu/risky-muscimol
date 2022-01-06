// simple model fitting the individual as rho-sigma agent with trial history outside of softmax
// using dirichlet
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
  int n_chose_lott[T]; // number chose lottery for this trial type
  int n_trials[T]; // total number of trials for this trial type
}
parameters {
  real rho_raw; // risk preference
  real<lower=0> sigma; // internal noise
  simplex[3] omega; // baseline fractions of being rational, lottery and surebet agent
}
transformed parameters{
  real<lower=0> rho;
  rho = exp(rho_raw);
}
model {
  vector[3] agent; // initialize an agent vector 
  real p_rational; 
  real p_chose_lott;
  vector[3] omega_alphas = [6, 2, 2]';
  
  rho_raw ~ normal(log(0.9), 0.4);
  sigma ~ gamma(6, 3);
  omega ~ dirichlet(omega_alphas);
  
  for(t in 1:T){
    p_rational = choiceprob(rho, sigma, lottery_mag[t], lottery_prob[t], sb_mag[t], total_rew_multi[t]);
    agent[1] = p_rational; // rational agent
    agent[2] = 1; // lottery agent
    agent[3] = 0; // surebet agent
    p_chose_lott = dot_product(agent, omega);
    n_chose_lott[t] ~ binomial(n_trials[t], p_chose_lott);
  }
}
