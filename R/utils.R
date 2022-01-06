# utility functions
options(warn = -1)
SEED = 12345 # random seed
BASE_SIZE = 15
ANNOTATION_SIZE = 5
infusion_animals = c(2152, 2153, 2154, 2155, 2156, 2160, 2165, 2166)
PPC_sb_animals = c(2153, 2154, 2156, 2160)
FOF_sb_animals = c(2153, 2154, 2155, 2156, 2160, 2165, 2166)
side_animals = c(2154, 2155, 2156, 2160, 2165, 2166)

subj_colors = c("#E16A86", "#C7821C", "#909800", "#00A846", "#00AD9A", "#00A2D3", "#9183E6", "#D766C9")
FOF_dosage_colors = c('azure4', 'rosybrown1', 'purple4') # control, 0.075, 0.3
PPC_dosage_colors = c('azure4', 'gold2') # control, 0.3
side_colors = c('control' = 'azure4', 'L' = 'mediumseagreen', 'R' = 'brown2') # control, Left, Right
prev_outcome_colors = c('coral3', 'deepskyblue', 'greesssssssssn4') # lottery_lose, surebet, lotter_win
dataset_colors = list('Control' = 'azure4','Bilateral-FOF' = 'purple3', 'Bilateral-PPC' = 'gold2') # control, Bilateral FOF, Bilateral PPC
sb_change_colors = c('gold2', 'deepskyblue')
full_sb_change_colors = c('azure4', 'deepskyblue', 'gold2', 'chartreuse3') # control, sb_change, just infusion, infusion+change

subj_shapes = c(3, 4, 5, 7, 8, 9, 15, 16)
dataset_shapes = c(3, 4, 5)

params = c('rho', 'sigma', 'omega_rational', 'omega_lottery', 'omega_surebet')
latex_params = c('rho' ='$\\rho$', 'sigma' = '$\\sigma$',
                 'omega_rational' = '$\\omega_{rational}$',
                 'omega_lottery' = '$\\omega_{lottery}$',
                 'omega_surebet' = '$\\omega_{surebet}$')
latex_delta_params = c('rho' ='$\\Delta \\rho$', 'sigma' = '$\\Delta \\sigma$',
                       'omega_rational' = '$\\Delta \\omega_{rational}$',
                       'omega_lottery' = '$\\Delta \\omega_{lottery}$',
                       'omega_surebet' = '$\\Delta \\omega_{surebet}$')

regions_list = list('bi_fof' = 'Bilateral FOF', 'bi_ppc' = 'Bilateral PPC',
                    'uni_fof' = 'Unilateral FOF', 'uni_ppc' = 'Unilateral PPC')

# models take time to compile and run, so we just include the fit data instead. You can check out model stan files in ...
# compile model
#individual_model = stan_model(file = 'R/individual.stan')
#individual_pred_model = stan_model(file = 'R/individual_pred.stan')
#inactivation_model = stan_model(file = 'R/inactivation.stan')
#inactivation_pred_model = stan_model(file = 'R/inactivation_pred.stan')
# inactivation_rho_model = stan_model(file = 'inactivation_rho.stan')
# inactivation_omega_model = stan_model(file = 'inactivation_omega.stan')
# inactivation_cv_model = stan_model(file = 'inactivation_cv.stan')
# inactivation_rho_cv_model = stan_model(file = 'inactivation_rho_cv.stan')
# inactivation_omega_cv_model = stan_model(file = 'inactivation_omega_cv.stan')


#' scale_save
#'
#' ggplot wrapper to save the plot
#'
#' @param p: a ggplot object to be saved
#' @param name: the name of the file
#' @param width: the width of the plot
#' @param height: the height of the plot
#' @param scale: scale of the plot, 1 is the default
#'
#' @return saves the plot in the specified path
scale_save = function(p, name, width = 4, height = 4, scale = 1){
  # ggsave wrapper
  p = p + theme(text = element_text(size = BASE_SIZE),
                axis.text = element_text(size = BASE_SIZE),
                legend.text = element_text(size = BASE_SIZE))
  fname = sprintf('plots/%s.pdf', name)
  ggsave(filename = fname, device = "pdf", width = width, height = height, scale = scale, unit = 'cm')
}


#' bino
#'
#' get binomial mean and confidence intervals
#'
#' @param x: a vector of 0s and 1s
#'
#' @return a dataframe with y, ymin and ymax
bino <-function(x){
  # get binomial mean and confidence intervals
  out = binom.test(sum(x), length(x))
  df = data.frame(y = mean(x), ymin=out$conf.int[1], ymax=out$conf.int[2])
  return(df)
}

#' plot_risk
#'
#' plots binned risk choices with specified subjid
#'
#' @param df: dataframe containing risky trials
#' @param subjid: subjid
#' @param by_prev_outcome: Boolean, if true it plots separately for lottery-win, lottery-lose and sure-bet as previous outcome
#'
#' @return ggplot object
plot_risk = function(df, subjid = 0, by_prev_outcome = FALSE){
  # plots binned risk choices with specified subjid
  if (subjid == 0){
    subj_df = df
  } else{
    subj_df = df[df$subjid == subjid,]
  }
  n_trials = dim(subj_df)[1]
  p = ggplot(subj_df) + theme_classic() +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
    geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) +
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(breaks = c(round(min(subj_df$delta_ev)/10)*10, 0, round(max(subj_df$delta_ev)/10)*10)) +
    annotate("text", label = paste0('n=', n_trials), x = max(subj_df$delta_ev)*0.85, y = 0.1) +
    annotate("text", label = unique(subj_df$subjid), fontface = 2, x = max(subj_df$delta_ev)*0.8, y = 0.2) +
    xlab(expression(Delta~EV)) + ylab("P(Chose Lottery)")
  if (!by_prev_outcome){
    p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice), fun.data = bino, geom = 'pointrange')
  } else {
    p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice, color = prev_outcome_s), fun.data = bino, geom = 'pointrange')
  }
  return(p)
}

#' binomialize
#'
#' format df into binomial version for model fitting
#'
#' @param df: dataframe containing risky trials
#'
#' @return dataframe
binomialize = function(df){
  # format df into binomial version for model fitting
  bino_df = df %>% group_by(lottery_mag, lottery_prob, sb_mag, total_rew_multi) %>%
    add_tally() %>% summarise(n_trials = mean(n), n_chose_lott = sum(choice)) %>%
    mutate(delta_ev = lottery_mag*lottery_prob*total_rew_multi - sb_mag*total_rew_multi) %>%
    ungroup()
  return(bino_df)
}

#' se
#'
#' standard error
#'
#' @param x: a vector of values
#'
#' @return numeric
se = function(x){
  # standard error
  return(sd(x) / sqrt(length(x)))
}

#' rho_sigma_agent
#'
#' returns choice_prob (probability choosing lottery) or choice (binary) from the simulated three-agent rho-sigma agent
#'
#' @param prob: Boolean, if true returns choice_prob
#' @param params: a list as in list('rho' = 1, 'sigma' = 0.4, 'omega' = c(0.8, 0.1, 0.1))
#' @param sb_mag: a vector of n_trials with surebet magnitude
#' @param lottery_mag: a vector of n_trials with lottery magnitude
#' @param lottery_prob: a vector of n_trials with lottery probability
#' @param total_rew_multi: a vector of n_trials with total reward multiplier
#'
#' @return a vector of n_trials with choice_prob or choice
rho_sigma_agent = function(prob = FALSE, params, sb_mag, lottery_mag, lottery_prob, total_rew_multi){
  # returns choice_prob from the simulated three-agent rho-sigma agent
  rho = params$rho # rho < 1 is risk-averse, rho = 1 is risk-neutral, rho > 1 is risk-seeking
  sigma = params$sigma # noise associated with the internal representation of utility
  omega = params$omega # vector of size 3, baseline fractions of being rational, lottery and surebet agent
  n_trials = length(sb_mag)
  choice_prob = vector(length = n_trials)
  for (tx in 1:n_trials){
    u_l = lottery_prob[tx]*(total_rew_multi[tx]*lottery_mag[tx])^rho
    u_sb = (total_rew_multi[tx]*sb_mag[tx])^rho
    p_rational = 1 - pnorm(0, u_l - u_sb, sqrt(2)*sigma)
    P = c(p_rational, 1, 0)
    choice_prob[tx] = omega %*% P
  }
  choice = rbinom(n_trials, 1, choice_prob)
  if (prob){
    return(choice_prob)
  } else{
    return(choice)
  }
}

logit2prob = function(logit){
  odds = exp(logit)
  prob = odds / (1 + odds)
  return(prob)
}

#' get_freq_task_param
#'
#' find the most frequent task parameter given the df
#'
#' @param df: dataframe with risky trials
#' @param param: task parameter name, 'sb_mag', 'lottery_mag', 'lottery_prob' and 'total_rew_multi'
#'
#' @return numeric
get_freq_task_param = function(df, param = 'sb_mag'){
  # find the most frequent task parameter given the df
  return(as.numeric(names(sort(table(df[param]), decreasing = TRUE)[1])))
}

#' gen_syn_smooth
#'
#' generate smooth synthetic data points for every task parameter combinations
#'
#' @param df: dataframe with risky trials
#' @param sb_mags: a list with sb_mags to include, if NULL it will get the most frequent sb_mag
#' @param extend: extend by 'lottery_mag' or 'delta_ev'
#'
#' @return dataframe
gen_syn_smooth = function(df, sb_mags = NULL, extend = 'delta_ev'){
  # generate smooth synthetic data points for every task parameter combinations
  if (is.null(sb_mags)){
    sb_mags = get_freq_task_param(df, 'sb_mag')
  }
  if (extend == 'delta_ev'){
    pred_df = df %>% group_by(subjid) %>%
      data_grid(delta_ev = seq_range(c(min(delta_ev), max(delta_ev)*1.1), by = 0.5),
                species = 2, individual = 1, prev_outcome,
                lottery_prob = get_freq_task_param(df, 'lottery_prob'),
                sb_mag = sb_mags,
                total_rew_multi = get_freq_task_param(df, 'total_rew_multi'),
                n_trials = 200) %>%
      mutate(lottery_mag = (delta_ev + sb_mag*total_rew_multi) / (lottery_prob*total_rew_multi))
  } else if (extend == 'lottery_mag'){
    pred_df = df %>% group_by(subjid) %>%
      data_grid(lottery_mag = seq_range(lottery_mag, by = 0.5),
                species = 2, individual = 1, prev_outcome = 0, # if extend based on lottery_mag, we assume history is not included
                lottery_prob = get_freq_task_param(df, 'lottery_prob'),
                sb_mag = sb_mags,
                total_rew_multi = get_freq_task_param(df, 'total_rew_multi'),
                n_trials = 200) %>%
      mutate(delta_ev = lottery_mag*lottery_prob*total_rew_multi - sb_mag*total_rew_multi)
  }
  pred_df = pred_df %>% mutate(prev_outcome_s = case_when(prev_outcome == -1 ~ 'lottery_lose',
                                                          prev_outcome == 1 ~ 'lottery_win',
                                                          prev_outcome == 0 ~ 'surebet')) %>%
    filter(lottery_mag >= 0) %>% ungroup()
  colnames(pred_df) = paste0('pred_', colnames(pred_df))
  pred_df$pred_prev_outcome_s = factor(pred_df$pred_prev_outcome_s, levels = c('lottery_lose', 'surebet', 'lottery_win'))
  return(pred_df)
}

#' compute_ipsi_bias
#'
#' computes the ipsilateral side bias
#' helper function for figure_learning
#'
#' @param choice: a vector with 0s and 1s
#' @param side: 'left', 'right'
#'
#' @return dataframe
compute_ipsi_bias = function(choice, side){
  # computes the ipsilateral side bias
  # helper function for figure_learning
  side = ifelse(side == 'R', 'right', 'left')
  oppo_side = ifelse(side == 'right', 'left', 'right')
  bias = (sum(choice == side) - sum(choice == oppo_side)) / length(choice)
  return(bias)
}

#' fit_animals
#'
#' fit all animal data with the individual model and save fit details in csv/
#'
#' @param dataset: 'control', 'bi_fof', 'bi_ppc
#'
#' @return dataframe
fit_animals = function(dataset = 'control'){
  # fit all animal data with the individual model and save fit details in csv/
  if (dataset == 'control'){
    df = read.csv('csv/figure_behavior_population.csv')
  } else if (dataset == 'bi_fof'){
    df = read.csv('csv/figure_infusion_bi_fof.csv') %>%
      filter(dosage == 0.3) %>% filter(as.Date(trialtime) < '2020-11-19') # experiment 1
  } else if (dataset == 'bi_ppc'){
    df = read.csv('csv/figure_infusion_bi_ppc.csv') %>%
      filter(dosage == 0.3) %>% filter(as.Date(trialtime) < '2020-11-19') # experiment 1
  }
  rho_pop = data.frame()
  sigma_pop = data.frame()
  omega_pop = data.frame()
  for (i in 1:length(infusion_animals)){
    animal = infusion_animals[i]
    if (animal == 2155 & dataset == 'Bilateral-PPC'){
      rho_pop = cbind(rho_pop, data.frame(rho = rep(NA, 4000)))
      sigma_pop = cbind(sigma_pop, data.frame(sigma = rep(NA, 4000)))
      omega_pop = cbind(omega_pop, data.frame(V1 = rep(NA, 4000), V2 = rep(NA, 4000), V3 = rep(NA, 4000)))
      lp_pop = cbind(lp_pop, data.frame(lp__ = rep(NA, 4000)))
    } else{
      subj_df = df %>% filter(subjid == animal)
      subj_df = subj_df %>% filter(sb_mag == get_freq_task_param(subj_df, 'sb_mag'))
      subj_df = sample_meta_rat(subj_df, n = 300)
      bino_df = binomialize(subj_df)
      data = as.list(bino_df)
      data$T = dim(bino_df)[1]
      # fit individual model
      fit = sampling(individual_model, data = data, seed = 194838, refresh = 0)
      check_hmc_diagnostics(fit)
      save(fit, file = sprintf('fits/%d.RData', animal))
      # append it to the population distribution
      if (i == 1){
        rho_pop = as.data.frame(rstan::extract(fit)$rho)
        sigma_pop = as.data.frame(rstan::extract(fit)$sigma)
        omega_pop = as.data.frame(rstan::extract(fit)$omega)
        lp_pop = as.data.frame(rstan::extract(fit)$lp__)
      } else{
        rho_pop = cbind(rho_pop, as.data.frame(rstan::extract(fit)$rho))
        sigma_pop = cbind(sigma_pop, as.data.frame(rstan::extract(fit)$sigma))
        omega_pop = cbind(omega_pop, as.data.frame(rstan::extract(fit)$omega))
        lp_pop = cbind(lp_pop, as.data.frame(rstan::extract(fit)$lp__))
      }
    }
  }
  N = length(infusion_animals)
  colnames(rho_pop) = paste0('rho', seq(1, N))
  colnames(sigma_pop) = paste0('sigma', seq(1, N))
  omega_rational_pop = omega_pop[,seq(1, N*3, 3)]
  omega_lottery_pop = omega_pop[,seq(1, N*3, 3)+1]
  omega_surebet_pop = omega_pop[,seq(1, N*3, 3)+2]
  colnames(lp_pop) = paste0('lp__', seq(1, N))
  colnames(omega_rational_pop) = paste0('omega_rational', seq(1, N))
  colnames(omega_lottery_pop) = paste0('omega_lottery', seq(1, N))
  colnames(omega_surebet_pop) = paste0('omega_surebet', seq(1, N))
  df = cbind(rho_pop, sigma_pop, omega_rational_pop, omega_lottery_pop, omega_surebet_pop, lp_pop)
  df$dataset = dataset
  write.csv(df, sprintf('csv/all_%s_fits.csv', dataset))
  return(df)
}

#' fit_animals_inactivation
#'
#' fit all animal data with the inactivation model and save in csv/
#'
#' @param fit_model: compiled stan model corresponding to the inactivation model used
#' @param model: 'rho-and-omega', 'rho-only', 'omega-only'
#'
#' @return dataframe
fit_animals_inactivation = function(fit_model, model = 'rho-only'){
  # fit all animal data with the inactivation model and save in csv/
  control_df = read.csv('csv/figure_behavior_population.csv') %>% mutate(which_trial = 1)
  fof_df = read.csv('csv/figure_infusion_bi_fof.csv') %>%
    filter(dosage == 0.3) %>% filter(as.Date(trialtime) < '2020-11-19') %>% mutate(which_trial = 2) # experiment 1
  ppc_df = read.csv('csv/figure_infusion_bi_ppc.csv') %>%
    filter(dosage == 0.3) %>% filter(as.Date(trialtime) < '2020-11-19') %>% mutate(which_trial = 3) # experiment 1
  df = rbind(control_df, fof_df, ppc_df)
  for (i in 1:length(infusion_animals)){
    animal = infusion_animals[i]
    subj_df = df %>% filter(subjid == animal)
    #subj_df = subj_df %>% filter(sb_mag == get_freq_task_param(subj_df, 'sb_mag'))
    #subj_df = subj_df %>% group_by(which_trial) %>% sample_n(200, replace = TRUE) %>% ungroup()
    bino_df = subj_df %>% group_by(which_trial, lottery_mag, lottery_prob, sb_mag, total_rew_multi) %>%
      add_tally() %>% summarise(n_trials = mean(n), n_chose_lott = sum(choice)) %>%
      mutate(delta_ev = lottery_mag*lottery_prob*total_rew_multi - sb_mag*total_rew_multi) %>%
      ungroup()
    data = as.list(bino_df)
    data$T = dim(bino_df)[1]
    fit = sampling(fit_model, data = data, seed = 194838, refresh = 0)
    check_hmc_diagnostics(fit)
    # extract the parameters
    draws = rstan::extract(fit)
    rho = exp(draws$rho_base)
    sigma = draws$sigma_base
    omega_rational = 1 / (1 + exp(- (draws$omega_1_base)))
    omega_2 = 1 / (1 + exp(- (draws$omega_2_base)))
    omega_lottery = (rep(1, 4000) - omega_rational)*omega_2
    omega_surebet = (rep(1, 4000) - omega_rational)*(1 - omega_2)

    lp = draws$lp__
    rho_fof = exp(draws$rho_base + draws$rho_delta_fof)
    sigma_fof = draws$sigma_base*draws$sigma_fof_scale
    omega_rational_fof = 1 / (1 + exp(- (draws$omega_1_base + draws$omega_1_delta_fof)))
    omega_2_fof = 1 / (1 + exp(- (draws$omega_2_base + draws$omega_2_delta_fof)))
    omega_lottery_fof = (rep(1, 4000) - omega_rational_fof)*omega_2_fof
    omega_surebet_fof = (rep(1, 4000) - omega_rational_fof)*(1 - omega_2_fof)

    rho_ppc = exp(draws$rho_base + draws$rho_delta_ppc)
    sigma_ppc = draws$sigma_base*draws$sigma_ppc_scale
    omega_rational_ppc = 1 / (1 + exp(- (draws$omega_1_base + draws$omega_1_delta_ppc)))
    omega_2_ppc = 1 / (1 + exp(- (draws$omega_2_base + draws$omega_2_delta_ppc)))
    omega_lottery_ppc = (rep(1, 4000) - omega_rational_ppc)*omega_2_ppc
    omega_surebet_ppc = (rep(1, 4000) - omega_rational_ppc)*(1 - omega_2_ppc)

    # append it to the population distribution
    if (i == 1){
      lp_pop = lp
      rho_pop = rho
      sigma_pop = sigma
      omega_rational_pop = omega_rational
      omega_lottery_pop = omega_lottery
      omega_surebet_pop = omega_surebet
      lp_pop = as.data.frame(rstan::extract(fit)$lp__)
      rho_fof_pop = rho_fof
      sigma_fof_pop = sigma_fof
      rho_delta_fof_pop = draws$rho_delta_fof
      omega_1_delta_fof_pop = draws$omega_1_delta_fof
      omega_2_delta_fof_pop = draws$omega_2_delta_fof
      omega_rational_fof_pop = omega_rational_fof
      omega_lottery_fof_pop = omega_lottery_fof
      omega_surebet_fof_pop = omega_surebet_fof
      rho_ppc_pop = rho_ppc
      sigma_ppc_pop = sigma_ppc
      rho_delta_ppc_pop = draws$rho_delta_ppc
      omega_1_delta_ppc_pop = draws$omega_1_delta_ppc
      omega_2_delta_ppc_pop = draws$omega_2_delta_ppc
      omega_rational_ppc_pop = omega_rational_ppc
      omega_lottery_ppc_pop = omega_lottery_ppc
      omega_surebet_ppc_pop = omega_surebet_ppc
    } else{
      lp_pop = cbind(lp_pop, lp)
      rho_pop = cbind(rho_pop, rho)
      sigma_pop = cbind(sigma_pop, sigma)
      omega_rational_pop = cbind(omega_rational_pop, omega_rational)
      omega_lottery_pop = cbind(omega_lottery_pop, omega_lottery)
      omega_surebet_pop = cbind(omega_surebet_pop, omega_surebet)
      lp_pop = cbind(lp_pop, as.data.frame(rstan::extract(fit)$lp__))

      rho_fof_pop = cbind(rho_fof_pop, rho_fof)
      sigma_fof_pop = cbind(sigma_fof_pop, sigma_fof)
      rho_delta_fof_pop = cbind(rho_delta_fof_pop, draws$rho_delta_fof)
      omega_1_delta_fof_pop = cbind(omega_1_delta_fof_pop, draws$omega_1_delta_fof)
      omega_2_delta_fof_pop = cbind(omega_2_delta_fof_pop, draws$omega_2_delta_fof)
      omega_rational_fof_pop = cbind(omega_rational_fof_pop, omega_rational_fof)
      omega_lottery_fof_pop = cbind(omega_lottery_fof_pop, omega_lottery_fof)
      omega_surebet_fof_pop = cbind(omega_surebet_fof_pop, omega_surebet_fof)

      rho_ppc_pop = cbind(rho_ppc_pop, rho_ppc)
      sigma_ppc_pop = cbind(sigma_ppc_pop, sigma_ppc)
      rho_delta_ppc_pop = cbind(rho_delta_ppc_pop, draws$rho_delta_ppc)
      omega_1_delta_ppc_pop = cbind(omega_1_delta_ppc_pop, draws$omega_1_delta_ppc)
      omega_2_delta_ppc_pop = cbind(omega_2_delta_ppc_pop, draws$omega_2_delta_ppc)
      omega_rational_ppc_pop = cbind(omega_rational_ppc_pop, omega_rational_ppc)
      omega_lottery_ppc_pop = cbind(omega_lottery_ppc_pop, omega_lottery_ppc)
      omega_surebet_ppc_pop = cbind(omega_surebet_ppc_pop, omega_surebet_ppc)
    }
  }
  N = length(infusion_animals)
  colnames(lp_pop) = paste0('lp__', seq(1, N))
  colnames(rho_pop) = paste0('rho', seq(1, N))
  colnames(sigma_pop) = paste0('sigma', seq(1, N))
  colnames(omega_rational_pop) = paste0('omega_rational', seq(1, N))
  colnames(omega_lottery_pop) = paste0('omega_lottery', seq(1, N))
  colnames(omega_surebet_pop) = paste0('omega_surebet', seq(1, N))

  control_df = as.data.frame(cbind(rho_pop, sigma_pop, omega_rational_pop, omega_lottery_pop, omega_surebet_pop)) %>% mutate(dataset = 'Control')
  colnames(rho_fof_pop) = paste0('rho', seq(1, N))
  colnames(sigma_fof_pop) = paste0('sigma', seq(1, N))
  colnames(omega_rational_fof_pop) = paste0('omega_rational', seq(1, N))
  colnames(omega_lottery_fof_pop) = paste0('omega_lottery', seq(1, N))
  colnames(omega_surebet_fof_pop) = paste0('omega_surebet', seq(1, N))
  fof_df = as.data.frame(cbind(rho_fof_pop, sigma_fof_pop, omega_rational_fof_pop, omega_lottery_fof_pop, omega_surebet_fof_pop)) %>% mutate(dataset = 'Bilateral-FOF')

  colnames(rho_ppc_pop) = paste0('rho', seq(1, N))
  colnames(sigma_ppc_pop) = paste0('sigma', seq(1, N))
  colnames(omega_rational_ppc_pop) = paste0('omega_rational', seq(1, N))
  colnames(omega_lottery_ppc_pop) = paste0('omega_lottery', seq(1, N))
  colnames(omega_surebet_ppc_pop) = paste0('omega_surebet', seq(1, N))
  ppc_df = as.data.frame(cbind(rho_ppc_pop, sigma_ppc_pop, omega_rational_ppc_pop, omega_lottery_ppc_pop, omega_surebet_ppc_pop)) %>% mutate(dataset = 'Bilateral-PPC')
  colnames(ppc_df) = c(paste0('rho', seq(1, N)), paste0('sigma', seq(1, N)), paste0('omega_rational', seq(1, N)), paste0('omega_lottery', seq(1, N)), paste0('omega_surebet', seq(1, N)), 'dataset')

  df = rbind(control_df, fof_df, ppc_df)
  df = df %>% cbind(rbind(lp_pop, lp_pop, lp_pop))
  write.csv(df, sprintf('csv/all_%s_fits.csv', model))
  # save delta parameters separately
  colnames(rho_delta_fof_pop) = paste0('rho_delta_fof', seq(1, N))
  colnames(omega_1_delta_fof_pop) = paste0('omega_1_delta_fof', seq(1, N))
  colnames(omega_2_delta_fof_pop) = paste0('omega_2_delta_fof', seq(1, N))
  colnames(rho_delta_ppc_pop) = paste0('rho_delta_ppc', seq(1, N))
  colnames(omega_1_delta_ppc_pop) = paste0('omega_1_delta_ppc', seq(1, N))
  colnames(omega_2_delta_ppc_pop) = paste0('omega_2_delta_ppc', seq(1, N))
  df2 = as.data.frame(cbind(rho_delta_fof_pop, omega_1_delta_fof_pop, omega_2_delta_fof_pop, rho_delta_ppc_pop, omega_1_delta_ppc_pop, omega_2_delta_ppc_pop))
  write.csv(df2, sprintf('csv/%s_delta_fits.csv', model))
  return(df2)
}

#' computeELPD
#'
#' adapted from https://github.com/stan-dev/stancon_talks/blob/master/2017/Contributed-Talks/07_nicenboim/kfold.Rmd
#' the ELPD is the theoretical expected log pointwise predictive density for the held-out data
#' the loglik from the fit is a n_iter x T matrix, the pointwise is 1 x T vector, elpd is the sum of pointwise
#'
#' @param log_lik_heldout:
#'
#' @return dataframe
computeELPD = function(log_lik_heldout){
  # adapted from https://github.com/stan-dev/stancon_talks/blob/master/2017/Contributed-Talks/07_nicenboim/kfold.Rmd
  # the ELPD is the theoretical expected log pointwise predictive density for the held-out data
  # the loglik from the fit is a n_iter x T matrix, the pointwise is 1 x T vector, elpd is the sum of pointwise
  library(matrixStats)
  logColMeansExp = function(x) {
    S = nrow(x)
    colLogSumExps(x) - log(S)
  }
  pointwise = matrix(logColMeansExp(log_lik_heldout), ncol = 1)
  colnames(pointwise) = "elpd"
  elpd = sum(pointwise)
  se_elpd = sqrt(ncol(log_lik_heldout) * var(pointwise))
  out = list('pointwise' = pointwise,
             'elpd' = elpd,
             'se_elpd' = se_elpd)
  return(structure(out, class = 'S4'))
}

#' kfold
#'
#' the generic function for manual K-fold cross validation
#'
#' @param K_FOLD: the number of folds
#' @param fit_model: a compiled stan model object
#' @param model: 'rho-only', 'omega-only', 'rho-and-omega'
#'
#' @return dataframe of ELPD
kfold = function(K_FOLD = 10, fit_model, model = 'rho-only'){
  # the generic function for manual K-fold cross validation
  control_df = read.csv('csv/figure_behavior_population.csv') %>% mutate(which_trial = 1)
  fof_df = read.csv('csv/figure_infusion_bi_fof.csv') %>%
    filter(dosage == 0.3) %>% filter(as.Date(trialtime) < '2020-11-19') %>% mutate(which_trial = 2) # experiment 1
  ppc_df = read.csv('csv/figure_infusion_bi_ppc.csv') %>%
    filter(dosage == 0.3) %>% filter(as.Date(trialtime) < '2020-11-19') %>% mutate(which_trial = 3) # experiment 1
  df = rbind(control_df, fof_df, ppc_df) %>% group_by(subjid)

  subj_list = unique(df$subjid)
  # first find the portion size for each subject and each inavtivation trial type
  portion_df = df %>% group_by(subjid, which_trial) %>% tally() %>% rename(total = n) %>%
    mutate(portion = round(total/K_FOLD), test_begin = 1, test_end = test_begin + portion)
  df = merge(df, portion_df, by = c('subjid', 'which_trial'))
  # initialize elpd container for this model's kfold
  elpd_df = data.frame()
  for (subj in subj_list){
    for (k in 1:K_FOLD){
      df$holdout = 0 # 1 means to be heldout, 0 means to be used for fitting
      df = df %>% group_by(subjid, which_trial) %>% mutate(group_inx = row_number()) %>%
        mutate(holdout = case_when(group_inx >= test_begin & group_inx <= test_end ~ 1,
                                   group_inx < test_begin ~ 0,
                                   group_inx > test_end ~ 0))
      # update test_begin and test_end number for the next fold
      portion_df = portion_df %>% group_by(subjid, which_trial) %>% mutate(test_begin = test_end)
      if (k == K_FOLD - 1){
        portion_df = portion_df %>% group_by(subjid, which_trial) %>% mutate(test_end = total)
      } else {
        portion_df = portion_df %>% group_by(subjid, which_trial) %>% mutate(test_end = test_end + portion)
      }
      # format it into binomial version
      all_df = df %>% filter(subjid == subj) %>% group_by(holdout, which_trial, lottery_mag, lottery_prob, sb_mag, total_rew_multi) %>%
        add_tally() %>% summarise(n_trials = mean(n), n_chose_lott = sum(choice))
      train_df = all_df[all_df$holdout == 0, ]
      test_df = all_df[all_df$holdout == 1, ]
      # put data into a list
      train_data = as.list(train_df)
      colnames(test_df) = paste0('test_', colnames(test_df))
      test_data = as.list(test_df)
      data = append(train_data, test_data)
      data$T = dim(train_df)[1]
      data$D = dim(test_df)[1]
      # fit the model
      cat(sprintf('Fitting %d/%d fold on %d using %s...\n', k, K_FOLD, subj, model))
      fit = sampling(fit_model, data = data, seed = SEED, refresh = 0, init = 'random')
      check_hmc_diagnostics(fit)
      # expand binomial elpd to bernoulli elpd by the number of trials
      ll_draws = as.data.frame(rstan::extract(fit, pars = 'log_lik'))
      for (i in 1:dim(ll_draws)[2]){
        temp = as.data.frame(matrix(rep(ll_draws[,i], test_df$test_n_trials[i]), ncol=test_df$test_n_trials[i]))
        if (i == 1){
          k_bernoulli_ll_df = temp # n_iter x n_trials log_likelihood for this test set
        } else{
          k_bernoulli_ll_df = cbind(k_bernoulli_ll_df, temp)
        }
      }
      if (k == 1){
        bernoulli_ll_df = k_bernoulli_ll_df
      } else{
        bernoulli_ll_df = cbind(bernoulli_ll_df, k_bernoulli_ll_df)
      }
    }
    # compute ELPD on log predictive posterior density aka. log likelihood on the test data
    elpd = computeELPD(as.matrix(bernoulli_ll_df))
    elpd_df = rbind(elpd_df, data.frame(subjid = subj, elpd = elpd$elpd, elpd_se = elpd$se_elpd))
  }
  write.csv(elpd_df, sprintf("csv/%s_kfold.csv", model))
  return(elpd_df)
}

is_upper = function(i, total = 25){
  # helper function for aggregated pairs; calculates if i is in the upper diagnal of the square matrix
  n = sqrt(total)
  row_num = ceiling(i / n)
  diagnal = seq(1, total, n+1)
  row_ends = seq(n, total+1, by = n)
  row_ends[length(row_ends)] = row_ends[length(row_ends)] - 1
  is_upper = (i > diagnal[row_num] & i <= row_ends[row_num])
  return(is_upper)
}

aggregated_pairs = function(df, multiple_datasets = FALSE, control_ticks = TRUE){
  # aggregated pairs plot using individual fits
  # multiple_datasets: when true, it makes the pairs plot colored by dataset
  # control_ticks: when true, x and y ticks labels are specified
  if (!multiple_datasets){
    stat_df = as.data.frame(colMeans(df))
    namez = rownames(stat_df)
    stat_df = stat_df %>% mutate(param = str_extract(namez, '[a-z_]+'),
                                 individual = str_extract(namez, '[0-9]+')) %>%
      rename(mean = 'colMeans(df)') %>%
      mutate(ymin = apply(df, 2, quantile, 0.1),
             ymax = apply(df, 2, quantile, 0.9)) %>%
      pivot_wider(names_from = param, values_from=c('mean', 'ymin', 'ymax'))
  } else{
    datasets = c('Control', 'Bilateral-FOF', 'Bilateral-PPC')
    stat_df = data.frame()
    for (ds in datasets){
      this_df = df %>% filter(dataset == ds) %>% select(!dataset)
      this_stat_df = as.data.frame(colMeans(this_df))
      namez = rownames(this_stat_df)
      this_stat_df = this_stat_df %>% mutate(param = str_extract(namez, '[a-z_]+'),
                                             individual = str_extract(namez, '[0-9]+')) %>%
        rename(mean = 'colMeans(this_df)') %>%
        mutate(ymin = apply(this_df, 2, quantile, 0.025),
               ymax = apply(this_df, 2, quantile, 0.975)) %>%
        pivot_wider(names_from = param, values_from=c('mean', 'ymin', 'ymax')) %>%
        mutate(dataset = ds)
      stat_df = rbind(stat_df, this_stat_df)
    }
    stat_df$dataset = factor(stat_df$dataset, datasets)
  }
  param_combos = as.data.frame(expand.grid(params, params))
  diagnal = seq(1, dim(param_combos)[1], length(params)+1)
  # define parameter-specific limits and breaks
  lower_limits = list('rho' = 0, 'sigma' = 0, 'omega_rational' = 0, 'omega_lottery' = 0, 'omega_surebet' = 0)
  upper_limits = list('rho' = 1, 'sigma' = 8, 'omega_rational' = 1, 'omega_lottery' = 1, 'omega_surebet' = 1)
  lower_breaks = list('rho' = 0, 'sigma' = 0, 'omega_rational' = 0, 'omega_lottery' = 0, 'omega_surebet' = 0)
  middle_breaks = list('rho' = 0.5, 'sigma' = 4, 'omega_rational' = 0.5, 'omega_lottery' = 0.5, 'omega_surebet' = 0.5)
  upper_breaks = list('rho' = 1, 'sigma' = 8, 'omega_rational' = 1, 'omega_lottery' = 1, 'omega_surebet' = 1)
  # make the 'pairs' plot
  for (i in 1:dim(param_combos)[1]){
    raw_x = as.character(param_combos[i,1])
    raw_y = as.character(param_combos[i,2])
    if (is_upper(i, nrow(param_combos))){
      P = P + plot_spacer()
    } else{
      if (i %in% diagnal){ # population posterior
        if (!multiple_datasets){ # concat all individual samples to form a joint distribution
          pop_param_df = as.data.frame(as_vector(df %>% select(contains(raw_x))))
          colnames(pop_param_df) = 'pop_param'
          p = ggplot(pop_param_df, aes(x = pop_param)) + geom_density(alpha = 0.5, fill = 'azure4')
        } else{ # concat all individual samples, group by dataset
          pop_param_df = df %>% select(contains(raw_x) | contains('dataset')) %>% pivot_longer(!dataset)
          p = ggplot(pop_param_df, aes(x = value, fill = dataset)) +
            geom_density(alpha = 0.5) +
            scale_fill_manual(values = dataset_colors)
        }
        p = p + theme_classic(base_size = BASE_SIZE) +
          geom_vline(xintercept = apply(pop_param_df, 2, median), color = 'black', size = 1) +
          xlab(TeX(latex_params[raw_x])) + ylab(TeX(latex_params[raw_y])) +
          theme(legend.position = 'none', axis.text.x = element_text(angle = 90, hjust = 5, size = 12))
      } else{ # scatter plot with individual parameter means
        xmin = paste0('ymin_', raw_x)
        xmax = paste0('ymax_', raw_x)
        ymin = paste0('ymin_', raw_y)
        ymax = paste0('ymax_', raw_y)
        x = paste0('mean_', raw_x)
        y = paste0('mean_', raw_y)
        if (!multiple_datasets){
          p = ggplot(stat_df, aes_string(x = x, y = y, ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, color = 'individual')) +
            scale_color_manual(values = subj_colors) +
            theme_classic(base_size = BASE_SIZE) +
            geom_pointrange(size = 0.1) + geom_errorbarh(size = 0.2) +
            xlab(TeX(latex_params[raw_x])) + ylab(TeX(latex_params[raw_y])) +
            theme(legend.position = 'none', axis.text.x = element_text(angle = 90, hjust = 5))
        } else{
          p = ggplot(stat_df, aes_string(x = x, y = y, color = 'dataset', shape = 'individual')) +
            geom_point(size = 1) +
            #geom_polygon(mapping = aes_string(x = x, y = y, group = 'individual', color = 'individual'), size = 0.2, alpha = 0.2, color = 'black', fill = 'white') +
            scale_color_manual(values = dataset_colors) + scale_fill_manual(values = dataset_colors) +
            scale_shape_manual(values = subj_shapes) +
            theme_classic(base_size = BASE_SIZE) +
            xlab(TeX(latex_params[raw_x])) + ylab(TeX(latex_params[raw_y])) +
            theme(legend.position = 'none', axis.text.x = element_text(angle = 90, hjust = 5))
        }
      }
      # control ticks label
      if (control_ticks){
        if (!(i %in% diagnal)){
          p = p + scale_y_continuous(limits = c(lower_limits[[raw_y]], upper_limits[[raw_y]]),
                                     breaks = c(lower_breaks[[raw_y]], middle_breaks[[raw_y]], upper_breaks[[raw_y]])) +
            scale_x_continuous(limits = c(lower_limits[[raw_x]], upper_limits[[raw_x]]),
                               breaks = c(lower_breaks[[raw_x]], middle_breaks[[raw_x]], upper_breaks[[raw_x]]))
        } else if (i %in% diagnal){
          p = p + scale_x_continuous(limits = c(lower_limits[[raw_x]], upper_limits[[raw_x]]),
                                     breaks = c(lower_breaks[[raw_x]], middle_breaks[[raw_x]], upper_breaks[[raw_x]]))
        }
      }
      # control x and y ticks
      if (i %% length(params) != 1){ # if not in the first column
        p = p + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
      }
      if (i <= length(params)^2 - length(params)){ # if not in the last row
        p = p + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
      }
      if (i %in% c(2,3,4,5,8,9,10,11,12,14,15,16,17,18)){
        p = p + theme(axis.text.x = element_blank())
      }
      if (i %in% c(3,4,5,8,9,10,12,14,15,17,18,20,22,23,24)){
        p = p + theme(axis.text.y = element_blank())
      }
      if (i %in% diagnal){
        p = p + theme(axis.text.x = element_text())
      }
      if (i == 1){
        P = p
      } else{
        P = P + p
      }
    }
  }
  P = P + plot_layout(ncol = length(params), nrow = length(params))
  return(P)
}

preprocessRisk = function(df, remove_forced = TRUE, remove_viol = TRUE, remove_slow = TRUE){
  # preprocess raw risk data to be fitting-friendly
  # df may contain single or multiple subjects, however the region must be the same
  df$individual = df %>% group_indices(subjid)
  df = df %>%
    arrange(subjid, sessid, trialid) %>%
    mutate(species_name = case_when(subjid > 1000 & subjid < 2000 ~ 'mouse',
                                    subjid > 2000 ~ 'rat'),
           species = ifelse(species_name == 'mouse', 1, 2)) %>%
    mutate(forced = is.na(lottery_poke) | is.na(surebet_poke), # forced trials
           forced_surebet = is.na(lottery_poke),
           forced_lottery = is.na(surebet_poke)) %>%
    mutate(is_side_choice = is.na(lottery_poke) & is.na(surebet_poke)) %>%  # side choice trials
    group_by(sessid) %>% # find session-specific total_rew_multiplier
    mutate(total_rew_multi = case_when(subj_choice == 'violation' ~ 0,
                                       subj_choice == 'lottery' ~ 0,
                                       subj_choice == 'surebet' ~ reward_received / sb_mag)) %>%
    mutate(total_rew_multi = sort(unique(total_rew_multi))[2]) %>% ungroup() %>%
    mutate(delta_ev = (total_rew_multi * lottery_mag * lottery_prob) - (total_rew_multi * sb_mag)) %>%
    mutate(prev_reward = lag(reward)) %>%  # add previous reward
    mutate(choice = case_when(subj_choice == 'lottery' ~ 1,
                              subj_choice == 'surebet' ~ 0,
                              subj_choice == 'violation' ~ 9)) %>%
    mutate(prev_choice = lag(choice)) %>% # add previous choice
    mutate(prev_outcome_s = case_when(prev_reward > 0 & prev_choice == 1 ~ 'lottery_win',
                                      prev_reward == 0 & prev_choice == 1 ~ 'lottery_lose',
                                      prev_choice == 0 ~ 'surebet',
                                      prev_choice == 9 ~ 'surebet')) %>% # trials preceded by a violation
    mutate(prev_outcome = case_when(prev_outcome_s == 'lottery_win' ~ 1,
                                    prev_outcome_s == 'lottery_lose' ~ -1,
                                    prev_outcome_s == 'surebet' ~ 0)) %>%
    filter(trialnum != 1) %>% # remove the first trials
    filter(!is_side_choice) %>% # remove side choice trials
    filter(!is.na(total_rew_multi)) %>%
    filter(!is.na(prev_outcome_s)) # fix me!
  df$prev_outcome_s = factor(df$prev_outcome_s, levels = c('lottery_lose', 'surebet', 'lottery_win'))
  #filter(!is.na(prev_outcome))
  if (remove_forced){
    df = df %>% filter(forced == 0) # remove forced trials
  }
  if (remove_viol){
    df = df %>% filter(viol == 0) # remove violation trials
  }
  if (remove_slow){
    df = df %>% filter(RT < 5) # remove slow trials, assuming no attention is paid to this trial
  }
  df = df %>% group_by(subjid) %>% mutate(log_RT = log(RT/mean(RT, na.rm = TRUE))) # normalize RT within subjects
  #df = df %>% mutate(log2_RT = log2(RT/mean(RT))) # normalize RT across subjects
  # some unilateral-specific preprocessing
  #if (any(unique(df$infusion_side) %in% c('L','R'))){
  df = df %>% group_by(subjid) %>%
    mutate(lottery_side = unique(lottery_poke)[!is.na(unique(lottery_poke))]) %>%
    ungroup() %>%
    mutate(pro_right_delta_ev = case_when(lottery_side == 'BotL' ~ -delta_ev,
                                          lottery_side == 'BotR' ~ delta_ev),
           choose_right = case_when(lottery_side == 'BotL' ~ 1 - choice,
                                    lottery_side == 'BotR' ~ choice))
  return(df)
}
