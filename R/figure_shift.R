# functions for figure5 (figure_shift)

#' figure_shift_individual_deviation
#'
#' figure5b: plot individual posterior deviation of FOF inactivation to control (matrix, row = rat, column = parameter)
#'
#' @param model = 'rho-and-omega', 'rho-only', 'omega-only'
#'
#' @return ggplot object
figure_shift_individual_deviation = function(model = 'rho-and-omega'){
  # figure5b: plot individual posterior deviation of FOF inactivation to control (matrix, row = rat, column = parameter)
  datasets = c('Control', 'Bilateral-FOF', 'Bilateral-PPC')
  df_control = read.csv(sprintf('csv/all_%s_fits.csv', model)) %>% filter(dataset == 'Control') %>% select(!c('X', 'dataset'))
  df_fof = read.csv(sprintf('csv/all_%s_fits.csv', model)) %>% filter(dataset == 'Bilateral-FOF') %>% select(!c('X', 'dataset'))
  df_deviation = df_fof - df_control

  param_lower_limits = c(-0.5, -0.5, -0.5, -0.5, -0.5) # rho, sigma, omega_rational, omega_lottery, omega_surebet deviations
  param_upper_limits = c(0.5, 0.5, 0.5, 0.5, 0.5)
  # loop through each subject and parameter
  for (i in 1:length(infusion_animals)){
    animal = infusion_animals[i]
    for (j in 1:length(params)){
      this_param = sprintf('%s%d', params[j], i)
      this_df = df_deviation %>% select(contains(this_param))
       p = ggplot(this_df) + theme_classic(BASE_SIZE) +
        geom_density(mapping = aes_string(x = this_param, y = '..scaled..'), fill = 'purple4', alpha = 0.5) +
        scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
        geom_vline(xintercept = 0, color = 'black', size = 1) +
        theme(legend.position = 'none',
              axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
              axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        scale_x_continuous(limits = c(param_lower_limits[j], param_upper_limits[j]),
                            breaks = c(param_lower_limits[j], (param_lower_limits[j]+param_upper_limits[j])/2, param_upper_limits[j]))
      if (j == 1){ # if in the first column
        p = p + ylab(" ")
      }
      if (i == length(infusion_animals)){ # if in the last row
        p = p + xlab(TeX(latex_delta_params[params[j]])) + theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 13))
      }
      if (i == 1 & j == 1){
        P = p
      } else {
        P = P + p
      }
    }
  }
  P = P + plot_layout(nrow = length(infusion_animals), ncol = length(params))
  return(P)
}

#' figure_shift_prediction
#'
#' figure5a & figureS10: model prediction ribbons overlyed with control and FOF-inactivation binned choice data
#'
#'
#' @return ggplot object
figure_shift_prediction = function(){
  # figure5a & figureS10: model prediction ribbons overlyed with control and FOF-inactivation binned choice data
  control_df = read.csv('csv/figure_behavior_population.csv') %>% mutate(which_trial = 1)
  fof_df = read.csv('csv/figure_infusion_bi_fof.csv') %>%
    filter(dosage == 0.3) %>% filter(as.Date(trialtime) < '2020-11-19') %>% mutate(which_trial = 2) # experiment 1
  df = rbind(control_df, fof_df)
  for (animal in infusion_animals){
    subj_df = df %>% filter(subjid == animal)
    bino_df = subj_df %>% group_by(subjid, which_trial, lottery_mag, lottery_prob, sb_mag, total_rew_multi) %>%
      add_tally() %>% summarise(n_trials = mean(n), n_chose_lott = sum(choice)) %>%
      mutate(delta_ev = lottery_mag*lottery_prob*total_rew_multi - sb_mag*total_rew_multi) %>%
      ungroup()
    abs_lottery_mag = subj_df$lottery_mag
    pred_df = subj_df %>% group_by(which_trial) %>%
      data_grid(lottery_mag = seq_range(abs_lottery_mag, by = 0.5),
                lottery_prob = get_freq_task_param(subj_df, 'lottery_prob'),
                sb_mag = get_freq_task_param(subj_df, 'sb_mag'),
                total_rew_multi = get_freq_task_param(subj_df, 'total_rew_multi'),
                n_trials = 200) %>% ungroup() %>%
      mutate(delta_ev = lottery_mag*lottery_prob*total_rew_multi - sb_mag*total_rew_multi)
    colnames(pred_df) = paste0('pred_', colnames(pred_df))
    fname = sprintf('fits/%d_inactivation_pred.RData', animal)
    if (file.exists(fname)){
      load(fname)
    } else{
      data = append(as.list(bino_df), as.list(pred_df))
      data$T = dim(bino_df)[1]
      data$P = dim(pred_df)[1]
      fit = sampling(inactivation_pred_model, data = data, seed = 194838, refresh = 0)
      check_hmc_diagnostics(fit)
      save(fit, file = fname)
    }
    # extract simulated n_chose_lott and confidence intervals
    draws = rstan::extract(fit)
    ncl_df = as.data.frame(t(draws$pred_n_chose_lott))
    pred_df = pred_df %>% mutate(y = rowMeans(ncl_df)/pred_n_trials,
                                 ymin_80 = apply(ncl_df, 1, quantile, 0.1)/pred_n_trials,
                                 ymax_80 = apply(ncl_df, 1, quantile, 0.9)/pred_n_trials,
                                 ymin_95 = apply(ncl_df, 1, quantile, 0.025)/pred_n_trials,
                                 ymax_95 = apply(ncl_df, 1, quantile, 0.975)/pred_n_trials,
                                 ymin_99 = apply(ncl_df, 1, quantile, 0.005)/pred_n_trials,
                                 ymax_99 = apply(ncl_df, 1, quantile, 0.995)/pred_n_trials)
    # plot
    p = ggplot(subj_df) + theme_classic(base_size = BASE_SIZE) +
      geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
      geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) +
      scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
      scale_x_continuous(breaks = c(round(min(subj_df$delta_ev)/10)*10, 0, round(max(subj_df$delta_ev)/10)*10)) +
      annotate("text", label = animal, x = max(subj_df$delta_ev)*0.70, y = 0.1, size = 7) +
      xlab(expression(EV[lottery]-EV[surebet])) + ylab("P(Chose Lottery)") +
      geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_80, ymax = ymax_80, fill = as.factor(pred_which_trial)), alpha = 0.6) +
      geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_95, ymax = ymax_95, fill = as.factor(pred_which_trial)), alpha = 0.3) +
      geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_99, ymax = ymax_99, fill = as.factor(pred_which_trial)), alpha = 0.1) +
      stat_summary_bin(mapping = aes(x = delta_ev, y = choice, color = as.factor(which_trial)), fun.data = bino, geom = 'pointrange', position = position_dodge(0)) +
      scale_fill_manual(values = c('azure4', 'purple4')) +
      scale_color_manual(values = c('azure4', 'purple4')) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none')
    # if (animal == 2152){
    #   p = p + theme(axis.title.y = element_text(angle = 90))
    # }
    # if (animal == 2156){
    #   p = p + theme(axis.title.x = element_text(), axis.title.y = element_text(angle = 90))
    # }
    if (animal == 2152){
      P = p
    } else{
      P = P + p
    }
    #scale_save(p, sprintf('%d_inactivation_pred', animal), 8, 8, 1)
  }
  P = P + plot_layout(nrow = 2)
  return(P)
}

#' figure_shift_elpd
#'
#' figure5c: compare ELPD from 10-fold cross-validation results using the rho-only, omega-only and rho-and-omega inactivation model
#'
#'
#' @return ggplot object
figure_shift_elpd = function(){
  # figure5c: compare ELPD from 10-fold cross-validation results using the rho-only, omega-only and rho-and-omega inactivation model

  #df = kfold(10, inactivation_rho_cv_model, 'rho-only')
  #df = kfold(10, inactivation_omega_cv_model, 'omega-only')
  #df = kfold(10, inactivation_cv_model, 'rho-and-omega')

  rho_only = read.csv('csv/rho-only_kfold.csv') %>% mutate(model = 'rho-only')
  omega_only = read.csv('csv/omega-only_kfold.csv') %>% mutate(model = 'omega-only')
  rho_and_omega = read.csv('csv/rho-and-omega_kfold.csv') %>% mutate(model = 'rho-and-omega')
  df = rbind(rho_only, omega_only, rho_and_omega) %>% mutate(subjid = as.character(subjid))

  p = ggplot(df) + theme_classic(BASE_SIZE) +
    geom_pointrange(mapping = aes(x = model, y = elpd, ymin = elpd - elpd.1, ymax = elpd + elpd.1, color = model, shape = model),
                    position = position_dodge(0.1)) +
    xlab(' ') + ylab('ELPD') + facet_wrap(~subjid, scales = 'free', ncol = 8) +
    scale_color_manual(values = c('#B9B7BD', '#868B8E', 'black')) +
    scale_shape_manual(values = c(15, 16, 17)) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = 'none')
  return(p)
}
