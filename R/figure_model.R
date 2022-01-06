# functions for figure4 (figure_model)

#' figure_model_prediction
#'
#' figure4b & figureS8b: plots psychometric curves (model ribbon prediction + binned data)
#' If fitting a new model, the ribbon might be slightly different each time
#'
#'
#' @return ggplot object
figure_model_prediction = function(){
  # figure4b & figureS8b: plots psychometric curves (model ribbon prediction + binned data)
  # if fitting a new model, the ribbon might be slightly different each time
  df = read.csv('csv/figure_behavior_population.csv')
  for (animal in infusion_animals){
    subj_df = df %>% filter(subjid == animal)
    subj_df = subj_df %>% filter(sb_mag == get_freq_task_param(subj_df, 'sb_mag'))
    bino_df = binomialize(subj_df)
    pred_df = genSynSmooth(subj_df)
    if (file.exists(sprintf('fits/%d_pred.RData', animal))){
      load(sprintf('fits/%d_pred.RData', animal))
    } else{
      data = append(as.list(bino_df), as.list(pred_df))
      data$T = dim(bino_df)[1]
      data$P = dim(pred_df)[1]
      fit = sampling(individual_pred_model, data = data, seed = SEED, refresh = 0)
      check_hmc_diagnostics(fit)
      save(fit, file = sprintf('fits/%d_pred.RData', animal))
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
    rho = mean(draws$rho)
    sigmav = mean(draws$sigmav)
    p = ggplot(subj_df) + theme_classic(base_size = BASE_SIZE) +
      geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
      geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) +
      scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
      scale_x_continuous(breaks = c(round(min(subj_df$delta_ev)/10)*10, 0, round(max(subj_df$delta_ev)/10)*10)) +
      annotate("text", label = animal, x = max(subj_df$delta_ev)*0.70, y = 0.1, size = 7) +
      xlab(expression(EV[lottery]-EV[surebet])) + ylab("P(Chose Lottery)") +
      geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_80, ymax = ymax_80), fill = 'gray', alpha = 0.8) +
      geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_95, ymax = ymax_95), fill = 'gray', alpha = 0.6) +
      geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_99, ymax = ymax_99), fill = 'gray', alpha = 0.3) +
      stat_summary_bin(mapping = aes(x = delta_ev, y = choice), fun.data = bino, geom = 'pointrange', position = position_dodge(10)) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    # if (animal == 2152){
    #   p = p + theme(axis.title.y = element_text(angle = 90))
    # }
    # if (animal == 2156){
    #   p = p + theme(axis.title.x = element_text(), axis.title.y = element_text(angle = 90))
    # }
    scale_save(p, sprintf('%d_model', animal), 8, 8, 1)
  }
}

#' figure_model_utility
#'
#' figure4c: use rho estimates from the model to plot utility curves for each subject
#'
#'
#' @return ggplot object
figure_model_utility = function(){
  # figure4c: use rho estimates from the model to plot utility curves for each subject
  params_df = read.csv('csv/all_Control_fits.csv') %>% select(matches('rho'))
  df = read.csv('csv/figure_behavior_population.csv')
  subj_list = unique(df$subjid)
  util_df = data.frame()
  for (i in 1:length(subj_list)){
    subj = subj_list[i]
    rho = as.numeric(colMeans(params_df[i]))
    lott_mags = unique(df %>% filter(subjid == subj) %>% pull(lottery_mag))
    temp_df = data.frame(subjid = subj, rho = rho, lottery_mag = lott_mags)
    util_df = rbind(util_df, temp_df)
  }
  util_df = util_df %>% mutate(utility = lottery_mag^rho)
  p = ggplot() + theme_classic(BASE_SIZE) +
    xlab('Water volume(x)') + ylab('u(x)') +
    scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(-0.05, 1.05)) +
    scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(-0.05, 1.05)) +
    geom_abline(linetype = 'dashed')
  for (subj in subj_list){
    p = p + geom_line(util_df %>% filter(subjid == subj), mapping = aes(x = lottery_mag/max(lottery_mag), y = utility/max(utility)), color = 'azure4', alpha = 0.6)
  }
  return(p)
}

#' figure_model_density
#'
#' figure4d: density plot for each parameter using concatenated posteriors from all subjects
#'
#' @return ggplot object
figure_model_density = function(){
  # figure4d: density plot for each parameter using concatenated posteriors from all subjects
  df = read.csv('csv/all_Control_fits.csv') %>% select(!c('X', 'dataset'))
  # define parameter-specific limits and breaks
  lower_limits = list('rho' = 0, 'sigma' = 0, 'omega_rational' = 0, 'omega_lottery' = 0, 'omega_surebet' = 0)
  upper_limits = list('rho' = 2, 'sigma' = 10, 'omega_rational' = 1, 'omega_lottery' = 1, 'omega_surebet' = 1)
  lower_breaks = list('rho' = 0, 'sigma' = 0, 'omega_rational' = 0, 'omega_lottery' = 0, 'omega_surebet' = 0)
  middle_breaks = list('rho' = 1, 'sigma' = 5, 'omega_rational' = 0.5, 'omega_lottery' = 0.5, 'omega_surebet' = 0.5)
  upper_breaks = list('rho' = 2, 'sigma' = 10, 'omega_rational' = 1, 'omega_lottery' = 1, 'omega_surebet' = 1)
  for (i in 1:length(params)){
    param = params[i]
    pop_param_df = as.data.frame(as_vector(df %>% select(contains(param))))
    colnames(pop_param_df) = 'pop_param'
    p = ggplot(pop_param_df, aes(x = pop_param)) + geom_density(alpha = 0.5, fill = 'azure4') +
      theme_classic(base_size = BASE_SIZE) +
      geom_vline(xintercept = apply(pop_param_df, 2, median), color = 'black', size = 1) +
      xlab(TeX(latex_params[param])) +
      theme(legend.position = 'none', axis.title.y = element_blank()) +
      scale_x_continuous(limits = c(lower_limits[[param]], upper_limits[[param]]),
                         breaks = c(lower_breaks[[param]], middle_breaks[[param]], upper_breaks[[param]]))
    if (i == 1){
      P = p
    } else{
      P = P + p
    }
  }
  P = P + plot_layout(ncol = length(params))
  return(P)
}
