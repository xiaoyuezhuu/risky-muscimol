# functions for the supplementary plots

#' figureS_timeline
#'
#' figureS1: timeline plot of all the experimental manipulations performed on an animal
#'
#' @param animal: a subjid in the list of infusion_animals
#'
#' @return ggplot object
figureS_timeline = function(animal = 2156){
  # figureS1: timeline plot of all the experimental manipulations performed on an animal
  timeline_df = read.csv('csv/infusion_timeline.csv')
  df = timeline_df %>% filter(subjid == animal)
  df$area = factor(df$area, c('control' ,'FOF', 'PPC' ,'Both'))
  df$side = factor(df$side, c('control' ,'Left', 'Right', 'Both'))
  p = ggplot(df, aes(x = days_post_surgery, y = mean_chose_lott*100, color = side, shape = area)) + theme_classic(base_size = 13) +
    geom_point(aes(size = !is.na(event))) + ylab('% Chose Lottery') +
    annotate("text", label = animal, x = min(df$days_post_surgery)*1.01, y = 1) +
    scale_shape_manual(values = c(16, 18, 15, 17)) + # round, diamond, square, triangle
    scale_color_manual(values = c('black', 'mediumseagreen', 'brown2', 'dodgerblue')) +
    scale_size_manual(values = c(2, 4)) +
    scale_x_continuous(sec.axis = sec_axis(~., name = 'Days after surgery'),
                       breaks = df$days_post_surgery[!is.na(df$event)],
                       labels = df$event[!is.na(df$event)],
                       limits = c(min(df$days_post_surgery), max(df$days_post_surgery)*1.05)) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
    geom_hline(yintercept = 50, linetype = 'dotted', alpha = 0.4) +
    geom_vline(xintercept = df$days_post_surgery[!is.na(df$sb_change)], linetype = 'solid', color = 'deepskyblue') +
    theme(legend.position = 'none',
          axis.text.x.bottom = element_text(angle = -35, hjust = -0.05),
          axis.title.x.bottom = element_blank(), axis.title.x.top = element_blank())
  return(p)
}

#' figureS_behavior
#'
#' figureS2b: soft fixation analysis
#'
#'
#' @return ggplot object
figureS_behavior = function(){
  # figureS2b: soft fixation analysis
  stay_prob_df = read.csv('csv/soft_fixation.csv')
  stay_prob_df = stay_prob_df %>% group_by(subjid, bins) %>%
    summarise(y = bino(hold)$y)
  p = ggplot(stay_prob_df, aes(x = bins, y = y)) + theme_classic(BASE_SIZE) +
    geom_col(color = '#747474', fill = '#747474', alpha = 0.4) + facet_wrap(~subjid, nrow = 2) +
    xlab('Bins before go-cue') + ylab('P(in port)') +
    scale_x_continuous(breaks = c(250, 750)) + scale_y_continuous(breaks = c(0, 0.5, 1)) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0))
  return(p)
}

#' figureS_infusion
#'
#' figureS4 & S5: GLMM fits and plots for Bilateral FOF/PPC, Unilateral FOF/PPC
#'
#' @param region: 'bi_fof', 'bi_ppc', 'uni_fof', 'uni_ppc'
#'
#' @return ggplot object
figureS_infusion = function(region = 'bi_fof'){
  # figureS4 & S5: GLMM fits and plots for Bilateral FOF/PPC, Unilateral FOF/PPC
  df = read.csv(sprintf('csv/figure_infusion_%s.csv', region)) %>%
    filter(as.Date(trialtime) < '2020-11-19') %>%
    filter(dosage != 0.15 & dosage != 0)
  control_df = read.csv('csv/figure_behavior_population.csv')
  df = rbind(df, control_df)

  if (str_detect(region, 'bi')){
    if (region == 'bi_fof'){colors = c('azure4', 'rosybrown1', 'purple4')} else{ colors = c('azure4', 'gold2')}
    df$infusion_side = factor(df$infusion_side)
    df$infusion_bino = factor(df$infusion_bino)
    # load GLMM or fit GLMM
    fname = sprintf('fits/%s_glmm.RData', region)
    if (file.exists(fname)){
      load(fname)
    } else{
      cat('Fitting GLMM...\n')
      m1 = glmer('choice ~ delta_ev*dosage + (delta_ev*dosage | subjid)', df, family = binomial)
      save(m1, file = fname)
    }
    ind_pred_df = data_grid(df, delta_ev = seq_range(delta_ev, by = 1), dosage = dosage, subjid = subjid)
    ind_pred_df$pred = predict(m1, ind_pred_df, type = 'response', allow.new.levels=TRUE)
    qt = quantile(df$delta_ev, probs = seq(0, 1, 0.25))
    p = ggplot(df) +
      #stat_summary_bin(mapping = aes(x = delta_ev, y = choice, color = dosage), breaks = qt, fun.data = bino, geom = 'pointrange', position = position_dodge(10)) +
      stat_summary_bin(mapping = aes(x = delta_ev, y = choice, color = as.factor(dosage)), fun.data = bino, geom = 'pointrange', position = position_dodge(10)) +
      geom_line(ind_pred_df, mapping = aes(x = delta_ev, y = pred, color = as.factor(dosage)), alpha = 0.5, size = 1) +
      scale_color_manual(values = colors) + scale_fill_manual(values = colors) +
      labs(fill = 'Dose(µg)', color = 'Dose(µg)') +
      xlab(expression(EV[lottery]-EV[surebet])) + ylab("P(Chose Lottery)")
  } else if (str_detect(region, 'uni')){
    df$dosage = factor(df$dosage)
    df$infusion_side = factor(df$infusion_side, )
    df$infusion_bino = factor(df$infusion_bino)
    # load GLMM or fit GLMM
    fname = sprintf('fits/%s_glmm.RData', region)
    if (file.exists(fname)){
      load(fname)
    } else{
      cat('Fitting GLMM...\n')
      m1 = glmer('choose_right ~ pro_right_delta_ev*infusion_side + (pro_right_delta_ev*infusion_side | subjid)', df, family = binomial)
      save(m1, file = fname)
    }
    ind_pred_df = data_grid(df, pro_right_delta_ev = seq_range(pro_right_delta_ev, by = 1), infusion_side = infusion_side, subjid = subjid)
    ind_pred_df$pred = predict(m1, ind_pred_df, type = 'response', allow.new.levels=TRUE)
    qt = quantile(df$delta_ev, probs = seq(0, 1, 0.25))
    p = ggplot(df) +
      #stat_summary_bin(mapping = aes(x = pro_right_delta_ev, y = choose_right, color = infusion_side), breaks = qt, fun.data = bino, geom = 'pointrange', position = position_dodge(10)) +
      stat_summary_bin(mapping = aes(x = pro_right_delta_ev, y = choose_right, color = infusion_side), fun.data = bino, geom = 'pointrange', position = position_dodge(10)) +
      geom_line(ind_pred_df, mapping = aes(x = pro_right_delta_ev, y = pred, color = infusion_side), alpha = 0.5, size = 1) +
      scale_color_manual(values = side_colors) + scale_fill_manual(values = side_colors) +
      labs(fill = 'Infusion Side', color = 'Infusion Side') +
      xlab(expression(EV[right]-EV[left])) + ylab("P(Chose Right)")
  }
  # plot
  ntrials_df = df %>% group_by(subjid) %>% tally() %>%
    mutate(label = paste0('n=', n), x = max(df$delta_ev)*0.75,  y = 0.1)
  p = p + theme_classic(base_size = BASE_SIZE) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
    geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) +
    scale_x_continuous(limits = c(-100, 300), breaks = c(-100, 0, 200)) +
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    geom_text(data = ntrials_df, mapping = aes(x = x, y = y, label = label)) +
    facet_wrap(~subjid) +
    theme(legend.position = 'bottom')
  return(p)
}

#' figureS_RT
#'
#' figureS6 & S7: LMM fits and plots for reaction time in Bilateral FOF/PPC, Unilateral FOF/PPC
#'
#' @param region: 'bi_fof', 'bi_ppc', 'uni_fof', 'uni_ppc'
#'
#' @return ggplot object
figureS_RT = function(region = 'bi_fof'){
  # figureS6 & S7: LMM fits and plots for reaction time in Bilateral FOF/PPC, Unilateral FOF/PPC
  choice_title = c('Choose sure-bet', 'Choose lottery')
  choices = c(0, 1)
  df = read.csv(sprintf('csv/figure_infusion_%s.csv', region)) %>%
    filter(as.Date(trialtime) < '2020-11-19') %>%
    filter(dosage != 0.15 & dosage != 0) %>%
    filter(RT < 3) %>% filter(!is.na(RT))
  control_df = read.csv('csv/figure_behavior_population.csv')
  df = rbind(df, control_df)
  fname = sprintf('fits/%s_RT_glmm.RData', region)
  # load or fit LMM
  if (str_detect(region, 'bi')){
    # load saved model filts
    if (file.exists(fname)){
      load(fname)
    } else{
      cat('Fitting GLMM...\n')
      m1 = lmer('log_RT ~ delta_ev*dosage*choice + (delta_ev*dosage*choice|subjid)', df)
      save(m1, file = fname)
    }
    x = 'delta_ev'
    c = 'dosage'
    if (region == 'bi_fof'){colors = c('azure4', 'rosybrown1', 'purple4')} else{ colors = c('azure4', 'gold2')}
    lab = 'Dosage'
    xlab = expression(EV[lottery]-EV[surebet])
  } else if (str_detect(region, 'uni')){
    df$infusion_side = factor(df$infusion_side, levels = c('control', 'L', 'R'))
    if (file.exists(fname)){
      load(fname)
    } else{
      cat('Fitting GLMM...\n')
      m1 = lmer('log_RT ~ delta_ev*infusion_side*choice + (pro_right_delta_ev*infusion_side*choice | subjid)', df)
      save(m1, file = fname)
    }
    x = 'pro_right_delta_evx'
    c = 'infusion_side'
    colors = side_colors
    lab = 'Infusion Side'
    xlab = expression(EV[right]-EV[left])
    df$pro_right_delta_evx = df$delta_ev
  }
  df$dosage = factor(df$dosage)
  df$choice = factor(df$choice)
  df$pred = predict(m1)
  for (i in 1:2){
    p = ggplot(df %>% filter(choice == choices[i]), aes_string(x = x, y = 'log_RT', color = c, fill = c)) +
      theme_classic(base_size = BASE_SIZE) + xlab(xlab) + ylab("log(RT)") +
      stat_summary(fun.data = mean_se) +
      ggtitle(choice_title[i]) +
      geom_line(mapping = aes_string(x = x, y = 'pred', color = c)) +
      scale_color_manual(values = colors) + scale_fill_manual(values = colors) +
      labs(fill = lab, color = lab) + facet_wrap(subjid~.) +
      scale_x_continuous(limits = c(-50, 200), breaks = c(-50, 0, 150)) +
      scale_y_continuous(limits = c(-1.5, 1.5), breaks = c(-1.5, 0, 1.5)) +
      theme(legend.position = 'bottom',
            axis.text.x = element_text(size = 11)) + facet_wrap(~subjid, scales = 'fixed')
    if (i == 1){P = p} else{P = P + p}
  }
  return(P)
}

#' figureS_sanity
#'
#' figure S8a: generate model parameters from the prior range, simulate dataset, and see if the model can recover them accurately
#' returns a scatter plot of predictions vs. true values
#' the plot will be different each time it's run
#'
#' @param N: number of synthetic datasets
#'
#' @return ggplot object
figureS_sanity = function(N = 20){
  # figure S8a: generate model parameters from the prior range, simulate dataset, and see if the model can recover them accurately
  # returns a scatter plot of predictions vs. true values
  # the plot will be different each time it's run
  params_df = data.frame(true = NULL, params = NULL, model_mean = NULL, model_sd = NULL)
  params = c('rho', 'sigma', 'omega_rational', 'omega_lottery', 'omega_surebet')
  # define parameter-specific limits and breaks
  lower_limits = list('rho' = 0, 'sigma' = 0, 'omega_rational' = 0, 'omega_lottery' = 0, 'omega_surebet' = 0)
  upper_limits = list('rho' = 2, 'sigma' = 6, 'omega_rational' = 1, 'omega_lottery' = 1, 'omega_surebet' = 1)
  lower_breaks = list('rho' = 0, 'sigma' = 0, 'omega_rational' = 0, 'omega_lottery' = 0, 'omega_surebet' = 0)
  middle_breaks = list('rho' = 1, 'sigma' = 3, 'omega_rational' = 0.5, 'omega_lottery' = 0.5, 'omega_surebet' = 0.5)
  upper_breaks = list('rho' = 2, 'sigma' = 6, 'omega_rational' = 1, 'omega_lottery' = 1, 'omega_surebet' = 1)
  for (n in 1:N){
    # generate parameters
    true_params = list()
    true_params$rho = exp(rnorm(1, log(0.9), 0.4))
    true_params$sigma = exp(rnorm(1, log(2), 0.5))
    true_params$omega = rdirichlet(1, alpha = c(6, 2, 2))
    # create synthetic data
    n_trials = 2000
    df = data.frame(lottery_mag = sample(c(0, 2, 4, 8, 16, 32), n_trials, replace = TRUE),
                    sb_mag = 3, total_rew_multi = 8, lottery_prob = 0.5) %>%
      mutate(delta_ev = total_rew_multi*lottery_mag*lottery_prob - total_rew_multi*sb_mag)
    df$choice = rho_sigma_agent(prob = FALSE, params = true_params, df$sb_mag, df$lottery_mag,
                                df$lottery_prob, df$total_rew_multi)
    # fit model
    data = as.list(binomialize(df))
    data$T = 6
    fit = sampling(individual_model, data = data, seed = 194838, refresh = 0)
    # append model estimates
    draws = as.data.frame(rstan::extract(fit)[c(names(true_params), 'lp__')])
    max_lp = which.max(draws$lp__)
    temp_df = data.frame(true = unlist(true_params), params = params,
                         mlp = as.numeric(draws[max_lp,][1:5]),
                         n = n)
    params_df = rbind(params_df, temp_df)
  }
  # make the scatter plots
  params_df$n = as.factor(params_df$n)
  for (i in 1:length(params)){
    param = params[i]
    this_df = params_df %>% filter(params == param)
    corr = cor.test(this_df$true, this_df$mlp)
    p = ggplot(this_df, aes(x = true, y = mlp, color = n)) + theme_classic(base_size = BASE_SIZE) +
      geom_point() + xlab(TeX(latex_params[param])) +
      theme(legend.position = 'none') +
      ylab('Model Estimate') + geom_abline(slope = 1, linetype =  'dashed', alpha = 0.4) +
      geom_smooth(method = lm, formula = y ~ x, color = 'gray', size = 0.2) +
      annotate("text", label = sprintf('R = %.3f', corr$estimate), x = middle_breaks[[param]], y = 0.1) +
      scale_x_continuous(limits = c(lower_limits[[param]], upper_limits[[param]]),
                         breaks = c(lower_breaks[[param]], middle_breaks[[param]], upper_breaks[[param]])) +
      scale_y_continuous(limits = c(lower_limits[[param]], upper_limits[[param]]),
                         breaks = c(lower_breaks[[param]], middle_breaks[[param]], upper_breaks[[param]])) # add an abline
    if (i == 1){P = p}
    else{p = p + theme(axis.title.y = element_blank())
    P = P + p}
  }
  P = P + plot_layout(ncol = length(params))
  return(P)
}

#' figureS_control_pairs
#'
#' figureS9: aggregated pairs plot with control posterior distributions
#'
#'
#' @return ggplot object
figureS_control_pairs = function(){
  # figureS9: aggregated pairs plot with control posterior distributions
  df = read.csv('csv/all_Control_fits.csv') %>% select(!c('X', 'dataset'))
  p = aggregated_pairs(df)
  return(p)
}

#' figureS_individual_posterior
#'
#' figureS11: plot individual posterior distribution in a matrix, where row = subjid, column = parameter, dataset = color
#'
#' @param model: 'rho-and-omega', 'rho-only', 'omega-only' for different inactivation models
#'
#' @return ggplot object
figureS_individual_posterior = function(model ='rho-and-omega'){
  # figureS11: plot individual posterior distribution in a matrix, where row = subjid, column = parameter, dataset = color
  datasets = c('Control', 'Bilateral-FOF', 'Bilateral-PPC')
  df = read.csv(sprintf('csv/all_%s_fits.csv', model)) %>% select(!contains('NA.'))
  # set 2155, set Bi-PPC to NA
  df_2155 = df %>% select(contains('4') | contains('dataset'))
  df = df %>% select(!contains('4'))
  df_2155[df_2155$dataset == 'Bilateral-PPC', ] = NA
  df = cbind(df, df_2155 %>% select(!'dataset'))
  df$dataset = factor(df$dataset, c('Control', 'Bilateral-FOF', 'Bilateral-PPC'))
  param_lower_limits = c(0, 0, 0, 0, 0)
  param_upper_limits = c(1, 10, 1, 1, 1)
  # loop through each subject and parameter
  for (i in 1:length(infusion_animals)){
    animal = infusion_animals[i]
    for (j in 1:length(params)){
      this_param = sprintf('%s%d', params[j], i)
      this_df = df %>% select(contains(this_param) | contains('dataset'))
      color_scheme_set('gray')
      p = ggplot(this_df) + theme_classic(BASE_SIZE) +
        geom_density(mapping = aes_string(x = this_param, y='..scaled..', fill = 'dataset'), alpha = 0.4, color = 'black', size = 0.2) +
        scale_fill_manual(values = dataset_colors) +
        scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
        theme(legend.position = 'none', axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        scale_x_continuous(limits = c(param_lower_limits[j], param_upper_limits[j]),
                           breaks = c(param_lower_limits[j], (param_lower_limits[j]+param_upper_limits[j])/2, param_upper_limits[j]))
      if (j == 1){ # if in the first column
        p = p + ylab(animal) + theme(axis.title.y = element_text())
      }
      if (i == length(infusion_animals)){ # if in the last row
        p = p + xlab(TeX(latex_params[params[j]])) + theme(axis.title.x = element_text())
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
