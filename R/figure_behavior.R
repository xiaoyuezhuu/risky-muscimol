# functions for figure1 (figure_behavior)

#' figure_behavior_timeline
#'
#' figure1b: timeline of trials in one example session.
#' @param n: the number of trials to show
#'
#' @return ggplot object
#'
figure_behavior_timeline = function(n = 30){
  # figure1b: timeline of trials in one example session.
  df = read.csv('csv/figure_behavior_timeline.csv')
  s = sample(unique(df$sessid), 1)
  df = df %>% filter(sessid == s) %>% # sample one random session
    filter(viol != 1 & trialnum < n) %>%
    mutate(trialtype = case_when(!forced ~ 1, forced_lottery ~ 2, forced_surebet ~ 3))
  df$freq = df %>% group_indices(lottery_mag)
  df = df %>% mutate(image = sprintf('sinewaves/%d.png', freq))
  n_trials = dim(df)[1]
  df$trialnum = seq(1, n_trials)

  reward_df = df %>% filter(reward_received > 0)
  no_reward_df = df %>% filter(reward_received == 0)
  p = ggplot(df, aes(x = trialnum)) + theme_classic(BASE_SIZE) +
    geom_point(aes(y = rep(3, n_trials), shape = as.factor(trialtype), fill = as.factor(trialtype)), size = 3) +
    geom_image(aes(y = rep(2, n_trials), image = image), size = 0.03) +
    geom_point(aes(y = rep(1, n_trials), color = as.factor(choice)), shape = 18, size = 3) +
    geom_point(reward_df, mapping = aes(x = trialnum, y = rep(0, dim(reward_df)[1]), size = as.factor(reward_received)), shape = 21, fill = 'dodgerblue', alpha = 0.4) +
    geom_point(no_reward_df, mapping = aes(x = trialnum, y = rep(0, dim(no_reward_df)[1])), shape = 4, color = 'indianred', size = 3) +
    scale_y_continuous(limits = c(-0.5, 3.5), breaks = c(0, 1, 2, 3), labels = c('Reward(μl)', 'Choice', 'Tone Freq', 'Trial Type')) +
    scale_shape_manual(values = c(23, 24, 25)) + # trialtype: choice, forced_lottery, forced_surebet
    scale_fill_manual(values = c('white', 'gold3', 'dodgerblue')) + # trialtype: choice, forced_lottery, forced_surebet
    scale_color_manual(values = c('dodgerblue', 'gold3')) + # choice: surebet, lottery
    xlab('Trials in session') + ylab(' ') +
    theme(legend.position = 'none', text = element_text(color = 'black'))
  return(p)
}

#' figure_behavior_example
#'
#' figure1c: example animal psychometric curves by GLMM, thin lines for each session, thick line for combined
#'
#' @return ggplot object
figure_behavior_example = function(){
  # figure1c: example animal psychometric curves by GLMM, thin lines for each session, thick line for combined
  risk_df = read.csv('csv/figure_behavior_population.csv') %>% filter(subjid == 2154)
  # GLMM fit
  m0 = glm('choice ~ delta_ev', risk_df, family = binomial)
  m1 = glmer('choice ~ delta_ev + (delta_ev | sessid)', risk_df, family = binomial)
  avg_pred_df = data_grid(risk_df, delta_ev = seq_range(c(min(delta_ev)*1.3, max(delta_ev)*1.1), by = 1))
  sess_pred_df = data_grid(risk_df, delta_ev = seq_range(c(min(delta_ev)*1.3, max(delta_ev)*1.1), by = 1), sessid = sessid)
  avg_pred_df$pred = predict(m0, avg_pred_df, type = 'response', allow.new.levels=TRUE)
  sess_pred_df$pred = predict(m1, sess_pred_df, type = 'response', allow.new.levels=TRUE)
  p = ggplot(risk_df) + theme_classic(base_size = BASE_SIZE) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
    geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) +
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(breaks = c(round(min(risk_df$delta_ev)/10)*10, 0, round(max(risk_df$delta_ev)/10)*10)) +
    annotate("text", label = '1 animal', x = max(risk_df$delta_ev)*0.70, y = 0.2, size = ANNOTATION_SIZE) +
    annotate("text", label = sprintf('%d sessions', length(unique(risk_df$sessid))), x = max(risk_df$delta_ev)*0.70, y = 0.1, size = ANNOTATION_SIZE) +
    xlab(expression(EV[lottery]-EV[surebet])) + ylab("P(Chose Lottery)") +
    geom_line(avg_pred_df, mapping = aes(x = delta_ev, y = pred), color = 'black', alpha = 0.5, size = 2)
  for (sessidx in unique(risk_df$sessid)){
    p = p + geom_line(sess_pred_df %>% filter(sessid == sessidx),
                      mapping = aes(x = delta_ev, y = pred), color = 'darkgray', alpha = 0.3)
  }
  p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice), fun.data = bino, geom = 'pointrange', color = 'black')
  return(p)
}

#' figure_behavior_population
#'
#' figure1d: population psychometric curves by GLMM, thin colored lines per subject, thick line for population
#'
#'
#' @return ggplot object
figure_behavior_population = function(){
  # figure1d: population psychometric curves by GLMM, thin colored lines per subject, thick line for population
  df = read.csv('csv/figure_behavior_population.csv')
  # GLMM fit
  m0 = glm('choice ~ delta_ev', df, family = binomial)
  m1 = glmer('choice ~ delta_ev + (delta_ev | subjid)', df, family = binomial)
  pop_pred_df = data_grid(df, delta_ev = seq_range(c(min(delta_ev)*1.3, max(delta_ev)*1.1), by = 1))
  ind_pred_df = data_grid(df, delta_ev = seq_range(c(min(delta_ev)*1.3, max(delta_ev)*1.1), by = 1), subjid = subjid)
  pop_pred_df$pred = predict(m0, pop_pred_df, type = 'response', allow.new.levels=TRUE)
  ind_pred_df$pred = predict(m1, ind_pred_df, type = 'response', allow.new.levels=TRUE)
  p = ggplot(df) + theme_classic(base_size = BASE_SIZE) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
    geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) +
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(breaks = c(round(min(df$delta_ev)/10)*10, 0, round(max(df$delta_ev)/10)*10)) +
    annotate("text", label = sprintf('%d trials \n %d sessions \n 8 animals', dim(df)[1], length(unique(df$sessid))), x = max(df$delta_ev)*0.70, y = 0.15, size = ANNOTATION_SIZE) +
    xlab(expression(EV[lottery]-EV[surebet])) + ylab("P(Chose Lottery)") +
    geom_line(pop_pred_df, mapping = aes(x = delta_ev, y = pred), color = 'black', alpha = 0.5, size = 2)
  for (i in 1:length(unique(df$subjid))){
    subjidx = unique(df$subjid)[i]
    p = p + geom_line(ind_pred_df %>% filter(subjid == subjidx),
                      mapping = aes(x = delta_ev, y = pred), color = subj_colors[i])
  }
  return(p)
}

#' figure_behavior_session_stat
#'
#' figure1e: average number of trials in session vs. number of sessions, colored by subject
#'
#'
#' @return ggplot object
figure_behavior_session_stat = function(){
  # figure1e: average number of trials in session vs. number of sessions, colored by subject
  df = read.csv('csv/figure_behavior_population.csv')
  df1 = df %>% group_by(subjid) %>% tally() %>% rename(n_trials=n)
  df2 = df %>% group_by(subjid, sessid) %>% summarise() %>%
    group_by(subjid) %>% tally() %>% rename(n_sessions = n)
  plot_df = merge(df1, df2, on = 'subjid') %>% mutate(avg = n_trials/n_sessions)
  p = ggplot(plot_df, aes(x = n_sessions, y = avg, color = as.factor(subjid))) +
    theme_bw(base_size = BASE_SIZE) + geom_point(size = 3) +
    geom_text(aes(label = subjid), hjust = 1.3, vjust = 0.5) +
    xlab('# sessions') + ylab('mean(# choices/session)') +
    #scale_y_continuous(limits = c(60, 150)) +
    scale_x_continuous(limits = c(5, 20)) +
    theme(legend.position = 'none') +
    scale_color_manual(values = subj_colors)
  return(p)
}
