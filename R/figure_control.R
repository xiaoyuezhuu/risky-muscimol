# functions for figure3 (figure_control)

#' figure_control_bias
#'
#' figure3b: compare ipsiateral bias before, during and after infusoin for both free choices and risky choices
#'
#'
#' @return ggplot object
figure_control_bias = function(){
  # figure3b: compare ipsiateral bias before, during and after infusoin for both free choices and risky choices
  bias_df = read.csv('csv/figure_control_bias_PPC.csv')
  # plot ipsilateral bias
  free_df = bias_df %>% select(c('before_bias', 'infusion_bias', 'after_bias', 'subjid')) %>%
    pivot_longer(!subjid, names_to = 'infusion')
  free_df$infusion = factor(free_df$infusion, levels = c('before_bias', 'infusion_bias', 'after_bias'))
  risky_df = bias_df %>% select(c('risky_before_bias', 'risky_infusion_bias', 'risky_after_bias', 'subjid')) %>%
    pivot_longer(!subjid, names_to = 'infusion')
  risky_df$infusion = factor(risky_df$infusion, levels = c('risky_before_bias', 'risky_infusion_bias', 'risky_after_bias'))
  p = plot_before_infusion_after(free_df)
  return(p)
}

#' plot_before_infusion_after
#'
#' helper function for figure_control_compare_bias
#'
#'
#' @return ggplot object
plot_before_infusion_after = function(plot_df){
  # helper function for figure_control_compare_bias
  p = ggplot(plot_df, aes(x = infusion, y = value*100)) + theme_classic(base_size = BASE_SIZE) +
    geom_point(aes(fill = infusion), size = 3, shape = 21) + geom_line(aes(group = subjid)) +
    xlab(' ') + ylab('% Ipsilateral Bias') +
    geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.4) +
    scale_fill_manual(values = c('black', 'yellow', 'black')) +
    scale_x_discrete(labels = c('Before', 'Infusion', 'After')) +
    scale_y_continuous(limits = c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
    theme(legend.position = 'none', axis.text.x = element_text(angle = -60, hjust = -0.1, face = 'bold'))
  return(p)
}

#' figure_control_bar
#'
#' figure3c: bar plot showing mean absolute change in % ipsilateral bias in free choice and risky choices
#'
#'
#' @return ggplot object
figure_control_bar = function(){
  # figure3c: bar plot showing mean absolute change in % ipsilateral bias in free choice and risky choices
  bias_df = read.csv('csv/figure_control_bias_PPC.csv')
  plot_df = data.frame(means = c(mean(bias_df$risky_increment), mean(bias_df$free_increment)),
                       ses = c(se(bias_df$risky_increment), se(bias_df$free_increment)),
                       task = c('Risky', 'Free'))
  plot_df$task = factor(plot_df$task, c('Risky', 'Free'))
  p = ggplot(plot_df) + theme_classic(base_size = BASE_SIZE) +
    geom_col(mapping = aes(x = task, y = means*100), color = 'black', fill = 'gold2') +
    geom_errorbar(mapping = aes(x = task, ymin = (means - ses)*100, ymax = (means + ses)*100), width = 0.1) +
    scale_y_continuous(limits = c(-20, 80), breaks = c(0, 20, 40, 60, 80)) +
    geom_hline(yintercept = 0) +
    xlab(' ') + ylab(expression(Delta~Ipsilateral~Bias)) +
    theme(legend.position = 'none', axis.text.x = element_text(angle = 0, face = 'bold'))
  return(p)
}

#' figure_control_risky
#'
#' figure3d: plots psychometric curves (model prediction + binned data), color by unilateral infusion and control
#'
#'
#' @return ggplot object
figure_control_risky = function(){
  # figure3d: plots psychometric curves (model prediction + binned data), color by unilateral infusion and control
  df = preprocessRisk(read.csv('csv/figure_control_risky_df.csv')) %>%
    mutate(pro_ipsi_delta_ev = case_when(lottery_side == 'BotR' & infusion_side == 'R' ~ delta_ev,
                                         lottery_side == 'BotR' & infusion_side == 'L' ~ -delta_ev,
                                         lottery_side == 'BotL' & infusion_side == 'R' ~ -delta_ev,
                                         lottery_side == 'BotL' & infusion_side == 'L' ~ delta_ev)) %>%
    mutate(chose_ipsi = case_when(choice_side == 'left' & infusion_side == 'L' ~ 1,
                                               choice_side == 'right' & infusion_side == 'L' ~ 0,
                                               choice_side == 'left' & infusion_side == 'R' ~ 0,
                                               choice_side == 'right' & infusion_side == 'R' ~ 1)) %>%
    mutate(condition = case_when(infusion_bino == 0 ~ 'Control',
                                 infusion_bino == 1 & infusion_side == 'L' ~ 'L',
                                 infusion_bino == 1 & infusion_side == 'R' ~ 'R'))
  df$dosage = factor(df$dosage)

  # Use GLM to plot ribbons
  m0 = glm('chose_ipsi ~ pro_ipsi_delta_ev*condition', df, family = binomial)
  #m1 = glmer('chose_ipsi ~ pro_ipsi_delta_ev*condition + (pro_ipsi_delta_ev*condition | subjid)', df, family = binomial)
  pop_pred_df = data_grid(df, pro_ipsi_delta_ev = seq_range(pro_ipsi_delta_ev, by = 1), condition = condition)
  pop_pred = predict(m0, pop_pred_df, type = 'response', allow.new.levels=TRUE, se.fit = TRUE)
  pop_pred_df$pred = pop_pred$fit
  pop_pred_df$ymin = pop_pred$fit - pop_pred$se.fit
  pop_pred_df$ymax = pop_pred$fit + pop_pred$se.fit

  qt = quantile(df$delta_ev, probs = seq(0, 1, 0.20))
  p = ggplot(df) + theme_classic(base_size = BASE_SIZE) +
    stat_summary_bin(df, mapping = aes(x = pro_ipsi_delta_ev, y = chose_ipsi, color = condition), breaks = qt, fun.data = bino) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
    geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) +
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(breaks = c(-260, 0, 260)) +
    annotate("text", label = sprintf('%d trials \n %d sessions', dim(df)[1], length(unique(df$sessid))), x = max(df$pro_ipsi_delta_ev)*0.6, y = 0.1, size = ANNOTATION_SIZE) +
    xlab(expression(EV[ipsi]-EV[contra])) + ylab("P(Chose Ipsi)") +
    geom_ribbon(pop_pred_df, mapping = aes(x = pro_ipsi_delta_ev, ymin = ymin, ymax = ymax, fill = condition), alpha = 0.5) +
    scale_color_manual(values = side_colors) + scale_fill_manual(values = side_colors) +
    labs(fill = 'Side', color = 'Side') + theme(legend.position = 'none')
  return(p)
}
