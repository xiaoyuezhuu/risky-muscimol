# functions for figure2 (figure_infusion)

#' figure_infusion_bi
#'
#' figure2b & 2e: plots psychometric curves (model prediction + binned data), color by bilateral infusion and control
#'
#' @param region: 'FOF' or 'PPC'
#'
#' @return ggplot object
figure_infusion_bi = function(region = 'FOF'){
  # figure2b & 2e: plots psychometric curves (model prediction + binned data), color by bilateral infusion and control
  if (region == 'FOF'){
    df = read.csv('csv/figure_infusion_bi_fof.csv') %>% filter(dosage %in% c(0.075, 0.3))
    dosage_colors = FOF_dosage_colors
  } else{
    df = read.csv('csv/figure_infusion_bi_ppc.csv') %>% filter(dosage != 0)
    dosage_colors = PPC_dosage_colors
  }
  control_df = read.csv('csv/figure_behavior_population.csv')
  df = df %>% filter(as.Date(trialtime) < '2020-11-19')
  df = rbind(control_df, df)

  # Use GLM to plot ribbons
  m0 = glm('choice ~ delta_ev*dosage', df, family = binomial)
  pop_pred_df = data_grid(df, delta_ev = seq(min(delta_ev) - 20, max(delta_ev) + 20, by = 1), dosage = dosage)
  pop_pred = predict(m0, pop_pred_df, type = 'response', allow.new.levels=TRUE, se.fit = TRUE)
  pop_pred_df$pred = pop_pred$fit
  pop_pred_df$ymin = pop_pred$fit - pop_pred$se.fit
  pop_pred_df$ymax = pop_pred$fit + pop_pred$se.fit

  # plot
  df$dosage = factor(df$dosage)
  pop_pred_df$dosage = factor(pop_pred_df$dosage)
  p = ggplot(df) + theme_classic(base_size = BASE_SIZE) +
    stat_summary_bin(mapping = aes(x = delta_ev, y = choice, color = dosage), bins = 7, fun.data = bino) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
    geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) +
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(breaks = c(-60, 0, 260)) +
    annotate("text", label = sprintf('%d trials \n %d sessions', dim(df)[1], length(unique(df$sessid))), x = max(df$delta_ev)*0.65, y = 0.1, size = ANNOTATION_SIZE) +
    xlab(expression(EV[lottery]-EV[surebet])) + ylab("P(Chose Lottery)") +
    geom_ribbon(pop_pred_df, mapping = aes(x = delta_ev, ymin = ymin, ymax = ymax, fill = dosage), alpha = 0.5) +
    scale_color_manual(values = dosage_colors) + scale_fill_manual(values = dosage_colors) +
    labs(fill = 'Dose(µg)', color = 'Dose(µg)') + theme(legend.position = 'none')
  return(p)
}

#' figure_infusion_uni
#'
#' figure2c & 2f: plots psychometric curves (model prediction + binned data), color by unilateral infusion and control
#'
#' @param region = 'FOF' or 'PPC'
#'
#' @return ggplot object
figure_infusion_uni = function(region = 'FOF'){
  # figure2c & 2f: plots psychometric curves (model prediction + binned data), color by unilateral infusion and control
  if (region == 'FOF'){
    df = read.csv('csv/figure_infusion_uni_fof.csv') %>% filter(dosage != 0)
  } else{
    df = read.csv('csv/figure_infusion_uni_ppc.csv') %>% filter(dosage != 0)
  }
  control_df = read.csv('csv/figure_behavior_population.csv')
  df = df %>% filter(as.Date(trialtime) < '2020-11-19')
  df = rbind(control_df, df)
  df$infusion_side = factor(df$infusion_side)

  # Use GLM fit for plotting ribbons
  m0 = glm('choose_right ~ pro_right_delta_ev*infusion_side', df, family = binomial)
  pop_pred_df = data_grid(df, pro_right_delta_ev = seq(min(pro_right_delta_ev) - 20, max(pro_right_delta_ev) + 20, by = 1), infusion_side = infusion_side)
  pop_pred = predict(m0, pop_pred_df, type = 'response', allow.new.levels=TRUE, se.fit = TRUE)
  pop_pred_df$pred = pop_pred$fit
  pop_pred_df$ymin = pop_pred$fit - pop_pred$se.fit
  pop_pred_df$ymax = pop_pred$fit + pop_pred$se.fit

  # plot
  p = ggplot(df) + theme_classic(base_size = BASE_SIZE) +
    stat_summary_bin(mapping = aes(x = pro_right_delta_ev, y = choose_right, color = infusion_side), bins = 7, fun.data = bino) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
    geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) +
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(breaks = c(-60, 0, 260)) +
    annotate("text", label = sprintf('%d trials \n %d sessions', dim(df)[1], length(unique(df$sessid))), x = max(df$delta_ev)*0.65, y = 0.1, size = ANNOTATION_SIZE) +
    xlab(expression(EV[right]-EV[left])) + ylab("P(Chose Right)") +
    geom_ribbon(pop_pred_df, mapping = aes(x = pro_right_delta_ev, ymin = ymin, ymax = ymax, fill = infusion_side), alpha = 0.5) +
    scale_color_manual(values = side_colors) + scale_fill_manual(values = side_colors) +
    labs(fill = 'Side', color = 'Side') + theme(legend.position = 'none')
  return(p)
}

#' figure_infusion_side
#'
#' figure2d: plots psychometric curves (model prediction + binned data), color by one side infusion and control
#'
#'
#' @return ggplot object
figure_infusion_side = function(){
  # figure2d: plots psychometric curves (model prediction + binned data), color by one side infusion and control
  df = read.csv('csv/figure_infusion_side.csv') %>% filter(dosage != 0)
  control_df = read.csv('csv/figure_behavior_population.csv')
  df = rbind(control_df, df)
  df$region = factor(df$region)
  m0 = glm('choose_right ~ pro_right_delta_ev*region', df, family = binomial)
  pop_pred_df = data_grid(df, pro_right_delta_ev = seq_range(pro_right_delta_ev, by = 1), region = region)
  pop_pred = predict(m0, pop_pred_df, type = 'response', allow.new.levels=TRUE, se.fit = TRUE)
  pop_pred_df$pred = pop_pred$fit
  pop_pred_df$ymin = pop_pred$fit - pop_pred$se.fit
  pop_pred_df$ymax = pop_pred$fit + pop_pred$se.fit

  qt = quantile(df$pro_right_delta_ev, probs = seq(0, 1, 0.2))
  p = ggplot(df) + theme_classic(base_size = BASE_SIZE) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
    geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) +
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(breaks = c(-60, 0, 260)) +
    annotate("text", label = sprintf('%d trials \n %d sessions', dim(df)[1], length(unique(df$sessid))), x = max(df$delta_ev)*0.6, y = 0.1, size = ANNOTATION_SIZE) +
    xlab(expression(EV[right]-EV[left])) + ylab("P(Chose Right)") +
    #geom_pointrange(mapping = aes(x = mid_point, y = y, ymin = ymin, ymax = ymax, color = infusion_side), position = position_dodge(10)) +
    stat_summary_bin(mapping = aes(x = pro_right_delta_ev, y = choose_right, color = region), breaks = qt, fun.data = bino, geom = 'pointrange', position = position_dodge(10)) +
    geom_ribbon(pop_pred_df, mapping = aes(x = pro_right_delta_ev, ymin = ymin, ymax = ymax, fill = region), alpha = 0.5) +
    scale_color_manual(values = c('azure4', 'mediumseagreen', 'brown2')) + scale_fill_manual(values = c('azure4', 'mediumseagreen', 'brown2')) +
    labs(fill = 'Side', color = 'Side') + theme(legend.position = 'none')
  return(p)
}
