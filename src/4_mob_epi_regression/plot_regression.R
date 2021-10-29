# Plot assessment of models for different analyses

analysis <- 1
result_data <- read_rds(paste0("data/regression/output/result_data_", analysis, ".rds"))
model_l2 <- read_rds(paste0("data/regression/output/model_l2_", analysis, ".rds"))
obs_pred <- read_rds(paste0("data/regression/output/obs_pred_", analysis, ".rds"))

# Identify the optimal lag for this analysis
lag_optim <- result_data %>% 
  filter(r2m == max(r2m)) %>% pull(lag_days)

############################################################################
# Main Figure Plot

# Plot (r2m, r2c, mae) (for optimal lag)
p_perf <- result_data %>% 
  filter(lag_days == lag_optim) %>% 
  select(end, r2m, r2c, med_abs_error) %>% 
  pivot_longer(!c(end)) %>% 
  mutate(name = factor(name, levels = c("r2m", "r2c", "med_abs_error"),
         labels = c("Marginal R2", "Conditional R2", "Median Absolute Error"))) %>% 
  ggplot() + 
  geom_path(aes(x = end, y = value, color = name)) + 
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")) + 
  facet_wrap(~name, nrow = 1, scales = "free_y") + 
  labs(title = "a", y = "Model Evaluation", x = "Period end date") + 
  theme_classic() + 
  theme(legend.position = "none")

# Plot of coefficients (for optimal lag)
coefs <- result_data %>% 
  filter(lag_days == lag_optim) %>% 
  select(end, npi_coef, mob_coef, events_coef) %>% 
  pivot_longer(!c(end)) %>% 
  mutate(name = stringr::str_replace(name, "_coef", "")) %>% 
  filter(name != "events")

pvals <- result_data %>% 
  filter(lag_days == lag_optim) %>% 
  select(end, npi_pval, mob_pval, events_pval) %>% 
  pivot_longer(!c(end)) %>% 
  rename(pval = value) %>% 
  mutate(name = stringr::str_replace(name, "_pval", "")) %>% 
  filter(name != "events")

coef_data <- coefs %>% 
  left_join(pvals, by = c("end", "name"))

p_param <- coef_data %>% 
  drop_na(value) %>% 
  mutate(name = factor(name, levels = c("mob", "npi", "events"), 
                      labels = c("Mobility", "NPI", "Events"))) %>% 
  ggplot() + 
  geom_point(aes(x = end, y = value, color = name, shape = pval < 0.001),
             size = 0.5) +
  scale_shape_manual(values = c("TRUE" = 3, "FALSE" = 23)) + 
  geom_hline(aes(yintercept = 0), size = 0.2, linetype = "dashed") + 
  labs(title = "b", y = "Parameter Estimate", color = "Variable", 
       x = "Period end date") + 
  theme_classic() + 
  theme(legend.position = "right")

# Absolute error per date & district
p_ae <- obs_pred %>% 
  mutate(diff = R - predictions) %>% 
  group_by(name2, date) %>% 
  summarise(ae = abs(diff)) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = ae), size = 0.3) + 
  facet_wrap(~name2) + 
  theme_classic() + 
  labs(y = "Absolute Error", x = NULL) + 
  theme(axis.text.x = element_text(size = 0.2),
        strip.text.x = element_text(size = 7))
  
# Plot of obs vs pred
p_obs_pred <- obs_pred %>% 
  ggplot() + 
  geom_abline(linetype = "dashed", color = "blue") + 
  geom_point(aes(x = R, y = predictions), size = 0.1) + 
  theme_classic() + 
  labs(title = "c", x = expression("Observed"~R[t]), y = expression("Predicted"~R[t]))

p_row <- cowplot::plot_grid(p_param, p_obs_pred)
p <- cowplot::plot_grid(p_perf, p_row, nrow = 2)

ggutils::ggsave_png_pdf(p,
                        paste0("output/figs_regression/update/main_plot_", analysis, ".png"),
                        10, 5)

ggutils::ggsave_png_pdf(p_ae,
                        paste0("output/figs_regression/update/ae_time_name2_", analysis, ".png"),
                        12, 6)

############################################################################
# Supplement figure plots

# Plot of performance metrics at every lag
p_lag <- result_data %>% 
  mutate(period_len = as.numeric(end - start)) %>% 
  #filter(period_len == 90) %>% 
  select(end, lag_days, r2m, r2c, med_abs_error) %>% 
  pivot_longer(!c(end, lag_days)) %>% 
  mutate(name = factor(name, levels = c("r2m", "r2c", "med_abs_error"),
                       labels = c("R2 Marginal", "R2 Conditional", "Median Absolute Error"))) %>% 
  ggplot() + 
  geom_path(aes(x = end, y = value, group = lag_days, color = lag_days),
            size = 0.3) + 
  facet_wrap(~name, nrow = 1, scales = "free_y") + 
  theme_classic() + 
  labs(color = "Lag (days)", x = "Period end date", y = "Model Evaluation") + 
  xlim(min(result_data$start), max(result_data$end))

ggutils::ggsave_png_pdf(p_lag,
                        paste0("output/figs_regression/update/lag_plot_", analysis, ".png"),
                        10, 4)

  
# table of regression coefficients for highest performing model
sjPlot::tab_model(model_l2, 
                  digits = 3,
                  digits.p = 3)

# Random effect sizes of l2 model
p_re <- merTools::plotREsim(merTools::REsim(model_l2))


ggutils::ggsave_png_pdf(p_re,
                        paste0("output/figs_regression/update/re_plot_", analysis, ".png"),
                        10, 5)
 
