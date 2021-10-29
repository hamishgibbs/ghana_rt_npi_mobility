# Script to train model with data from prepare_regression_data.R
require(tidyverse)
require(splines)

# The things that change in model training:
# Different mob variables
# Different npi variables
# Different periods (fixed and increasing)
# Different lags

# A run of the model is defined by the:
# Type of mobility data
# Type of npi data
# Type of window

modelling_analyses <- list(
  list(mob_name="mob_vf",
       npi_si_name="npi_si_custom",
       period_type="varying_length"),
  list(mob_name="mob_vf",
       npi_si_name="npi_si_ox",
       period_type="varying_length"),
  list(mob_name="mob_goog",
       npi_si_name="npi_si_custom",
       period_type="varying_length"),
  list(mob_name="mob_vf",
       npi_si_name="npi_si_custom",
       period_type="fixed_length")
)

reg_data <- read_rds("data/regression/processed/regression_data.rds")

analysis <- 4

mob_name <- modelling_analyses[[analysis]][["mob_name"]]
npi_si_name <- modelling_analyses[[analysis]][["npi_si_name"]]

# Define the data with the variables that will be used for this iteration of the model
reg_data_i <- create_reg_data(reg_data, mob_name, npi_si_name)
start <- min(reg_data[["rt"]]$date)
end <- max(reg_data[["rt"]]$date)


periods <- get_periods(start, end,
                       modelling_analyses[[analysis]][["period_type"]])

#########################################################################
# Then create the L1 model (this is not lagged and not on a varied window)
reg_data_l1 <- join_lagged_data(reg_data_i["mob"][[1]], reg_data_i["npi"][[1]], c("date"), 0) %>%
  replace_na(list(npi_si = 0)) %>%
  drop_na(mob) %>% 
  # centring and scaling
  mutate(mob = (mob - mean(mob)) / sd(mob),
         npi_si = (npi_si - mean(npi_si)) / sd(npi_si))

model_l1 <- lme4::lmer("mob ~ npi_si + (1 | name2)",
                       data=reg_data_l1)


# get & lag model residuals
l1_residuals <- tibble(date = reg_data_l1$date,
                       name2 = reg_data_l1$name2,
                       res = residuals(model_l1))

reg_data_i[["l1"]] <- l1_residuals


#########################################################################
# Train model for different combinations of periods

results <- list()
reg_data_l2_store <- list()
model_l2_store <- list()
obs_pred_store <- list()
r2m <- c()
i <- 1
opt_i <- 0
for (lag_days in 1:30){
  print(lag_days)
  for (period in periods){
    reg_data_l2 <- get_reg_data_l2(period, lag_days, reg_data_i) %>%
      drop_na(res) %>%
      mutate(npi_si = (npi_si - mean(npi_si)) / sd(npi_si),
             res = (res - mean(res)) / sd(res)) %>% 
      replace_na(list(npi_si = 0))

    model_l2 <- lme4::lmer("R ~ npi_si + res + events + (1 | name2)",
                           data=reg_data_l2)

    results[[i]] <- evaluate_model_l2(i, period, lag_days, mob_name, npi_si_name, model_l2, reg_data_l2)
    model_l2_store[[i]] <- model_l2
    reg_data_l2$predictions <- predict(model_l2)
    obs_pred_store[[i]] <- reg_data_l2
    r2m_i <- MuMIn::r.squaredGLMM(model_l2)[1]
    r2m <- append(r2m, r2m_i)
    if(r2m_i == max(r2m)){opt_i = i}
    i <- i + 1
  }
}

results_data <- results %>%
  do.call(rbind, .) %>%
  mutate(iteration = row_number())

results_data %>%
  filter(r2m == max(r2m)) %>% pull(lag_days)

write_rds(results_data, paste0("data/regression/output/result_data_", analysis, ".rds"))

write_rds(model_l2_store[[opt_i]], paste0("data/regression/output/model_l2_", analysis, ".rds"))
write_rds(obs_pred_store[[opt_i]], paste0("data/regression/output/obs_pred_", analysis, ".rds"))
rm(list = ls())
gc()

