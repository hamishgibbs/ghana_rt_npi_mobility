lag_data <- function(data_to_lag, lag_days){
  
  return(data_to_lag %>% mutate(date = date + lag_days))
  
}

join_data <- function(in_data, join_data, join_cols){
  in_data <- in_data %>% 
    left_join(join_data, by = join_cols)
  
  return(in_data)
}

join_lagged_data <- function(in_data, join_data, join_cols, lag_days){
  
  join_data <- lag_data(join_data, lag_days)
  
  in_data <- join_data(in_data, join_data, join_cols)
  
  return(in_data)
  
}

get_model_r2 <- function(model){
  model_r2 <- MuMIn::r.squaredGLMM(model)
  return(model_r2)
}

get_fixed_periods <- function(start, end, fixed_lengths=c(30, 60, 90)){
  # Get fixed - length periods between dates
  
  study_dates <- c(start, end)
  study_length <- as.numeric(diff(study_dates))
  
  periods_fixed <- list()
  a <- 1
  for (fixed_length in fixed_lengths){
    for (i in 1:study_length){
      start_date <- study_dates[1] + i
      end_date <- start_date + fixed_length
      if (end_date <= study_dates[2]){
        periods_fixed[[a]] <- list(
          start = start_date, end = end_date, type = "fixed_length"
        ) 
        a <- a + 1
      }
    }
  }
  return(periods_fixed)
}

get_varying_periods <- function(start, study_length){
  # Get varying - length periods from a start date
  
  periods_varying <- list()
  a <- 1
  for (i in 1:study_length){
    if (((start + i) - start) > 10){
      periods_varying[[a]] <- list(start = start, end = start + i,
                                   type = "varying_length") 
      a <- a + 1
    }
  }
  return(periods_varying)
}

get_periods <- function(start, end, type = c("both")){
  # Return fixed, varying, or both types of period between a start and end date
  
  periods_fixed <- get_fixed_periods(start, end)
  periods_varying <- get_varying_periods(start, as.numeric(end - start))
  
  if (type == c("both")) {
    periods <- list()
    periods[1:length(periods_fixed)] <- periods_fixed
    periods[length(periods_fixed) + 1:length(periods_varying)] <- periods_varying
    return(periods)
  } else if (type == c("fixed_length")) {
    return(periods_fixed)
  } else if (type == c("varying_length")) {
    return(periods_varying)
  }
  
}

create_reg_data <- function(reg_data, mob_name, npi_si_name){
  # Selects a single mobility & npi variable to use
  
  reg_data[["mob"]] <- reg_data[["mob"]] %>% 
    select(date, name2, !!sym(mob_name)) %>% 
    rename(mob = !!sym(mob_name))
  
  reg_data[["npi"]] <- reg_data[["npi"]] %>% 
    select(date, !!sym(npi_si_name)) %>% 
    rename(npi_si = !!sym(npi_si_name))
  
  return(reg_data)
  
}


get_reg_data_l2 <- function(period, lag_days, reg_data){
  # Get model data for a single period for a single lag
  
  reg_data_l2 <- join_lagged_data(reg_data[["rt"]], reg_data[["npi"]], 
                                  c("date"), lag_days) %>% 
    filter(date >= period$start & date <= period$end) %>% 
    join_lagged_data(., reg_data[["events"]], c("date"), lag_days) %>% 
    replace_na(list(npi_si = 0, 
                    events = 0)) %>% 
    join_lagged_data(., reg_data[["l1"]], c("date", "name2"), lag_days)
  
  return (reg_data_l2)
  
}

evaluate_model_l2 <- function(i, period, lag_days, mob_name, npi_si_name, model_l2, reg_data_l2){
  # Record all model evaluation metrics in a single df
  
  # Here: calculate median absolute error
  med_abs_error <- median(abs(predict(model_l2) - reg_data_l2$R))
  r2_res <- get_model_r2(model_l2)
  l2_coef <- coef(model_l2)$name2
  npi_coef_val = tryCatch(as.numeric(unique(l2_coef["npi_si"])), error=function(x){return(NA)})
  mob_coef_val = tryCatch(as.numeric(unique(l2_coef["res"])), error=function(x){return(NA)})
  events_coef_val = tryCatch(as.numeric(unique(l2_coef["events"])), error=function(x){return(NA)})
  
  model_l2_summary <- summary(model_l2)
  
  p_vals <- 2 * (1 - pnorm(abs(model_l2_summary$coefficients[,3])))
  names(p_vals) <- names(model_l2_summary$coefficients[,1])
  
  npi_pval = tryCatch(as.numeric(p_vals["npi_si"]), error=function(x){return(NA)})
  mob_pval = tryCatch(as.numeric(p_vals["res"]), error=function(x){return(NA)})
  events_pval = tryCatch(as.numeric(p_vals["events"]), error=function(x){return(NA)})
  
  
  return (tibble(i,
                 lag_days, 
                 med_abs_error,
                 r2m = r2_res[1], 
                 r2c = r2_res[2],
                 start = period$start,
                 end = period$end,
                 period_type = period$type,
                 mob_name = mob_name,
                 npi_si_name = npi_si_name,
                 npi_coef = npi_coef_val,
                 mob_coef = mob_coef_val,
                 events_coef = events_coef_val,
                 npi_pval = npi_pval,
                 mob_pval = mob_pval,
                 events_pval = events_pval))
}

