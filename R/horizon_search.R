#' compute_availability
#'
#' @param dam name of dam to be analyzed
#' @param water_week water week
#' @param horizon look ahead horizon (1 <= h <= 52)
#' @param max_fill_gap maximum gap (days) allowed for storage and inflow data
#' @param compute_from variable (i, r) from which to back calculate
#' @details compute availability for given week of year and horizon (in weeks)
#' @importFrom dplyr mutate filter
#' @importFrom zoo rollsum
#' @return tibble of release versus availability for given water week and horizon
#' @export
#'
compute_availability <- function(dam, water_week, horizon,
                                 max_fill_gap = 10,
                                 compute_from = "i",
                                 data_dir = NULL,
                                 cutoff_year = NULL){

  if(!(water_week %in% 1:52)) stop("water_week must be in the range 1->52")
  if(!(horizon %in% 1:52)) stop("horizon must be in the range 1->52")

  dam %>%
    read_dam(data_dir = data_dir) %>%
    convert_to_metric() %>%
    fill_NAs(max_fill_gap = max_fill_gap) %>%
    convert_to_complete_water_years() %>%
    aggregate_to_water_weeks() %>%
    back_calc_missing_flows(compute_from = compute_from) %>%
    mutate(i_sum = rollsum(i, horizon, fill = NA, align = "left"),
           a = s_start + i_sum) %>%
    filter(is.na(a) == F, is.na(r) == F) %>%
    mutate(horizon = !! horizon %>% as.integer()) %>%
    filter(water_week == !! water_week) %>%
    set_cutoff_year(cutoff_year)

}


#' optimize_piecewise_function
#'
#' @param r_a_tibble a tibble specifying release and availability for given water_week and horizon
#' @param append_r_pred append preductions of r to input tibble rather than return parameters?
#' @details runs the a local optimizer to get piecewise function
#' @importFrom nloptr nloptr
#' @importFrom zoo rollmean rollapply
#' @importFrom dplyr select mutate filter arrange
#' @importFrom tibble tibble
#' @return optimized parameters of piecewise function
#' @export
#'
optimize_piecewise_function <- function(r_a_tibble,
                                        append_r_pred = F){

  # exit routine and return NA for horizons > allowable within water year
  r_a_tibble %>% .$water_week %>% unique -> ww
  r_a_tibble %>% .$horizon %>% unique -> lead_weeks
  maximum_lead <- 53 - ww
  if(lead_weeks > maximum_lead){
    return(
      # NA values for all parameters
      c(NA, NA, NA, NA, NA)
    )
  }

  # prepare model data
  r_a_tibble %>% select(a, r) %>% arrange(a) -> mod_data
  mod_data$a %>% sort() %>% tail(3) %>% .[1] -> bp_max
  mod_data$a %>% sort() %>% .[3] -> bp_min

  get_slope <- function(r_a) {
    model <- lm(r ~ a, as.data.frame(r_a))
    coef(model)[2] %>% unname()
  }

  # estimate breakpoint using smoothed slope inflection
  mod_data %>%
    mutate(slope = c(NA,
                     rollapply(mod_data,
                               3, get_slope,
                               by.column = FALSE), NA),
           slope_smooth = rollmean(slope, 5, fill = NA),
           slope_diff = slope_smooth - lag(slope_smooth)) ->
    slopes

  slopes %>% arrange(-slope_diff) %>%
    .$a %>% .[1] ->
    bp_est_a

  # define maximum slope constraint
  #slopes %>% .$slope_smooth %>% max(na.rm = T) -> max_slope
  slopes %>% .$slope %>% max(na.rm = T) -> max_slope

  # estimate slopes from breakpoint estimate
  mod_data %>%
    filter(a <= bp_est_a) %>%
    lm(r ~ a, .) %>% coefficients() -> coef_lhs
  coef_lhs %>% .[2] %>% unname() %>% max(., 1e-3) -> slope_1_est

  mod_data %>%
    filter(a >= bp_est_a) %>%
    lm(r ~ a, .) %>% coefficients() -> coef_rhs
  coef_rhs %>% .[2] %>% unname() %>% max(., 1e-3) -> slope_2_est

  # find intersect and set as breakpoint
  tibble(r = (slope_2_est * coef_lhs[1] - slope_1_est * coef_rhs[1]) /
           (slope_2_est - slope_1_est),
         a = (coef_lhs[1] - coef_rhs[1]) / (slope_2_est - slope_1_est)) ->
    bp_est

  # function to be minimized in piecewise function optimization
  get_pw_rmse <- function(x){
    mod_data %>%
      mutate(lhs_r = (a * x[1]) + (x[3] - (x[1] * x[4])),
             rhs_r = (a * x[2]) + (x[3] - (x[2] * x[4])),
             pred_r = case_when(
               a <= x[4] ~ lhs_r,
               a > x[4] ~ rhs_r
             ),
             sq_error = (r - pred_r) ^ 2) %>%
      .$sq_error %>% mean() %>% sqrt()
  }

  # inequality constraint
  ineq_1 <- function(x){
    x[1] - x[2]
  }

  # polish with local optimizer
  nloptr(x0 = c(s1 = slope_1_est,
                s2 = slope_2_est,
                bp_r = min(max(bp_est$r, 0), max(mod_data$r)),
                bp_a = min(max(bp_est$a, bp_min), bp_max)),
         eval_f = get_pw_rmse,
         eval_g_ineq = ineq_1,
         lb = c(1e-3, 1e-3, 0, bp_min),
         ub = c(max(max_slope, slope_1_est),
                max(max_slope, slope_2_est),
                max(mod_data$r), bp_max),
         opts = list("algorithm" = "NLOPT_LN_COBYLA",
                     "xtol_rel" = 1.0e-3,
                     "maxeval" = 1000)) -> pw_model

  message(paste0("Optimization for water week ", ww, " with horizon = ", lead_weeks, " complete!"))
  flush.console()

  p <- pw_model$solution
  r_a_tibble %>%
    mutate(pred_r = case_when(
      a <= p[4] ~ (a * p[1]) + (p[3] - (p[1] * p[4])),
      a > p[4] ~ (a * p[2]) + (p[3] - (p[2] * p[4]))
    )
    ) -> mod_vs_obs

  if(isTRUE(append_r_pred)) return(mod_vs_obs)

  # return optimized parameters and associated r-squared value
  tibble(
    p1 = round(p[1], 4),
    p2 = round(p[2], 4),
    p3 = round(p[3], 4),
    p4 = round(p[4], 4),
    r_sq = round(cor(mod_vs_obs$r, mod_vs_obs$pred_r) ^ 2, 4),
    water_week = ww,
    horizon = lead_weeks
    )
}

#' get_optimized_models
#'
#' @param dam name of dam to be analyzed
#' @details gets all model parameters (and associated r-sq values)
#' @importFrom future plan multiprocess
#' @importFrom furrr future_map
#' @importFrom readr write_csv
#' @return optimized piecewise functions for all water week and horizon combinations
#' @export
#'
get_optimized_models <- function(dam, all_valid_combos,
                                 write_to = NULL,
                                 water_week = NULL, horizon = NULL,
                                 compute_from = "i",
                                 max_fill_gap = 10,
                                 write_loc = NULL,
                                 data_dir = NULL,
                                 cutoff_year = NULL){
  # set up multicore access for mapping
  plan(multiprocess)

  if(isTRUE(all_valid_combos)){
    # create a tibble of week and horizon combinations
    expand.grid(ww = 1:52,
                h = 1:30) %>%
      filter(h+ww <= 53) ->
      ww_h_combos
  }else{
    expand.grid(ww = water_week,
                h = horizon) ->
      ww_h_combos
  }

  1:nrow(ww_h_combos) %>%
    future_map(function(x){
      compute_availability(dam,
                           water_week = ww_h_combos$ww[x],
                           horizon = ww_h_combos$h[x],
                           compute_from = compute_from,
                           max_fill_gap = max_fill_gap,
                           data_dir = data_dir,
                           cutoff_year = cutoff_year)
    }) ->
    r_a_tibbles

  r_a_tibbles %>%
    future_map(optimize_piecewise_function) %>%
    bind_rows() -> optimized_piecewise_functions

  if(is.null(write_loc)) return(optimized_piecewise_functions)

  write_csv(optimized_piecewise_functions,
            paste0(write_loc, "pw_functions_", dam, ".csv"))

}


#' processes optimized models to extract
#'
#' @param file_loc
#' @details
#' @importFrom furrr future_map
#' @importFrom readr write_csv
#' @return optimized piecewise functions for all water week and horizon combinations
#' @export
#'
#'

