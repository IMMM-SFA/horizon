#' compute_availability
#'
#' @param dam name of dam to be analyzed
#' @param water_week water week
#' @param horizon look ahead horizon (1 <= h <= 52)
#' @param max_fill_gap maximum gap (days) allowed for storage and inflow data
#' @param compute_from variable (i, r) from which to back calculate
#' @param min_allowable_points minimum number of points for building r-a function (default = 10)
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
                                 cutoff_year = NULL,
                                 min_allowable_points = 10){

  if(!(water_week %in% 1:52)) stop("water_week must be in the range 1->52")
  if(!(horizon %in% 1:52)) stop("horizon must be in the range 1->52")

  dam %>%
    read_dam(data_dir = data_dir) %>%
    convert_to_metric() %>%
    fill_NAs(max_fill_gap = max_fill_gap) %>%
    #convert_to_complete_water_years() %>%
    convert_to_water_years() %>%
    aggregate_to_water_weeks() %>%
    back_calc_missing_flows(compute_from = compute_from) %>%
    mutate(i_sum = rollsum(i, horizon, fill = NA, align = "left"),
           a = s_start + i_sum) %>%
    mutate(horizon = !! horizon %>% as.integer()) %>%
    filter(water_week == !! water_week) %>%
    set_cutoff_year(cutoff_year) %>%
    filter(!is.na(r),
           !is.na(a)) -> release_and_availability

  if(nrow(release_and_availability) >= min_allowable_points){
    return(release_and_availability)
  }else{
    stop(paste0("Bad data... too few points (",
                nrow(release_and_availability), ") for building release availability function... ",
                "water_week = ", water_week, "; horizon = ", horizon, "; dam = ", dam))
  }

}


#' optimize_piecewise_function
#'
#' @param r_a_tibble a tibble specifying release and availability for given water_week and horizon
#' @param append_r_pred append preductions of r to input tibble rather than return parameters?
#' @param opt_mode set as "two_param" or "four_param" (i.e., with slopes) optimization
#' @details runs the a local optimizer to get piecewise function
#' @importFrom nloptr nloptr
#' @importFrom zoo rollmean rollapply
#' @importFrom dplyr select mutate filter arrange
#' @importFrom tibble tibble
#' @return optimized parameters of piecewise function
#' @export
#'
optimize_piecewise_function <- function(r_a_tibble,
                                        append_r_pred = F,
                                        opt_mode){

  # exit routine and return NA for horizons > allowable within water year (deprecated!)
  r_a_tibble %>% .$water_week %>% unique -> ww
  r_a_tibble %>% .$horizon %>% unique -> lead_weeks
  # maximum_lead <- 53 - ww
  # if(lead_weeks > maximum_lead){
  #   return(
  #     # NA values for all parameters
  #     c(NA, NA, NA, NA, NA)
  #   )
  # }

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
      .$sq_error %>% mean() %>% sqrt(.)
  }

  # v2 has the slope constraint inside the function (i.e. slopes are not decision variables)
  get_pw_rmse_v2 <- function(x){

    mod_data[which(mod_data$a < x[2]), ] -> lhs
    mod_data[which(mod_data$a >= x[2]), ] -> rhs

    lm(I(lhs$r - x[1]) ~ 0 + I(lhs$a - x[2]))$coef -> lhs_s
    lm(I(rhs$r - x[1]) ~ 0 + I(rhs$a - x[2]))$coef -> rhs_s

    if(lhs_s < 1e-3) lhs_s <- 1e-3
    if(rhs_s < lhs_s) rhs_s <- lhs_s

    mod_data %>%
      mutate(lhs_r = (a * lhs_s) + (x[1] - (lhs_s * x[2])),
             rhs_r = (a * rhs_s) + (x[1] - (rhs_s * x[2])),
             pred_r = case_when(
               a <= x[2] ~ lhs_r,
               a > x[2] ~ rhs_r
             ),
             sq_error = (r - pred_r) ^ 2) %>%
      # ggplot(aes(a, r)) + geom_point() +
      # geom_point(aes(y = pred_r), col = "blue")
      .$sq_error %>% mean() %>% sqrt(.)


    # mod_data %>%
    #   mutate(a_ = a - x[2],
    #          side = if_else(a_ < 0, "lhs", "rhs")) %>%
    #   split(.$side) %>%
    #   map(function(side){
    #     side %>%
    #       add_predictions(lm(I(.$r - x[1]) ~ 0 + .$a_)) %>%
    #       mutate(pred = pred + x[1])
    #   }) %>%
    #   bind_rows() -> dip  # data and initial predictions
    #
    # # constrain slopes if needed
    # filter(dip, side == "lhs") -> dip_lhs
    # filter(dip, side == "rhs") -> dip_rhs
    #
    # slope_lhs <- dip_lhs$pred[2] - dip_lhs$pred[1] / dip_lhs$r[2] - dip_lhs $r[1]
    # slope_rhs <- dip_rhs$pred[2] - dip_rhs$pred[1] / dip_rhs$r[2] - dip_rhs $r[1]
    #
    # if(slope_lhs < 0 | slope_rhs <0){
    #   dip %>%
    #     mutate(pred_ = case_when(
    #       side == "lhs" & slope_lhs < 0 ~ x[1],
    #       side == "lhs" & slope_lhs >=0 ~ pred,
    #       side == "rhs" & slope_rhs < 0 ~ x[1],
    #       side == "rhs" & slope_rhs >= 0 ~ pred
    #     )) -> dip
    #   # ggplot(aes(a, r)) + geom_point() +
    #   # geom_point(aes(y = pred_), col = "blue")
    # dip%>%
    #   mutate(sq_error = (r - pred) ^ 2) %>%
    #   .$sq_error %>% mean() %>% sqrt(.)
  }

  # inequality constraint
  ineq_1 <- function(x){
    x[1] - x[2]
  }

  # polish with local optimizer
  if(opt_mode == "four_param"){
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

    p <- pw_model$solution

  }else if(opt_mode == "two_param"){
    # v2: only optimize breakpoint--compute and constrain slope within function
    nloptr(x0 = c(bp_r = min(max(bp_est$r, 0), max(mod_data$r)),
                  bp_a = min(max(bp_est$a, bp_min), bp_max)),
           eval_f = get_pw_rmse_v2,
           lb = c(0, bp_min),
           ub = c(max(mod_data$r), bp_max),
           opts = list("algorithm" = "NLOPT_LN_BOBYQA",
                       "xtol_rel" = 1.0e-3,
                       "maxeval" = 1000)) -> pw_model

    p_ <- pw_model$solution
    mod_data[which(mod_data$a < p_[2]), ] -> lhs
    mod_data[which(mod_data$a >= p_[2]), ] -> rhs
    max(lm(I(lhs$r - p_[1]) ~ 0 + I(lhs$a - p_[2]))$coef, 1e-3) -> lhs_s
    max(lm(I(rhs$r - p_[1]) ~ 0 + I(rhs$a - p_[2]))$coef, lhs_s) -> rhs_s

    p <- c(lhs_s, rhs_s, pw_model$solution)

  }

  message(paste0("Optimization for water week ", ww, " with horizon = ", lead_weeks, " complete!"))
  flush.console()


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
                                 allow_fcast_across_water_yr = TRUE,
                                 write_to = NULL,
                                 water_week = NULL, horizon = NULL,
                                 compute_from = "i",
                                 max_fill_gap = 10,
                                 max_horizon = 30,
                                 write_loc = NULL,
                                 data_dir = NULL,
                                 cutoff_year = NULL,
                                 opt_mode = "two_param"){
  # set up multicore access for mapping
  plan(multiprocess)

  if(isTRUE(all_valid_combos)){
    expand.grid(ww = 1:52,
                h = 1:max_horizon) ->
      ww_h_combos
    if(isFALSE(allow_fcast_across_water_yr)){
      ww_h_combos <- ww_h_combos %>% filter(h+ww <= 53)
    }
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
    future_map(optimize_piecewise_function, opt_mode = opt_mode) %>%
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

