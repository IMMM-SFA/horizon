#' get_optimized_models_daily
#'
#' @param dam name of dam to be analyzed
#' @details gets all model parameters (and associated r-sq values)
#' @importFrom future plan multiprocess
#' @importFrom furrr future_map
#' @importFrom readr write_csv
#' @return optimized piecewise functions for all water week and horizon combinations
#' @export
#'
get_optimized_models_daily <- function(dam, all_valid_combos,
                                       write_to = NULL,
                                       mth = NULL, horizon = NULL,
                                       max_fill_gap = 10,
                                       max_horizon = 7,
                                       write_loc = NULL,
                                       data_dir = NULL,
                                       cutoff_year = NULL,
                                       opt_mode = "two_param"){
  # set up multicore access for mapping
  plan(multiprocess)

  if(isTRUE(all_valid_combos)){
    expand.grid(m = 1:12,
                h = 1:max_horizon) ->
      ww_h_combos
  }else{
    expand.grid(m = mth,
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


#' compute_fcast_short
#'
#' @param dam name of dam to be analyzed
#' @param water_week water week
#' @param horizon look ahead horizon (1 <= h <= 52)
#' @param max_fill_gap maximum gap (days) allowed for storage and inflow data
#' @param compute_from variable (i, r) from which to back calculate
#' @param min_allowable_points minimum number of points for building r-a function (default = 10)
#' @details compute availability for given week of year and horizon (in weeks)
#' @importFrom dplyr mutate filter row_number n first last one_of
#' @importFrom zoo rollsum
#' @importFrom lubridate month
#' @importFrom purrr map_dfr
#' @return tibble of release versus availability for given water week and horizon
#' @export
#'
compute_fcast_short <- function(dam,
                                max_fill_gap = 3,
                                data_dir = NULL,
                                cutoff_year = NULL,
                                min_allowable_points = 10){


  dam %>%
    read_dam(data_dir = data_dir) %>%
    convert_to_metric() %>%
    # remove duplicates (mainly for cdec)
    group_by(date) %>% summarise(s = first(s), i = first(i), r = first(r)) %>%
    filter(!is.na(date)) %>%
    # add in any missing rows
    right_join(tibble(date = seq.Date(first(.[["date"]]), last(.[["date"]]), by = "days")),
               by = "date") %>%
    fill_NAs(max_fill_gap = max_fill_gap) %>%
    convert_to_water_years() %>%
    set_cutoff_year(cutoff_year) %>%
    mutate(i1 = rollsum(i, 1, fill = NA, align = "left"),
           i2 = rollsum(i, 2, fill = NA, align = "left"),
           i3 = rollsum(i, 3, fill = NA, align = "left"),
           i4 = rollsum(i, 4, fill = NA, align = "left"),
           i5 = rollsum(i, 5, fill = NA, align = "left"),
           i6 = rollsum(i, 6, fill = NA, align = "left"),
           i7 = rollsum(i, 7, fill = NA, align = "left")) ->
    forecasts_1_to_7_days

  forecasts_1_to_7_days %>%
    mutate(weekday = wday(date, label = T),
           op_week = if_else(weekday == "Mon", as.character(date), NA_character_)) %>%
    tidyr::fill(op_week, .direction = "down") %>%
    filter(!is.na(i7), !is.na(op_week), !is.na(r)) %>%
    group_by(op_week) %>%
    filter(row_number() %in% 1:7, n() == 7) %>% ungroup() %>%
    select(-water_year, -date, i)
}

#' compute_fcast_short_scores
#'
#' @param dam name of dam to be analyzed
#' @param water_week water week
#' @param horizon look ahead horizon (1 <= h <= 52)
#' @param max_fill_gap maximum gap (days) allowed for storage and inflow data
#' @param compute_from variable (i, r) from which to back calculate
#' @param min_allowable_points minimum number of points for building r-a function (default = 10)
#' @details compute availability for given week of year and horizon (in weeks)
#' @importFrom dplyr mutate filter row_number n first last one_of
#' @importFrom zoo rollsum
#' @importFrom lubridate month
#' @importFrom purrr map_dfr
#' @return tibble of release versus availability for given water week and horizon
#' @export
#'
compute_fcast_short_scores <- function(dam,
                                max_fill_gap = 3,
                                data_dir = NULL,
                                cutoff_year = NULL,
                                min_allowable_points = 10,
                                method = "spearman"){

  compute_fcast_short(dam = dam,
                      max_fill_gap = max_fill_gap,
                      data_dir = data_dir,
                      cutoff_year = cutoff_year) %>%
    split(.$op_week) %>%
    map_dfr(function(x){
      1:7 %>% map_dfr(function(horizon){
        x %>% select(i = one_of(paste0("i", horizon)), r) -> obs
        if(var(obs$r) == 0){
          score = NA_real_
        }else{
          score = cor(obs$i, obs$r, method = method)
        }
        tibble(horizon = !! horizon, score = !! score)
      }) %>% mutate(op_week = x[["op_week"]], s = x[["s"]], i = x[["i"]])
    })
}

