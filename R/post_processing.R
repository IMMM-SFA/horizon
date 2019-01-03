# functions for working on optimized piecewise models for all weeks and horizons

#' select_best_horizon
#'
#' @param set input set
#' @param pre_smooth logical. Is smoothing desired? (default: FALSE)
#' @description identify horizon that produces best-fit model
#' @importFrom dplyr filter mutate group_by summarise ungroup arrange
#' @importFrom purrr map map_dfr
#' @return smoothed horizons
#' @export
select_best_horizon <- function(set,
                                pre_smooth = FALSE){

  if(isFALSE(pre_smooth)){
    return(
      set %>%
        # rounding ensures longer horizons need significantly...
        # ...larger score to be selected
        mutate(r_sq = round(r_sq, 1)) %>%
        group_by(water_week) %>%
        arrange(horizon) %>%
        summarise(horizon = which.max(r_sq),
                  r_sq = max(r_sq, na.rm = T)) %>%
        ungroup() %>%
        arrange(water_week)
    )
  }

  # if pre-smoothing is applied...
  set %>%
    filter(is.na(r_sq) == F) %>%
    split(.$water_week) %>%
    map(smooth_rsq_horizon) %>%
    map_dfr(magrittr::extract, .id = "water_week") %>%
    mutate(horizon = as.integer(horizon),
           water_week = as.integer(water_week)) %>%
    mutate(r_sq = round(r_sq, 1)) %>%
    group_by(water_week) %>%
    arrange(horizon) %>%
    summarise(horizon = which.max(r_sq),
              r_sq = max(r_sq, na.rm = T)) %>%
    ungroup() %>%
    arrange(water_week)

}

# smooth_rsq_horizon
#
# simple lowess to smooth out the r_sq before picking a horizon (probably not necessary)
#
smooth_rsq_horizon <- function(x){
  lowess(x$horizon, x$r_sq, f = 0.25) %>%
    as_tibble() %>%
    rename(horizon = x, r_sq = y)
}

# remove_low_rsq
#
# sets selected horizon to 1 if r_sq is too low
#
remove_low_rsq <- function(x, rsq_cutoff){
  x %>%
    mutate(horizon = case_when(
      r_sq < rsq_cutoff ~ as.integer(1),
      r_sq >= rsq_cutoff ~ as.integer(horizon)
    ))
}

# despike
#
# removes sharp spikes
#
despike <- function(x, tolerable_diff = 6){
  x %>%
    mutate(lag_h = lag(horizon),
           lead_h = lead(horizon),
           mean_either_side = (lag_h + lead_h) / 2,
           h_despiked = case_when(
             abs(horizon - mean_either_side) > tolerable_diff ~ mean_either_side,
             abs(horizon - mean_either_side) <= tolerable_diff ~ as.double(horizon),
             is.na(mean_either_side) ~ as.double(horizon))) %>%
    mutate(horizon = h_despiked) %>%
    select(water_week, horizon, r_sq)
}

# post_smooth
#
# final smooth on selected models
#
post_smooth <- function(x, lowess_f_param = 0.1){
  lowess(x$water_week, x$horizon, f = lowess_f_param) %>%
    as_tibble() %>%
    rename(water_week = x, h_smoothed = y) %>%
    left_join(x, by = "water_week") %>%
    mutate(horizon = round(h_smoothed, 0)) %>%
    select(-h_smoothed)
}


#' select_all_models
#'
#' @param results_dir directory of results (optimized models for all weeks)
#' @param write_loc location to write results
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate rename case_when select left_join right_join
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @return list of selected models
#' @export
select_all_models <- function(results_dir, write_loc = NULL){

  list.files(results_dir, recursive = T) %>%
    .[grepl("pw_functions", .)] %>%
    map(function(x){

      dam <- strsplit(x, split = "/")[[1]][1:2] %>%
        paste(collapse = "_")


      read_csv(paste0(results_dir, "/", x),
               col_types = cols(p1 = "d",
                                p2 = "d",
                                p3 = "d",
                                p4 = "d",
                                water_week = "i",
                                horizon = "i")) %>%
        select_best_horizon() ->
        selected_models_unsmoothed

      selected_models_unsmoothed %>%
        remove_low_rsq(rsq_cutoff = 0.2) %>%
        despike() %>%
        post_smooth() %>% select(-r_sq) %>%
        rename(horizon_smth = horizon) %>%
        right_join(selected_models_unsmoothed, by = "water_week") ->
        selected_models

      if(is.null(write_loc)) return(selected_models %>%
                                      mutate(dam = !! dam))

      selected_models %>%
        write_csv(paste0(write_loc, "/", dam, ".csv"))

  })
}




