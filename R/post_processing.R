# functions for working on optimized piecewise models for all weeks and horizons

#' select_best_horizon
#'
#' @param set input set
#' @para smooth logical. Is smoothing desired? (default: FALSE)
#' @description identify horizon that produces best-fit model
#' @importFrom dplyr filter mutate group_by summarise ungroup arrange
#' @importFrom purrr map map_dfr
#' @return smoothed horizons
#' @export
select_best_horizon <- function(set, smooth = FALSE){

  if(isFALSE(smooth)){
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

  # if smoothing is applied...
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
