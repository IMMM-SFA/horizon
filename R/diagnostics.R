#' hplot_check_raw_daily_obs
#'
#'
#'
hplot_check_raw_daily_obs <- function(dam){

  dam %>%
    read_dam()
}


#' hplot_calculated_flows
#'
#' @param dam name of dam to be analyzed
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom dplyr select mutate if_else summarise
#' @return plot of i and r observed versus computed
#' @export
hplot_calculated_flows <- function(dam, plot_years = NULL){

  dam %>%
    read_dam() %>%
    convert_to_metric() %>%
    fill_NAs() %>%
    convert_to_complete_water_years() %>%
    aggregate_to_water_weeks() %>%
    select(water_year, water_week, i, i_, r, r_) %>%
    gather(variable, flow, -water_year, -water_week) %>%
    mutate(type = if_else(grepl("_", variable), "calculated", "observed"),
           variable = substr(variable, 1, 1)) ->
    obs_calc_flows

  if(is.null(plot_years)){
    obs_calc_flows -> obs_calc_flows_plot
  }else{
    obs_calc_flows %>% filter(water_year %in% plot_years) ->
      obs_calc_flows_plot
  }

  obs_calc_flows_plot %>%
    ggplot(aes(water_week, flow, color = type)) +
    geom_line() +
    facet_grid(water_year~variable) +
    labs(title = "Obs vs Calc flow and release (Mm^3)",
         subtitle = "Computed using change in weekly storage",
         y = NULL, x = "Water week")
}


#' hplot_ready_data
#'
#'@param dam name of dam to be analyzed
#'@import ggplot2
#'@importFrom dplyr bind_rows
#'@importFrom furrr future_map
#'@importFrom future plan multiprocess
#'@return plot of r as function of availability
#'@export
#'
hplot_ready_data <- function(dam, water_week, horizon,
                             add_piecewise_fn = F,
                             compute_from = "i",
                             max_fill_gap = 10){

  # set up multicore access for mapping
  plan(multiprocess)

  # ensure logical add_piecewise_fn
  if(!is.logical(add_piecewise_fn)) stop("add_piecewise_fn must be TRUE or FALSE")

  # create a tibble of week and horizon combinations
  expand.grid(ww = water_week,
              h = horizon) ->
    ww_h_combos

  # calculate availability for all cases

  1:nrow(ww_h_combos) %>%
    future_map(function(x){
      compute_availability(dam,
                           water_week = ww_h_combos$ww[x],
                           horizon = ww_h_combos$h[x],
                           compute_from = compute_from,
                           max_fill_gap = max_fill_gap)
    }) ->
    r_a_tibbles

  # combine to single tibble and plot

  if(isFALSE(add_piecewise_fn)){
    r_a_tibbles %>%
      bind_rows() %>%
      ggplot(aes(a, r, color = water_year)) + geom_point() +
      facet_grid(water_week ~ horizon, scales = "free", labeller = "label_both") +
      labs(title = "Release as a function of available water",
           y = "Relase (Mm^3)",
           x = "Availability (Water in storage + inflow[1:h]) (Mm^3)",
           color = "Water year")
  }else{
    r_a_tibbles %>%
      future_map(optimize_piecewise_function, append_r_pred = T) %>%
      bind_rows() %>%
      ggplot(aes(a, r, color = water_year)) + geom_point() +
      facet_grid(water_week ~ horizon, scales = "free", labeller = "label_both") +
      labs(title = "Release as a function of available water",
           y = "Relase (Mm^3)",
           x = "Availability (Water in storage + inflow[1:h]) (Mm^3)",
           color = "Water year") +
      geom_line(aes(y = pred_r), color = "red")
  }
}


#' hplot_model_performances
#'
#' @param file_loc file containing piecewise functions to be plotted
#' @param smoothing vector of values in order r_sq cutoff, smoothing_span, "", ""
#' @import ggplot2
#' @importFrom readr read_csv
#' @return plots of horizon and associated r_sq for all weeks
#' @export
#'
hplot_model_performances <- function(file_loc,
                                     smoothing = "x_x_x_x"){

}
