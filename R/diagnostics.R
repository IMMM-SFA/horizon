#' hplot_calculated_flows
#'
#' @param dam name of dam to be analyzed
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom dplyr select mutate if_else summarise
#' @return plot of i and r observed versus computed
#' @export
#'
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
                             cutoff_year = NULL,
                             add_piecewise_fn = F,
                             compute_from = "i",
                             max_fill_gap = 10,
                             opt_mode = "two_param"){

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
                           cutoff_year = cutoff_year,
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
      future_map(optimize_piecewise_function, append_r_pred = T, opt_mode = opt_mode) %>%
      bind_rows() %>%
      ggplot(aes(a, r, color = water_year)) + geom_point() +
      facet_grid(water_week ~ horizon, scales = "free", labeller = "label_both") +
      labs(title = "Release as a function of available water",
           y = expression(Release~(Mm^3)),
           x = expression(Availability~(Mm^3)),
           color = "Water year") +
      geom_line(aes(y = pred_r), color = "red") +
      theme_bw()
  }
}



#' hplot_selected_models
#'
#' @param optimized_models tibble containing all piecewise functions for all water weeks
#' @param smooth logical. Is pre-smoothing desired? (default: FALSE)
#' @import ggplot2
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate rename case_when select left_join
#' @return plot of selected horizon for all water weeks
#' @export
#'
hplot_selected_models <- function(selected_models){

  selected_models %>%
    ggplot(aes(water_week, horizon, color = r_sq)) +
    geom_point() +
    geom_line(aes(water_week, horizon), color = "black", alpha = 0.2) +
    labs(title = "Forecast use signature",
         x = "Water week",
         y = "Horizon selected (weeks)",
         color = "R-squared") +
    theme_bw()

}


#' hplot_fcast_short
#'
#' @param dam dam
#' @param mth month
#' @param horizon horizon
#' @param max_fill_gap maximum number of NA days to fill across
#' @param data_dir data directory
#' @param cutoff_year cutoff year
#' @param min_allowable_points minimum allowable points for model build
#' @details plot short forecast model
#' @import ggplot2
#' @importFrom modelr add_predictions
#' @importFrom purrr map_dfr
#' @return plot of selected horizon for all water weeks
#' @export
#'
hplot_fcast_short <- function(dam, mth, horizon,
                              max_fill_gap = 3,
                              data_dir = NULL,
                              cutoff_year = NULL,
                              min_allowable_points = 5){

  compute_fcast_short(dam = dam, mth = mth, horizon = horizon,
                      max_fill_gap = max_fill_gap,
                      data_dir = data_dir,
                      cutoff_year = cutoff_year) %>%
    filter(!is.na(r), !is.na(i_sum)) %>%
    split(.$op_week) %>%
    map_dfr(function(x){
      x %>%
        add_predictions(lm(r ~ i_sum, data = .))
    }) %>% mutate(year = year(date)) %>%
    ggplot(aes(i_sum, r, group = op_week)) + geom_point(alpha = 0.7) +
    geom_line(aes(y = pred)) +
    facet_wrap(~year, scales = "free") +
    theme_bw() + theme(strip.background = element_blank()) +
    labs(title = paste0(dam, " ", month.name[mth], " (h =  ", horizon, " days)" ))
}


#' hplot_fcast_short_gof
#'
#' @param dam dam
#' @param max_fill_gap maximum number of NA days to fill across
#' @param data_dir data directory
#' @param cutoff_year cutoff year
#' @param min_allowable_points minimum allowable points for model build
#' @details plot r2 resulrs for all models
#' @import ggplot2
#' @importFrom modelr add_predictions add_residuals
#' @importFrom purrr pmap pmap_dfr
#' @importFrom tidyr gather
#' @importFrom cowplot plot_grid
#' @return plot of selected horizon for all water weeks
#' @export
#'
hplot_fcast_short_gof <- function(dam,
                                  max_fill_gap = 3,
                                  data_dir = NULL,
                                  cutoff_year = NULL,
                                  min_allowable_points = 5,
                                  method = "spearman", min_score = 0.7){
  compute_fcast_short_scores(dam = dam,
                      max_fill_gap = max_fill_gap,
                      data_dir = data_dir,
                      cutoff_year = cutoff_year,
                      method = method) ->
    scores_all_weeks_and_horizons

  scores_all_weeks_and_horizons %>%
    mutate(score = round(score, 1)) %>%
    group_by(op_week) %>% filter(score == max(score)) %>%
    summarise(horizon = first(horizon), score = first(score), s = mean(s), i = mean(i)) %>%
    ungroup() %>%
    filter(score >= min_score) %>%
    mutate(month = month(op_week, label = TRUE)) ->
    best_fit_horizons

  best_fit_horizons %>%
    group_by(month) %>%
    count(horizon) %>% mutate(perc = 100 * n / sum(n)) %>% ungroup() %>%
    mutate(horizon = factor(horizon, levels = 1:7)) ->
    horizons_binned

  horizons_binned %>%
    ggplot(aes(month)) + geom_col(aes(fill = horizon, y = perc), col = "black") +
    geom_text(data = group_by(horizons_binned, month) %>%
                summarise(n = paste0("n = ", sum(n))), aes(y = 100, label = n, vjust = -1)) +
    theme_void() + scale_fill_viridis(discrete = T, option = "E", direction = 1) +
    theme(axis.text.x = element_text(size = 14, vjust = 1),
          legend.position = "bottom", legend.direction = "horizontal",
          plot.margin = margin(10, 10, 10, 10, "pt")) +
    labs(title = dam, fill = "Forecast horizon (days)",
         subtitle = "") -> distribution_of_best_fit_horizons

  distribution_of_best_fit_horizons

  # best_fit_horizons %>%
  #   mutate(season = case_when(
  #     month %in% c("Jul", "Aug", "Sep") ~ "summer",
  #     month %in% c("Oct", "Nov", "Dec") ~ "fall",
  #     month %in% c("Jan", "Feb", "Mar") ~ "winter",
  #     month %in% c("Apr", "May", "Jun") ~ "spring")) %>%
  #   mutate(horizon = factor(horizon, levels = 1:7)) %>%
  #   mutate(season = factor(season, levels = c("spring", "summer", "fall", "winter"))) %>%
  #   ggplot(aes(horizon, s, group = horizon)) +
  #   theme_void() + #scale_fill_viridis(discrete = T) +
  #   geom_jitter(pch = 21, size = 2, width = 0.3, aes(fill = season)) +
  #   geom_boxplot(outlier.shape = NA, alpha = 0) +
  #   theme(legend.position = "bottom",
  #         axis.text.x = element_text(size = 14, vjust = 1)) +
  #   labs(title = "Influece of storage on forecast use") -> storage_plot
  #
  # plot_grid(distribution_of_best_fit_horizons, storage_plot, ncol = 1)

  # best_fit_horizons %>%
  #   mutate(season = case_when(
  #     month %in% c("Jul", "Aug", "Sep") ~ "summer",
  #     month %in% c("Oct", "Nov", "Dec") ~ "fall",
  #     month %in% c("Jan", "Feb", "Mar") ~ "winter",
  #     month %in% c("Apr", "May", "Jun") ~ "spring")) %>%
  #   mutate(horizon = factor(horizon, levels = 1:7)) %>%
  #   mutate(season = factor(season, levels = c("spring", "summer", "fall", "winter"))) %>%
  #   ggplot(aes(horizon, i, group = horizon)) +
  #   theme_void() + #scale_fill_viridis(discrete = T) +
  #   geom_jitter(pch = 21, size = 2, width = 0.3, aes(fill = season)) +
  #   geom_boxplot(outlier.shape = NA, alpha = 0) +
  #   theme(legend.position = "bottom",
  #         axis.text.x = element_text(size = 14, vjust = 1)) +
  #   labs(title = "Influece of storage on forecast use") -> inflow_plot
  #
  # plot_grid(distribution_of_best_fit_horizons, inflow_plot, ncol = 1)


  }
