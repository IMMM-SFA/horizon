# functions for data preparation

#' read_dam
#'
#' @param dam name of dam to be loaded into the environment
#' @details reads in observed data as tibble
#' @importFrom readr read_csv cols
#' @return tibble of observed dam data (storage, inflow, release)
#' @export
read_dam <- function(dam, data_dir){

  if(missing(data_dir)){
    data_dir <- system.file("extdata/", package = "horizon")
  }

  data_source <- strsplit(dam, "_")[[1]][1]
  data_fn <- strsplit(dam, "_")[[1]][2]
  read_csv(paste0(data_dir, data_source, "/processed/", data_fn, ".csv"),
           col_types = cols(date = "D",
                            s_af = "d",
                            i_cfs = "d",
                            r_cfs = "d"))
}


#' convert_to_metric
#'
#' @param x tibble to be converted
#' @details converts daily data (s in af; i, r in cfs) metric volumes (Mm^3)
#' @importFrom dplyr mutate select
#' @return daily reservoir data in metric volumes (Mm^3)
#' @export
convert_to_metric <- function(x){

  x %>%
    mutate(s = s_af * af_to_Mm3,
           i = i_cfs * cfs_to_Mm3day,
           r = r_cfs * cfs_to_Mm3day) %>%
    select(date, s, i, r)
}


#' fill_NAs
#'
#' @param x tibble to be gap-filled
#' @param max_fill_gap maximum gap (days) allowed for storage and inflow data
#' @importFrom dplyr mutate select if_else
#' @importFrom zoo na.spline
#' @return daily, gap-filled reservoir data in metric volumes (Mm^3)
#' @export
fill_NAs <- function(x, max_fill_gap = 10){

  x %>%
    mutate(s = if_else(s <= 0, NA_real_, s),
           i = if_else(i <= 0, NA_real_, i),
           r = if_else(r < 0, NA_real_, r)) ->
    x_zero_fix

  all(is.na(x_zero_fix$s)) -> s_all_NA
  all(is.na(x_zero_fix$i)) -> i_all_NA
  all(is.na(x_zero_fix$r)) -> r_all_NA

  if(s_all_NA) stop("no data for storage")

  if(i_all_NA & r_all_NA) stop("no data for inflows or releases")

  if(i_all_NA){
    i_fill <- x_zero_fix$i
  }else{
    i_fill <- round(na.spline(x_zero_fix$i, maxgap = max_fill_gap, na.rm = F), 3)
  }

  if(r_all_NA){
    r_fill <- x_zero_fix$r
  }else{
    r_fill <- round(na.spline(x_zero_fix$r, maxgap = max_fill_gap, na.rm = F), 3)
  }


  x_zero_fix %>%
    mutate(s = round(na.spline(s, maxgap = max_fill_gap, na.rm = F), 3),
           i = i_fill,
           r = r_fill)

}


# remove_incomplete_water_years
#
# mapped in filter_invalid_years to remove years with incomplete data
#
remove_incomplete_water_years <- function(x){
  if(nrow(x) < 365) return(x %>% filter(water_year == "return a blank tibble"))
  # return a blank if water contains invalid entries
  x %>%
    mutate(viable = if_else(
      is.na(i) & is.na(r), FALSE, TRUE
    )) %>%
    mutate(viable = if_else(is.na(s), FALSE, viable)) ->
    x_
  if(x_ %>% filter(viable == FALSE) %>% nrow() > 1){
    return(x %>% filter(water_year == "return a blank tibble"))
  }
  x
}


#' convert_to_complete_water_years
#'
#' @param x tibble to be filtered
#' @import lubridate
#' @importFrom dplyr mutate filter select bind_rows case_when
#' @importFrom purrr map
#' @return filtered data in water years
#' @export
#'
convert_to_complete_water_years <- function(x){
  x %>%
    mutate(month = month(date),
           year = year(date),
           water_year = case_when(
             month >= 10 ~ year + 1,
             month < 10 ~ year
           )) %>%
    select(water_year, date, s, i, r) %>%
    split(.$water_year) %>%
    map(remove_incomplete_water_years) %>%
    bind_rows()
}


# get_weekly_res_data
#
# aggregation to water weeks; mapped across all water years in aggregate_to_water_weeks
#
get_weekly_res_data <- function(x){
  x %>% arrange(date) %>%
    mutate(water_week = rep(1:53, each = 7, length.out = nrow(.))) %>%
    group_by(water_year, water_week) %>%
    summarise(i = sum(i),
              r = sum(r),
              s_start = first(s)) %>%
    ungroup() %>%
    mutate(s_end = lead(s_start, 1),
           s_change = s_end - s_start,
           r_ = i - s_change,
           r_ = if_else(r_ < 0, 0, r_),
           i_ = r + s_change) %>%
    filter(is.na(s_change) == F)
}


#' aggregate_to_water_weeks
#'
#' @param x tibble to be aggregated
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @return data in water weeks
#' @export
#'
aggregate_to_water_weeks <- function(x){
  x %>%
    split(.$water_year) %>%
    map(get_weekly_res_data) %>%
    bind_rows()
}


#' back_calc_missing_flows
#'
#' @param x tibble of s, i, r in water weeks
#' @param compute_from variable (i, r) from which to back calculate
#' @details assumes no losses (evap., seepage, etc.)
#' @importFrom dplyr mutate if_else select rename filter
#' @return data ready for horizon search
#' @export
#'
back_calc_missing_flows <- function(x, compute_from = "i"){

  # catch incorrect compute_from entry
  if(!(compute_from %in% c("i", "r"))) stop("compute_from must be \"r\" or \"i\"")

  # if inflow (i) AND release (r) observations are available, then ...
  # ... we can use either to back-calculate.
  # Using both sets of observations is viable, but mass balance would not be preserved

  if(compute_from == "i"){
    x %>%
      mutate(i = if_else(is.na(i) & !is.na(r), i_, i),
             r = if_else(is.na(r_), r, r_)) %>%
      select(water_year, water_week, i, r, s_start) -> res_data_weekly
  }

  if(compute_from == "r"){
    x %>%
      mutate(r = if_else(is.na(r) & !is.na(i), r_, r),
             i = if_else(is.na(i_), i, i_)) %>%
      select(water_year, water_week, i, r, s_start) ->
      res_data_weekly
      # ... and filter for desired number of training years
      # filter(water_year %in% tail(unique(.$water_year),
      #                             training_yrs_desired)) ->
  }

  # if(sum(is.na(res_data_weekly$r)) >= 1){
  #   stop("NA release values in training data")
  # }
  #
  # if(sum(is.na(res_data_weekly$i)) >= 1){
  #   stop("NA inflow values in training data")
  # }
  #
  # if(sum(is.na(res_data_weekly$s_start)) >= 1){
  #   stop("NA s_start values in training data")
  # }
  return(res_data_weekly)
}




