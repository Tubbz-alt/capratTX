#' simulate_gcm_inflows_all_dams
#' @details ...
#' @param res_filepath full path to reservoir data file for ERCOT
#' @param period either "baseline" or "future"
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate group_by summarise
#' @importFrom lubridate year month
#' @export
#'
simulate_gcm_inflows_all_dams <- function(res_filepath, period){
  read_ERCOT_data(res_filepath)[["storage_levels"]] %>%
    .[["Look Up Name for Reservoir"]] %>%
    unique() -> reservoir_shortnames

  vroom::vroom("../parameters.csv") -> params

  reservoir_shortnames %>%
    .[!grepl("Amistad", .)] %>%
    .[!grepl("STP", .)] %>%
    map_dfr(function(x){

      # calibrate_reservoir_model(x,
      #                           res_filepath = res_filepath,
      #                           plot = FALSE,
      #                           output = "parameters") -> parameters



      read_ERCOT_data(res_filepath)[["reservoir_info"]] %>%
        filter(`Look Up Name for Reservoir` == x) %>%
        .[["Maximum storage level (Acre-ft)"]] * af_to_mcm ->
        storage_capacity_reported

      simulate_gcm_inflows(res_filepath = res_filepath,
                           reservoir = x,
                           period = period,
                           parameters = params) %>%
        mutate(s_cap = storage_capacity_reported)

    })
}

#' simulate_gcm_inflows
#' @details ...
#' @param res_filepath full path to reservoir data file for ERCOT
#' @param reservoir name of the reservoir
#' @param period either "baseline" or "future"
#' @param parameters tibble with headers of "target", "hedge", and "reservoir"
#' @param output return either optimized parameters ("parameters") or simulation results ("results")
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate group_by summarise
#' @importFrom lubridate year month
#' @export
#'
simulate_gcm_inflows <- function(res_filepath, reservoir, period, parameters, s_initial = 1.0){

  parameters %>% filter(reservoir == !! reservoir) -> x
  target <- x$target[1]
  hedge <- x$hedge[1]

  # get storage capacity
  read_ERCOT_data(res_filepath)[["reservoir_info"]] %>%
    filter(`Look Up Name for Reservoir` == reservoir) %>%
    .[["Maximum storage level (Acre-ft)"]] * af_to_mcm ->
    storage_capacity

  gcm_names %>%
    map_dfr(function(gcm){

      # read the inflows
      read_gcm_flows(reservoir, gcm, period) %>%
        mutate(year = year(date), month = month(date)) %>%
        group_by(year, month) %>%
        summarise(cumulative_inflow_mcm = sum(flow)) %>% ungroup() ->
        flows

      # simulate
      simulate_reservoir(
        s_cap = storage_capacity,
        s = storage_capacity * s_initial,
        i = flows[["cumulative_inflow_mcm"]],
        params = c(target, hedge)) %>%
        select(-s_obs) %>%
        mutate(reservoir = !!reservoir,
               gcm = !!gcm,
               inflow = flows[["cumulative_inflow_mcm"]],
               year = flows[["year"]],
               month = flows[["month"]])
    })
}

