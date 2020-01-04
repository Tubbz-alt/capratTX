#' calibrate_all_reservoirs
#' @details ...
#' @param res_filepath full path to reservoir data file for ERCOT
#' @param output return either optimized parameters ("parameters") or simulation results ("results")
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate
#' @export
#'
calibrate_all_reservoirs <- function(res_filepath,
                                     output = "parameters"){

  stopifnot(output == "results" | output == "parameters")

  read_ERCOT_data(res_filepath)[["storage_levels"]] %>%
    .[["Look Up Name for Reservoir"]] %>%
    unique() -> reservoir_shortnames

  reservoir_shortnames %>%
    .[!grepl("Amistad", .)] %>%
    .[!grepl("STP", .)] %>%
    map_dfr(function(x){
      message(x)
      calibrate_reservoir_model(x,
                                res_filepath = res_filepath,
                                plot = FALSE,
                                output = output) -> result
      if(output == "parameters"){
        result <- tibble(target = result[1], hedge = result[2])
      }
      result %>%
        mutate(reservoir = x)
    }) -> results_all

  return(results_all)
}

#' calibrate_reservoir_model
#' @details Performs calibration of reservoir model using monthly demand factors
#' @param reservoir_name name of reservoir to be calibrated
#' @param res_filepath full path to reservoir data file for ERCOT
#' @param plot T/F plot storage time series?
#' @param output return either optimized parameters ("parameters") or simulation results ("results")
#' @importFrom lubridate as_date month year
#' @importFrom tidyr gather
#' @importFrom nloptr nloptr
#' @import dplyr
#' @import ggplot2
#' @export
#'
calibrate_reservoir_model <- function(reservoir_name,
                                      res_filepath,
                                      plot = FALSE,
                                      s_cap_def = "reported",
                                      output = "parameters"){

  stopifnot(output == "results" | output == "parameters")

  read_controlflows(reservoir_name) -> flows

  read_ERCOT_data(res_filepath)[["storage_levels"]] %>%
    filter(`Look Up Name for Reservoir` == reservoir_name) %>%
    mutate(start_storage_mcm = `Storage (Acre-ft)` * af_to_mcm,
           Date = as_date(Date),
           year = year(Date), month = month(Date)) %>%
    select(year, month, start_storage_mcm) ->
    monthly_starting_storage

  read_ERCOT_data(res_filepath)[["reservoir_info"]] %>%
    filter(`Look Up Name for Reservoir` == reservoir_name) %>%
    .[["Maximum storage level (Acre-ft)"]] * af_to_mcm ->
    storage_capacity_reported

  if(s_cap_def == "reported"){
    storage_capacity <- storage_capacity_reported
  }else{
    storage_capacity <- max(storage_capacity_reported,
                            monthly_starting_storage[["start_storage_mcm"]])
  }

  # deprecated (water consumption to guide release)
  # read_ERCOT_data(res_filepath)[["generator_info"]] %>%
  #   filter(`Look Up Name for Reservoir` == reservoir_name) %>%
  #   select(`2014` = `2014 MWh Produced`,
  #          `2015` = `2015 MWh Produced`,
  #          `2016` = `2016 MWh Produced`,
  #          `2017` = `2017 MWh Produced`,
  #          `2018` = `2018 MWh Produced`,
  #          consumption_galperMwh = `Consumed Gallons/MWh Est. (based on technology using NREL data)`) %>%
  #   mutate(MWh_av = `2014` + `2015` + `2016` + `2017` + `2018`,
  #          cons_total_gal = MWh_av * consumption_galperMwh,
  #          cons_mcm_annual = cons_total_gal * US_gallon_to_mcm) %>%
  #   .[["cons_mcm_annual"]] %>% sum() %>% .[]/12 -> monthly_water_demand_for_cooling

  flows %>%
    mutate(year = year(date), month = month(date)) %>%
    group_by(year, month) %>%
    summarise(cumulative_inflow_mcm = sum(flow)) %>% ungroup() %>%
    right_join(monthly_starting_storage, by = c("year", "month")) -> calibration_data
    # mutate(period = paste(year, month, sep = "-")) %>%
    # mutate(period = factor(period, levels = .$period)) %>%
    # select(-year, -month) %>%
    # tidyr::gather(metric, vol, -period) %>%
    # ggplot(aes(period, vol, group = metric, col = metric)) + geom_line()

  # set up function for optimizing model parameters
  optimize_monthly_adjustments <- function(x){
    simulate_reservoir(
      s_cap = storage_capacity,
      s = calibration_data[["start_storage_mcm"]],
      i = calibration_data[["cumulative_inflow_mcm"]],
      params = x
    ) %>%
      # constrain the s_obs to max storage
      mutate(s_obs_cnstr = if_else(s_obs > storage_capacity,
                                   storage_capacity,
                                   s_obs)) ->
      sim_results

    # get storage TRMSE for minimization
    (box_cox_transform(sim_results[["s_sim"]], 0.2) -
        box_cox_transform(sim_results[["s_obs_cnstr"]], 0.2)) ^ 2 %>%
      mean() %>% sqrt()
  }

  nloptr(x0 = c(rep(0.5, 1), rep(0.5, 1)),
         eval_f = optimize_monthly_adjustments,
         lb = c(rep(0, 1), rep(0, 1)),
         ub = c(rep(10, 1), rep(1.2, 1)),
         opts = list("algorithm" = "NLOPT_LN_COBYLA",
                     "xtol_rel" = 1e-5,
                     "maxeval" = 10000)) %>%
    .$solution -> optimized_parameters

  if (output == "parameters") return(optimized_parameters)

  message("Optimized parameters: ",
          round(optimized_parameters[1], 3), " ",
          round(optimized_parameters[2], 3))

  simulate_reservoir(
    s_cap = storage_capacity,
    s = calibration_data[["start_storage_mcm"]],
    i = calibration_data[["cumulative_inflow_mcm"]],
    params = optimized_parameters
  ) %>%
    bind_cols(calibration_data[1:nrow(.), ]) %>%
    mutate(s_obs_cnstr = if_else(s_obs > storage_capacity,
                                 storage_capacity,
                                 s_obs)) -> result

  if (plot == FALSE) return(result)

   result %>%
     mutate(date = as_date(paste(year, month, 1, sep = "-"))) %>%
     select(date,
            simulated = s_sim,
            observed = s_obs,
            obs_cap_constr = s_obs_cnstr) %>%
     gather(metric, storage, -date) %>%
     ggplot(aes(date, storage, color = metric, linetype = metric)) +
     geom_line(alpha = 0.8) +
     scale_color_manual(values = c("darkgrey", "darkgrey", "dodgerblue")) +
     scale_linetype_manual(values = c(1, 2, 1)) +
     expand_limits(ymin = 0) + theme_classic() +
     theme(legend.position = "none") +
     labs(color = NULL, y = "Storage volume (Million cubic meters)", x = NULL,
          title = "Calibrated monthly storage model",
          subtitle = reservoir_name)

}
