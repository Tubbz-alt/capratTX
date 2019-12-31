#' calibrate_all_reservoirs
#' @details ...
#' @param res_filepath full path to reservoir data file for ERCOT
#' @export
#'
calibrate_all_reservoirs <- function(res_filepath){

}

#' calibrate_reservoir_model
#' @details Performs calibration of reservoir model using monthly demand factors
#' @param reservoir_name name of reservoir to be calibrated
#' @param res_filepath full path to reservoir data file for ERCOT
#' @param plot T/F plot storage time series?
#' @importFrom lubridate as_date month year
#' @importFrom tidyr gather
#' @importFrom nloptr nloptr
#' @import dplyr
#' @import ggplot2
#' @export
#'
calibrate_reservoir_model <- function(reservoir_name, res_filepath, plot = FALSE){

  read_controlflows(reservoir_name) -> flows

  # this section to be taken out when naming convention and units detail fixed
  reservoir_name <- "Bardwell"
  flows %>% mutate(flow_Mm3 = (flow * 1e-3 / 25) * 400e6 * 1e-6) -> flows
  # ------

  read_ERCOT_data(res_filepath)[["storage_levels"]] %>%
    filter(`Look Up Name for Reservoir` == reservoir_name) %>%
    mutate(start_storage_Mm3 = `Storage (Acre-ft)` * af_to_Mm3,
           Date = as_date(Date),
           year = year(Date), month = month(Date)) %>%
    select(year, month, start_storage_Mm3) ->
    monthly_starting_storage

  read_ERCOT_data(res_filepath)[["reservoir_info"]] %>%
    filter(`Look Up Name for Reservoir` == reservoir_name) %>%
    .[["Maximum storage level (Acre-ft)"]] * af_to_Mm3 ->
    storage_capacity_reported

  storage_capacity <- max(storage_capacity_reported,
                          monthly_starting_storage[["start_storage_Mm3"]])

  read_ERCOT_data(res_filepath)[["generator_info"]] %>%
    filter(`Look Up Name for Reservoir` == reservoir_name) %>%
    select(`2014` = `2014 MWh Produced`,
           `2015` = `2015 MWh Produced`,
           `2016` = `2016 MWh Produced`,
           `2017` = `2017 MWh Produced`,
           `2018` = `2018 MWh Produced`,
           consumption_galperMwh = `Consumed Gallons/MWh Est. (based on technology using NREL data)`) %>%
    mutate(MWh_av = `2014` + `2015` + `2016` + `2017` + `2018`,
           cons_total_gal = MWh_av * consumption_galperMwh,
           cons_Mm3_annual = cons_total_gal * US_gallon_to_Mm3) %>%
    .[["cons_Mm3_annual"]] %>% sum() %>% .[]/12 -> monthly_water_demand_for_cooling

  flows %>%
    mutate(year = year(date), month = month(date)) %>%
    group_by(year, month) %>%
    summarise(cumulative_inflow_Mm3 = sum(flow_Mm3)) %>% ungroup() %>%
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
      s = calibration_data[["start_storage_Mm3"]],
      i = calibration_data[["cumulative_inflow_Mm3"]],
      r = monthly_water_demand_for_cooling,
      params = x,
      mth = calibration_data[["month"]]
    ) -> sim_results

    # get storage TRMSE for minimization
    (box_cox_transform(sim_results[["s_sim"]], 0.1) -
        box_cox_transform(sim_results[["s_obs"]], 0.1)) ^ 2 %>%
      mean() %>% sqrt()
  }

  nloptr(x0 = c(rep(0, 1), rep(0.5, 1)),
         eval_f = optimize_monthly_adjustments,
         lb = c(rep(0, 1), rep(0, 1)),
         ub = c(rep(100, 1), rep(1.2, 1)),
         opts = list("algorithm" = "NLOPT_GN_CRS2_LM",
                     "xtol_rel" = 1e-5,
                     "maxeval" = 10000)) %>%
    .$solution -> optimized_parameters

  message("Optimized parameters: ",
          round(optimized_parameters[1], 3), " ",
          round(optimized_parameters[2], 3))

  simulate_reservoir(
    s_cap = storage_capacity,
    s = calibration_data[["start_storage_Mm3"]],
    i = calibration_data[["cumulative_inflow_Mm3"]],
    r = monthly_water_demand_for_cooling,
    params = optimized_parameters,
    mth = calibration_data[["month"]]
  ) %>%
    bind_cols(calibration_data[1:nrow(.), ]) -> result

  if (plot == FALSE) return(result)

   result %>%
     mutate(date = as_date(paste(year, month, 1, sep = "-"))) %>%
     select(date, s_sim, s_obs) %>%
     gather(metric, storage, -date) %>%
     ggplot(aes(date, storage, color = metric)) + geom_line() +
     expand_limits(ymin = 0) + theme_classic() +
     labs(color = NULL, y = "Storage volume (Million cubic meters)", x = NULL,
          title = "Calibrated monthly storage model",
          subtitle = reservoir_name)



}
