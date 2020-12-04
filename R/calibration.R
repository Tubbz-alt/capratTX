#' calibrate_all_reservoirs
#' @details ...
#' @param data_path path to data directory "ERCOT Reservoir Watershed Delineations and Inflow Scenarios"
#' @param output return either optimized parameters ("parameters") or simulation results ("results")
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate
#' @export
#'
calibrate_all_reservoirs <- function(data_path,
                                     output = "parameters"){

  stopifnot(output == "results" | output == "parameters")

  paste0(data_path,
         "/Reservoir records and attributes/",
         "ERCOT_reservoir_attributes_and_records_PUBLIC.xlsx") ->
    res_filepath

  read_ERCOT_data(res_filepath)[["storage_levels"]] %>%
    .[["Look Up Name for Reservoir"]] %>%
    unique() -> reservoir_shortnames

  reservoir_shortnames %>%
    .[!grepl("Amistad", .)] %>%
    .[!grepl("STP", .)] %>%
    map_dfr(function(x){
      message(x)
      calibrate_reservoir_model(x,
                                data_path = data_path,
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

#' validate_all_reservoirs
#' @details perform k-fold validation on all reservoir models
#' @param data_path path to data directory "ERCOT Reservoir Watershed Delineations and Inflow Scenarios"
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate
#' @export
#'
validate_all_reservoirs <- function(data_path){

  paste0(data_path,
         "/Reservoir records and attributes/",
         "ERCOT_reservoir_attributes_and_records_PUBLIC.xlsx") ->
    res_filepath

  read_ERCOT_data(res_filepath)[["storage_levels"]] %>%
    .[["Look Up Name for Reservoir"]] %>%
    unique() -> reservoir_shortnames

  reservoir_shortnames %>%
    .[!grepl("Amistad", .)] %>%
    .[!grepl("STP", .)] %>%
    .[!grepl("Falcon", .)] %>%
    map(function(x){
      message(x)
      calibrate_reservoir_model(x,
                                data_path = data_path,
                                plot = FALSE,
                                validation_mode = TRUE) -> trmse_scores

      list(trmse_scores) -> trmse_scores_list
      names(trmse_scores_list) <- x
      return(trmse_scores_list)

    }) -> results_all

  return(results_all)
}


#' calibrate_reservoir_model
#' @details Performs calibration of reservoir model using monthly demand factors
#' @param reservoir_name name of reservoir to be calibrated
#' @param data_path path to data directory "ERCOT Reservoir Watershed Delineations and Inflow Scenarios"
#' @param plot T/F plot storage time series?
#' @param output return either optimized parameters ("parameters") or simulation results ("results")
#' @param validation_mode logical. Set to TRUE to perform k-fold validation on model and report RMSE scores across validation groups.
#' @importFrom lubridate as_date month year
#' @importFrom purrr map
#' @importFrom tidyr gather
#' @importFrom nloptr nloptr
#' @import dplyr
#' @import ggplot2
#' @export
#'
calibrate_reservoir_model <- function(reservoir_name,
                                      data_path,
                                      plot = FALSE,
                                      s_cap_def = "reported",
                                      output = "parameters",
                                      validation_mode = FALSE){

  stopifnot(output == "results" | output == "parameters")

  paste0(data_path,
         "/Reservoir records and attributes/",
         "ERCOT_reservoir_attributes_and_records_PUBLIC.xlsx") ->
    res_filepath

  read_controlflows(reservoir_name,
                    data_path = data_path) ->
    flows

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

  # get yr col for k-fold cross validation
  calibration_data[["year"]] -> yr_col_all
  yr_col_all[which(!is.na(calibration_data[["cumulative_inflow_mcm"]]))] -> yr_col

  # set up function for optimizing model parameters
  optimize_monthly_adjustments <- function(x, leave_out_yr){
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

    training_index <- which(yr_col != leave_out_yr)

    # get storage TRMSE for minimization
    (box_cox_transform(sim_results[["s_sim"]], 0.2) -
        box_cox_transform(sim_results[["s_obs_cnstr"]], 0.2)) ^ 2 %>%
      .[training_index] %>%
      mean() %>% sqrt()
  }

  nloptr(x0 = c(rep(0.5, 1), rep(0.5, 1)),
         eval_f = optimize_monthly_adjustments,
         lb = c(rep(0, 1), rep(0, 1)),
         ub = c(rep(10, 1), rep(1.2, 1)),
         opts = list("algorithm" = "NLOPT_LN_COBYLA",
                     "xtol_rel" = 1e-5,
                     "maxeval" = 10000),
         leave_out_yr = 2100) ->
    # ^^ 2100 is arbitrary yr not included in data, avoiding validation
    nloptr_output

  nloptr_output$solution -> optimized_parameters

  if (output == "parameters" & validation_mode == FALSE) return(optimized_parameters)

  message("Optimized parameters: ",
          round(optimized_parameters[1], 3), " ",
          round(optimized_parameters[2], 3))

  message("Objective result = ", nloptr_output$objective)


  c(yr_col) %>% unique() %>% .[-1] %>%
    map(function(loy){

      nloptr(x0 = c(rep(0.5, 1), rep(0.5, 1)),
             eval_f = optimize_monthly_adjustments,
             lb = c(rep(0, 1), rep(0, 1)),
             ub = c(rep(10, 1), rep(1.2, 1)),
             opts = list("algorithm" = "NLOPT_LN_COBYLA",
                         "xtol_rel" = 1e-5,
                         "maxeval" = 10000),
             leave_out_yr = loy)$solution -> train_params

      simulate_reservoir(
        s_cap = storage_capacity,
        s = calibration_data[["start_storage_mcm"]],
        i = calibration_data[["cumulative_inflow_mcm"]],
        params = train_params
      ) %>%
        # constrain the s_obs to max storage
        mutate(s_obs_cnstr = if_else(s_obs > storage_capacity,
                                     storage_capacity,
                                     s_obs),
               year = yr_col) %>%
        filter(year == loy) ->
        sim_results_loy

      simulate_reservoir(
        s_cap = storage_capacity,
        s = calibration_data[["start_storage_mcm"]],
        i = calibration_data[["cumulative_inflow_mcm"]],
        params = optimized_parameters
      ) %>%
        # constrain the s_obs to max storage
        mutate(s_obs_cnstr = if_else(s_obs > storage_capacity,
                                     storage_capacity,
                                     s_obs),
               year = yr_col) %>%
        filter(year == loy) ->
        sim_results

      (box_cox_transform(sim_results[["s_sim"]], 0.2) -
          box_cox_transform(sim_results[["s_obs_cnstr"]], 0.2)) ^ 2 %>%
        mean() %>% sqrt() -> sim_result

      (box_cox_transform(sim_results_loy[["s_sim"]], 0.2) -
          box_cox_transform(sim_results_loy[["s_obs_cnstr"]], 0.2)) ^ 2 %>%
        mean() %>% sqrt() -> sim_result_loy

      # impact of leave year out on test year performance result
      100* ((sim_result_loy - sim_result) / sim_result)

    }) %>% unlist() -> k_fold_results

  if(validation_mode == TRUE) return(k_fold_results)


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

