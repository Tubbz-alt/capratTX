#' read_ERCOT_data
#' @details ...
#' @param res_filepath full path to reservoir data file for ERCOT
#' @importFrom readxl read_xlsx
#' @export
#'
read_ERCOT_data <- function(res_filepath) {

  # 1. Generator info
  read_xlsx(res_filepath, sheet = "Generator Info", skip = 1) ->
    generator_info

  # 2. Reservoirs info
  read_xlsx(res_filepath, sheet = "Reservoirs Info") ->
    reservoir_info

  # 3. Storage records
  read_xlsx(res_filepath, sheet = "Storage Levels") ->
    storage_levels

  # 4. Bathymetry
  read_xlsx(res_filepath, sheet = "Bathymetry") ->
    bathymetry

  return(
    list("generator_info" = generator_info,
         "reservoir_info" = reservoir_info,
         "storage_levels" = storage_levels,
         "bathymetry" = bathymetry)
  )

}


#' read_controlflows
#' @details ...
#' @param res_filepath full path to reservoir data file for ERCOT
#' @importFrom purrr map_dfr
#' @importFrom vroom vroom cols
#' @importFrom dplyr select one_of
#' @export
#'
read_controlflows <- function(reservoir){

  list.files(
    paste0(system.file("extdata/", package = "ECRAT"), "/Control"),
    full = T
  ) %>% .[grepl("runoff_mcm_", .)] -> control_files

  vroom_silent <- function(x) vroom(x, col_types = cols())

  # replace space with _ for reservoir column names
  gsub(" ", "_", reservoir) -> reservoir_
  if(reservoir_ == "STP"){
    reservoir_ <- "South_Texas_Project_Reservoir"
  }

  control_files %>%
    map_dfr(vroom_silent) %>%
    select(date, one_of(reservoir_)) -> controlflows

  names(controlflows) <- c("date", "flow")

  return(controlflows)

}


#' read_gcm_flows
#' @details ...
#' @param reservoir ...
#' @param gcm gcm
#' @param period "baseline" or "future"
#' @importFrom purrr map_dfr
#' @importFrom vroom vroom cols
#' @importFrom dplyr select one_of
#' @export
#'
read_gcm_flows <- function(reservoir, gcm, period){

  list.files(
    paste0(system.file("extdata/", package = "ECRAT"), "/gcm"),
    full = T
  ) %>%
    .[grepl(gcm, .)] %>%
    .[grepl(period, .)] -> gcm_file_dir

  list.files(gcm_file_dir, full = T) -> gcm_files

  vroom_silent <- function(x) vroom(x, col_types = cols())

  # replace space with _ for reservoir column names
  gsub(" ", "_", reservoir) -> reservoir_
  if(reservoir_ == "STP"){
    reservoir_ <- "South_Texas_Project_Reservoir"
  }

  gcm_files %>%
    map_dfr(vroom_silent) %>%
    select(date, one_of(reservoir_)) -> gcm_flows

  names(gcm_flows) <- c("date", "flow")

  return(gcm_flows)

}



box_cox_transform <- function(x, lamda){
  ((1 + x) ^ lamda - 1) / lamda
}
