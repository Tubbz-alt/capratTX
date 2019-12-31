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
  ) -> control_files

  vroom_silent <- function(x) vroom(x, col_types = cols())

  control_files %>%
    map_dfr(vroom_silent) %>%
    select(date, one_of(reservoir)) -> controlflows

  names(controlflows) <- c("date", "flow")

  return(controlflows)

}

box_cox_transform <- function(x, lamda){
  ((1 + x) ^ lamda - 1) / lamda
}
