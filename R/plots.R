#' plot_watershed
#' @details plot watershed for a named reservoir or set of reservoirs
#' @param reservoir name of reservoir (or reservoirs as vector)
#' @export
#'
plot_watershed <- function(reservoir) {
  message(paste("plotting watershed for", reservoir))
}

#' plot_texas_watershed
#' @details plot ERCOT reservoir points by Summer Capacity and Storage Level
#' @param drawdown_scenario tibble with reservoir name and storage
#' @import ggplot2
#' @importFrom vroom vroom
#' @importFrom viridis scale_color_viridis
#' @importFrom dplyr mutate rename
#' @importFrom sf st_read st_intersection
#' @importFrom tmaptools set_projection
#' @importFrom sp SpatialPointsDataFrame CRS
#' @export

plot_texas_reservoirs <- function(drawdown_scenario){

  system.file("extdata", package = "capratTX") -> sys_dir

  vroom_silent(paste0(sys_dir,
               "/ercot_reservoirs_lat_lon.csv")) -> reservoirs

  # Temporary random storage levels
  reservoirs %>%
    left_join(drawdown_scenario, by = "res_name") %>%
    filter(!is.na(storage)) -> res_table

  # Get lat/long and data for spatial points
  res_table[,c(3,4)] -> latlon
  res_table[,c(1,2,5,6)] -> data
  SpatialPointsDataFrame(coords = latlon, data = data,
                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    as.data.frame() %>%
    rename(`Summer Capacity MW` = Summer.Capacity..MW..at.Reservoir) -> pointsdata

  # Load in texas shape
  st_read(paste0(sys_dir,"/Texas_Files/Texas_Shape.shp"), quiet = TRUE) %>%
    set_projection(projection = "+proj=longlat +datum=WGS84 +no_defs") -> texas

  # Load in Texas HUC4 shape
  st_read(paste0(sys_dir, "/Texas_Files/Texas_HUC4.shp"), quiet = TRUE) %>%
    set_projection(projection = "+proj=longlat +datum=WGS84 +no_defs")-> huc4

  # Intersect to only get parts of HUC4 within Texas
  suppressMessages(suppressWarnings(st_intersection(texas, huc4))) -> select_huc

  # Plot Texas Summer Capacity and Storage with HUC4
  ggplot(texas) +
    geom_sf(color = "black") +
    geom_sf(data = select_huc, fill = "grey98", colour = "grey30") +
    geom_point(data = pointsdata,
               alpha = 1,
               aes(x = lon, y = lat, size = `Summer Capacity MW`, col = storage)) +
    scale_color_viridis(name = "Storage %",
                        breaks=c(0,25,50,75,100),
                        limits=c(0,100), direction = -1, option = "E") +
    geom_point(data = pointsdata, shape = 21, aes(x = lon, y = lat, size = `Summer Capacity MW`)) +
    scale_size_continuous(name = "Capacity MW",
                          breaks = c(1000, 5000),
                          range = c(0.1, 15)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_void()
}




