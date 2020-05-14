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
#' @param reservoir local data directory
#' @import ggplot2
#' @importFrom vroom vroom
#' @importFrom viridus scale_color_viridis
#' @importFrom dplyr mutate rename
#' @importFrom sf st_read st_intersection
#' @importFrom tmaptools set_projection
#' @importFrom sp SpatialPointsDataFrame
#' @export

plot_texas_reservoirs <- function(data_dir){

  # Load in ERCOT reservoir data
  vroom(paste0(data_dir,"/inst/extdata/ercot_reservoirs_lat_lon.csv")) -> reservoirs

  # Insert code for storage level data below
  #left_join(reservoirs, storage_levels, by = res_name) -> res_table

  # Temporary random storage levels
  reservoirs %>%
    mutate(storage = sample(0:100, 44)) -> res_table

  # Get lat/long and data for spatial points
  res_table[,c(3,4)] -> latlon
  res_table[,c(1,2,5,6)] -> data
  SpatialPointsDataFrame(coords = latlon, data = data,
                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    as.data.frame() %>%
    rename(`Summer Capacity MW` = Summer.Capacity..MW..at.Reservoir) -> pointsdata

  # Load in texas shape
  st_read(paste0(data_dir,"/inst/extdata/Texas_Files/Texas_Shape.shp")) %>%
    set_projection(projection = "+proj=longlat +datum=WGS84 +no_defs") -> texas

  # Load in Texas HUC4 shape
  st_read(paste0(data_dir, "/inst/extdata/Texas_Files/Texas_HUC4.shp")) %>%
    set_projection(projection = "+proj=longlat +datum=WGS84 +no_defs")-> huc4

  # Intersect to only get parts of HUC4 within Texas
  suppressMessages(suppressWarnings(st_intersection(texas, huc4))) -> select_huc

  # Plot Texas Summer Capacity and Storage with HUC4
  ggplot(texas) +
    geom_sf() +
    geom_sf(data = select_huc, fill = "grey86", colour = "grey30") +
    geom_point(data = pointsdata, alpha = 0.8,aes(x = lon, y = lat, size = `Summer Capacity MW`, col = storage)) +
    scale_color_viridis(name = "Storage %",
                        breaks=c(0,25,50,75,100),
                        limits=c(0,100)) +
    geom_point(data = pointsdata, shape = 21, aes(x = lon, y = lat, size = `Summer Capacity MW`)) +
    scale_size_continuous(name = "Summer Capacity MW",
                          breaks = c(1000, 3000, 5000, 6000),
                          range = c(0.01,9)) +
    theme(line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("ERCOT Reservoir Summer Capacity and Storage")

}




