
#' @title Export data to NetCDF format
#' @description Export processed data to a NetCDF
#' @param country Name of the country.
#' @param attribute Name of the attribute.
#' @param result Processed data with processFile().
#' @param dataDir Directory where Globiom data is stored.
#' @param prefix Prefix for the filename to be saved. The file name will be
#' prefix followed by attribute followed by .nc.
#' @export
ColRowToNetCDF <- function(country, attribute, result, dataDir, prefix){
  centroids <- getCRcentroids(country, paste0(dataDir, "/"))

  pts <- centroids |>
    dplyr::left_join(
      sf::st_drop_geometry(result),
      by = "ID"
    )

  xyz <- data.frame(
    x = pts$XCOORD,
    y = pts$YCOORD,
    v = pts[[attribute]]
  )

  r <- terra::rast(
    xyz,
    type = "xyz",
    crs = "EPSG:4326"
  )

  terra::writeCDF(
    r,
    paste0(prefix, "_", attribute, ".nc"),
    varname = attribute,
    overwrite = TRUE
  )
}
