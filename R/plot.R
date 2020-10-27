
#' @title Plot several maps according to a prefix
#' @description Plot several maps according to an attribute prefix.
#' This function is useful when the user wants to plot different
#' attributes using the same legend.
#' Tipically, the strings after the prefixes are years and the rest of
#' the documentation uses "year" with this in mind, but the
#' implementation is generic.
#' @param data A sf (simple feature) object.
#' @param prefix A string with prefix name. Every attribute starting with
#' this prefix will be drawn in a separate map.
#' @param title A title prefix to be used in the map. The map title
#' will also have the year.
#' @param slices The number of slices. The default value is 5.
#' @param palette A string with an RColorBrewer palette.
#' @param additional One or more tmap functions (concatenated by +) to be
#' drawn after plotting the map.
#' @param extension The file extension. The default is "pdf".
#' @param ... Any additional parameter to be passed to tm_fill.
#' @export
plotAll <- function(data, prefix, title = prefix, slices = 5, palette = "RdPu", additional = NULL, extension = "pdf", ...){
  attributes <- colnames(data) %>%
    stringr::str_sub(1, nchar(prefix)) %>%
    (function(.) colnames(data)[which(. == prefix)])

  palette <- RColorBrewer::brewer.pal(slices, palette)
  values <- as.data.frame(data)[,attributes]

  vmax <- max(values)
  vmin <- min(values)

  cuts <- seq(vmin, vmax, length.out = slices)

  for(attribute in attributes){
    cat(paste0("Processing attribute '", attribute, "'\n"))

    suffix <- stringr::str_sub(attribute, nchar(prefix) + 1, nchar(attribute))
    mtitle <- paste(title, suffix)

    m <- tmap::tm_shape(data) +
      tmap::tm_fill(col = attribute, palette = palette, breaks = cuts, title = mtitle, ...) +
      additional

    tmap::tmap_save(m, paste0(attribute, ".", extension))
  }
}
