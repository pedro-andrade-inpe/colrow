
#' @title Overlap between two polygonal simple features
#' @description Computes the overlap between each pair of objects from two
#' simple features and returns their intersection area. This function can be
#' used to compute the relation between GLOBIOM representations and specific
#' regions such as states or biomes. It returns a data.table with three columns:
#' One ID from the first simple feature, one ID from the second simple feature,
#' and their intersection area in Mha. Objects with no intersection will not be
#' shown in the output.
#' @param geom First simple feature, whose unique identified is the attribute ID.
#' This object is usually from GLOBIOM, a SimU, CR, or LU dataset.
#' @param ref Second simple feature, representing the region(s) to be mapped to
#' GLOBIOM representation.
#' @param id The attribute to be used as unique identifier for the second
#' simple feature.
#' @export
buildRelations <- function(geom, ref, id){
  old <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)

  ref <- sf::st_transform(ref, sf::st_crs(geom))

  myintersect <- suppressWarnings(sf::st_intersection(geom, ref))

  units::install_unit("Mha", "1e6ha")

  myintersect$area <- sf::st_area(myintersect) %>% units::set_units("Mha")

  result <- as.data.frame(myintersect) %>%
    dplyr::select(ID, !!id, area) %>%
    dplyr::transmute(ID1 = ID, ID2 = !!id, area = area)

  sf::sf_use_s2(old)

  return(result)
}


