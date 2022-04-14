
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
#  geom=myLU
#  ref=biomes
#  id="CD_LEGEN1"

  old <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)

  ref <- sf::st_transform(ref, sf::st_crs(geom))

  myintersect <- suppressWarnings(sf::st_intersection(geom, ref))

  units::install_unit("Mha", "1e6ha")

  myintersect$area <- sf::st_area(myintersect) %>% units::set_units("Mha")

  result <- as.data.frame(myintersect) %>%
    dplyr::select(ID, !!id, area) %>%
    dplyr::rename(ID1 = ID, ID2 = !!id, area = area)

  sf::sf_use_s2(old)

  return(result)
}

#' @title Normalize relations according to the area
#' @description Normalize the relations according to the covered area. This
#' function is useful to distribute SimU, CR, or LU to other polygonal
#' representations. The returned values equal to 1 are completely distributed
#' to a single polygon. Whenever a value is less than 1, the SimU, CR, or LU
#' overlaps with two or more polygons, meaning that the content will be
#' divided to the respective polygons according to the intersection area.
#' @param data A tibble comming from buildRelations().
#' @export
normalizeRelationsByArea <- function(data){
  result <- data %>%
    mutate(area = units::drop_units(area)) %>%
    dplyr::group_by(ID1) %>%
    dplyr::mutate(area = area / sum(area)) %>%
    dplyr::arrange(ID1)

  return(result)
}

#' @title Filter the relations with greater area
#' @description Filter the relations with the greater intersection area. The
#' output will have only one row per each SimU, CR, or LU.
#' @param data A tibble comming from normalizeRelationsByArea().
#' @export
filterGreater <- function(data){
  result <- result %>%
    group_by(ID1) %>%
    filter(area == max(area))

  return(result)
}
