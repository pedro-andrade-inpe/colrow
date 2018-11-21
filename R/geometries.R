
LU2CR <- function(){
  data <- system.file("extdata/representations/LUtoCR.txt", package = "colrow") %>%
    read.csv(header = FALSE, as.is = TRUE) %>%
    unlist() %>% tibble::tibble()

  colnames(data) <- "values"
  data <- data[-dim(data)[1],] # remove the last element, which is ""

  data <- dplyr::mutate(data, values = stringr::str_replace_all(values, " ", "")) %>%
    tidyr::separate("values", c("LU", "CR")) %>%
    dplyr::group_by_(.dots = "LU") %>%
    dplyr::arrange(.by_group = TRUE)

  data[-which(is.na(data[,"CR"])),]
}

## Map from LU to SimU
LU2SimU <- function(){
  system.file("extdata/representations/LUtoSimU.txt", package = "colrow") %>%
    read.csv(header = FALSE, as.is = TRUE) %>%
    unlist() %>%
    tibble::tibble() %>%
    magrittr::set_colnames("values") %>%
    dplyr::mutate(values = stringr::str_replace_all(values, " ", "")) %>%
    dplyr::filter(values != "") %>%
    tidyr::separate("values", c("SimU", "LU")) %>%
    dplyr::group_by(SimU) %>%
    dplyr::arrange(.by_group = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(SimU = as.numeric(SimU))
}

#' @title Return SimU geometries for a given country.
#' @description Return all Simulation Units of a given country as a simple feature.
#' @param countryName Name of the country.
#' @param dataDirectory Directory where input data is located. This directory needs to have
#' files available at https://www.dropbox.com/sh/sqocqe45jwmug2p/AAAbv-IAg24a_R4vYsP9zqV_a?dl=0.
#' @param join Should all SimuS with the same ID be represented together as a single MultiPolygon?
#' The default value is true.
#' @export
getSimU <- function(countryName, dataDirectory, join = TRUE){
  cat(crayon::green("Reading all countries\n"))

  countries <- sf::read_sf(paste0(dataDirectory, "g2006_2.shp")) %>% sf::st_set_crs(4326)

  cat(crayon::green(paste0("Selecting ", countryName, "\n")))

  country <- countries %>% dplyr::filter(ADM0_NAME == countryName)

  if(dim(country)[1] == 0){
    distances <- stringdist::stringdist(countryName, countries$ADM0_NAME, method = "dl")

    suggestions <- unique(countries$ADM0_NAME[which(distances < 3)])

    if(length(suggestions) > 0)
      stop(paste0("Could not find ", countryName, ". Do you mean ", paste(suggestions), "?"))
    else
      stop(paste0("Could not find ", countryName), ".")
  }

  cat(crayon::green("Reading all SimUs\n"))

  simu <- sf::read_sf(paste0(dataDirectory, "SimU_all.shp"))

  cat(crayon::green("Subsetting SimUs\n"))

  simuCountry <- suppressMessages(simu[country, op = sf::st_intersects])

  countryNumber <- simuCountry$COUNTRY %>% table() %>% which.max() %>% names() %>% as.numeric()

  simuCountry <- simuCountry %>%
    dplyr::filter(COUNTRY == countryNumber) %>%
    dplyr::filter(SimUID != 0) %>%
    tidyr::separate("Grd30", c("X", "Y")) %>%
    dplyr::mutate(ColRow = paste0("CR", X, Y)) %>%
    dplyr::mutate(ID = SimUID) %>%
    dplyr::select(ID, ColRow)

  if(join){
    cat(crayon::green("Joining SimUs\n"))

    simuCountry <- simuCountry %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(ColRow = ColRow[1])
  }

  simuCountry
}

#' @title Return LU geometries for a given country.
#' @description Return all Large Units of a given country as a simple feature.
#' @param countryName Name of the country.
#' @param dataDirectory Directory where input data is located. This directory needs to have
#' files available at https://www.dropbox.com/sh/sqocqe45jwmug2p/AAAbv-IAg24a_R4vYsP9zqV_a?dl=0.
#' @export
getLU <- function(countryName, dataDirectory){
  res <- colrow::getSimU(countryName, dataDirectory, FALSE)
  cat(crayon::green("Mapping SimU to LU\n"))

  lusimu <- LU2SimU()

  names(lusimu)[1] <- "ID"

  cat(crayon::green("Merging data\n"))

  data <- merge(res, lusimu)

  cat(crayon::green("Computing union of SimUs within the same LU\n"))

  countryLU <- maptools::unionSpatialPolygons(sf::as_Spatial(data), data$LU)

  ids <- row.names(countryLU)

  countryLU <- sf::st_as_sf(countryLU)
  countryLU$ID <- ids

  countryLU
}

#' @title Return all country names available for g2006.
#' @description Return a vector of strings with all countries.
#' @param dataDirectory Directory where input data is located. This directory needs to have
#' files available at https://www.dropbox.com/sh/sqocqe45jwmug2p/AAAbv-IAg24a_R4vYsP9zqV_a?dl=0.
#' @export
getCountries <- function(dataDirectory){
  countries <- sf::read_sf(paste0(dataDirectory, "g2006_2.shp"))

  sort(unique(countries$ADM0_NAME))
}

#' @title Return CR geometries for a given country.
#' @description Return all ColRows of a given country as a simple feature.
#' @param countryName Name of the country.
#' @param dataDirectory Directory where input data is located. This directory needs to have
#' files available at https://www.dropbox.com/sh/sqocqe45jwmug2p/AAAbv-IAg24a_R4vYsP9zqV_a?dl=0.
#' @export
getCR <- function(countryName, dataDirectory){
  res <- getSimU(countryName, dataDirectory, FALSE)
  cat(crayon::green("Mapping SimU to CR\n"))

  countryCR <- maptools::unionSpatialPolygons(sf::as_Spatial(res), res$ColRow)
  ids <- row.names(countryCR)

  countryCR <- sf::st_as_sf(countryCR)
  countryCR$ID <- ids

  countryCR
}
