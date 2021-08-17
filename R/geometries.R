
checkVersion = function(directory){
  mfile <- paste0(directory, "/version.txt")
  outdatedMessage <- "Outdated data directory. Please download a new version from https://bit.ly/2RrgZi9"
  if(file.exists(mfile)){
    con <- file(mfile, "r")
    line <- readLines(con, n = 1)
    close(con)

    if(line != "0.3")
      stop(outdatedMessage)
  }
  else{
    if(file.exists(paste0(directory, "/COLROW30.shp")))
      stop(outdatedMessage)
    else
      stop("Invalid data directory")
  }
}

LU2CR <- function(){
  luctocrfile <- "extdata/representations/LUtoCR.txt"
  data <- system.file(luctocrfile, package = "colrow") %>%
    utils::read.csv(header = FALSE, as.is = TRUE) %>%
    unlist() %>% tibble::tibble()

  colnames(data) <- "values"
  data <- data[-dim(data)[1],] # remove the last element, which is ""

  data <- data %>%
    dplyr::mutate(values = stringr::str_replace_all(values, " ", "")) %>%
    tidyr::separate("values", c("LU", "CR")) %>%
    dplyr::group_by(LU) %>%
    dplyr::arrange(.by_group = TRUE)

  data[-which(is.na(data[,"CR"])),]
}

## Map from LU to SimU
LU2SimU <- function(){
  system.file("extdata/representations/LUtoSimU.txt", package = "colrow") %>%
    utils::read.csv(header = FALSE, as.is = TRUE) %>%
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
#' @param countryNames Names of the countries as a string vector. It can have one or more values.
#' @param dataDirectory Directory where input data is located. This directory needs to have
#' files available at https://bit.ly/2RrgZi9 (shortened from https://www.dropbox.com/sh/sqocqe45jwmug2p/AAAbv-IAg24a_R4vYsP9zqV_a?dl=0).
#' @param join Should all SimuS with the same ID be represented together as a single MultiPolygon?
#' The default value is true.
#' @param simu A simple feature with loaded SimUs. Default is NULL, which means that the SimU data will be loaded from dataDirectory.
#' @export
getSimU <- function(countryNames, dataDirectory, join = TRUE, simu = NULL){
  if(length(countryNames) > 1){
    result <- getSimU(countryNames[1], dataDirectory)
    result$Country <- countryNames[1]

    for(i in 2:length(countryNames)){
      country <- getSimU(countryNames[i], dataDirectory)
      country$Country <- countryNames[i]
      result <- rbind(result, country)
    }

    return(result)
  }

  checkVersion(dataDirectory)
  cat(crayon::green("Reading all countries\n"))

  countries <- utils::read.csv(system.file("extdata/ID_COUNTRY.csv", package = "colrow"))

  cat(crayon::green(paste0("Selecting ", countryNames, "\n")))

  country <- countries %>% dplyr::filter(ALLCOUNTRY == countryNames)

  if(dim(country)[1] == 0){
    distances <- countryNames %>%
      stringdist::stringdist(countries$ALLCOUNTRY, method = "dl")

    suggestions <- unique(countries$ALLCOUNTRY[which(distances < 3)])

    if(length(suggestions) > 0)
      stop(paste0("Could not find ",
                  countryNames,
                  ". Do you mean ",
                  paste(suggestions),
                  "?"))
    else
      stop(paste0("Could not find ", countryNames), ".")
  }

  if(is.null(simu)){
    cat(crayon::green("Reading all SimUs\n"))
    simu <- sf::read_sf(paste0(dataDirectory, "SimU_all.shp"))
  }

  cat(crayon::green("Subsetting SimUs\n"))

  simuCountry <- simu %>%
    dplyr::filter(COUNTRY == country$COUNTRYCODE_UN) %>%
    dplyr::filter(SimUID != 0) %>%
    tidyr::separate("Grd30", c("X", "Y")) %>%
    dplyr::mutate(ColRow = paste0("CR", stringr::str_pad(X, width = 3, pad = "0"), stringr::str_pad(Y, width = 3, pad = "0"))) %>%
    dplyr::mutate(ID = SimUID) %>%
    dplyr::select(ID, ColRow)

  if(join){
    cat(crayon::green("Joining SimUs\n"))

    simuCountry <- suppressMessages(simuCountry %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(ColRow = ColRow[1]))
  }

  simuCountry
}

#' @title Return LU geometries for a given country.
#' @description Return all Large Units of a given country as a simple feature.
#' @param countryNames Names of the countries as a string vector. It can have one or more values.
#' As default, it will return all countries in the World as LU.
#' @param dataDirectory Directory where input data is located. This directory needs to have
#' files available at https://www.dropbox.com/sh/sqocqe45jwmug2p/AAAbv-IAg24a_R4vYsP9zqV_a?dl=0.
#' @param cache If true (default), use the LUs precomputed.
#' Otherwise, it will compute from the original data.
#' @param as.cr A list of countries to be loaded as CR instead of LU.
#' @param simu A simple feature with loaded SimUs. Default is NULL, which means that the SimU data
#' will be loaded from dataDirectory.
#' @export
getLU <- function(countryNames = NULL, dataDirectory, cache = TRUE, simu = NULL, as.cr = NULL){
  if(is.null(countryNames)){
    countryNames <- colrow::getCountries()
  }

  if(!is.null(as.cr)){
    countryNames <- countryNames[-which(countryNames %in% as.cr)]
    ludata <- colrow::getLU(countryNames, dataDirectory)
    crdata <- colrow::getCR(as.cr, dataDirectory)

    return(rbind(ludata, crdata))
  }

  if(length(countryNames) > 1){
    result <- getLU(countryNames[1], dataDirectory, cache, simu)

    for(i in 2:length(countryNames)){
      result <- rbind(result, getLU(countryNames[i], dataDirectory, cache, simu))
    }

    return(result)
  }

  checkVersion(dataDirectory)

  if(cache){
    cat(crayon::green(paste0("Loading cached version of LU data for ", countryNames, "\n")))

    result <- sf::read_sf(paste0(dataDirectory, "worldLU.shp")) %>%
      sf::st_set_crs(4326) %>%
      dplyr::filter(Country == countryNames)

    return(result)
  }

  res <- colrow::getSimU(countryNames, dataDirectory, FALSE, simu)

  if(dim(res)[1] == 0) return(NULL)

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
  countries <- utils::read.csv(system.file("extdata/ID_COUNTRY.csv", package = "colrow"))

  sort(unique(countries$ALLCOUNTRY))
}

#' @title Return CR geometries for a given country.
#' @description Return all ColRows of a given country as a set of simple feature
#' polygons, using the geometries of their respective SimUs.
#' @param countryNames Names of the countries as a string vector. It can have one or more values.
#' @param dataDirectory Directory where input data is located. This directory needs to have
#' files available at https://www.dropbox.com/sh/sqocqe45jwmug2p/AAAbv-IAg24a_R4vYsP9zqV_a?dl=0.
#' @param cache If true (default), use the LUs precomputed.
#' Otherwise, it will compute from the original data.
#' @param simu A simple feature with loaded SimUs. Default is NULL, which means that the SimU data will be loaded from dataDirectory.
#'
#' @export
getCR <- function(countryNames, dataDirectory, cache = TRUE, simu = NULL){
  if(length(countryNames) > 1){
    result <- getCR(countryNames[1], dataDirectory, cache, simu)

    for(i in 2:length(countryNames)){
      result <- rbind(result, getCR(countryNames[i], dataDirectory, cache, simu))
    }

    return(result)
  }

  checkVersion(dataDirectory)

  if(cache){
    cat(crayon::green(paste0("Loading cached version of CR data for ", countryNames, "\n")))

    result <- sf::read_sf(paste0(dataDirectory, "worldCR.shp")) %>%
      sf::st_set_crs(4326) %>%
      dplyr::filter(Country == countryNames)

    return(result)
  }

  res <- getSimU(countryNames, dataDirectory, FALSE, simu)

  if(dim(res)[1] == 0) return(NULL)

  cat(crayon::green("Mapping SimU to CR\n"))

  countryCR <- maptools::unionSpatialPolygons(sf::as_Spatial(res), res$ColRow)
  ids <- row.names(countryCR)

  countryCR <- sf::st_as_sf(countryCR)
  countryCR$ID <- ids

  countryCR
}

#' @title Return CR centroids for a given country.
#' @description Return all ColRows of a given country as a set of simple feature points.
#' @param countryNames Name of the country.
#' @param dataDirectory Directory where input data is located. This directory needs to have
#' files available at https://www.dropbox.com/sh/sqocqe45jwmug2p/AAAbv-IAg24a_R4vYsP9zqV_a?dl=0.
#' @export
getCRcentroids <- function(countryNames, dataDirectory){
  myCR <- colrow::getCR(countryNames, dataDirectory)

  cat(crayon::green("Reading CR centroids\n"))

  CRpoints <- sf::read_sf(paste0(dataDirectory, "COLROW30.shp")) %>%
    sf::st_set_crs(4326)

  CRvalueToID <- function(value){
    vs <- strsplit(value, " - ")
    v1 <- vs[[1]][1] %>% stringr::str_pad(3, pad = "0")
    v2 <- vs[[1]][2] %>% stringr::str_pad(3, pad = "0")
    paste0("CR", v1, v2)
  }

  cat(crayon::green("Generating CR IDs\n"))

  CRpoints$ID <- sapply(CRpoints$COLROW30, CRvalueToID)

  cat(crayon::green("Subsetting CR centroids\n"))

  result <- CRpoints[CRpoints$ID %in% myCR$ID, ]
  result$RealArea_m <- NULL
  return(result)
}
