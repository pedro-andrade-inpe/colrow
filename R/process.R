
extractUse <- function (alldata, use, year) {
  var_name <- paste(use, year, sep = "")

  result <- alldata %>%
    dplyr::select_(.dots = c("ALLCOLROW", "USE", "SCENARIO", "YEAR", "VALUE")) %>%
    dplyr::filter_(~YEAR == year) %>%
    dplyr::filter_(~USE == use) %>%
    dplyr::select_(.dots = c("ALLCOLROW", "VALUE"))

  colnames(result) <- c("colrow", var_name)
  return(result)
}

processUse <- function(alldata){
  years <- unique(alldata$YEAR)
  uses <- unique(alldata$USE)

  res <- unique(alldata["ALLCOLROW"])
  colnames(res) <- c("colrow")

  for (u in uses) {
    for (y in years) {
      res1 <- extractUse(alldata, u, y)
      res <- dplyr::left_join(res, res1, by = "ID")
    }
  }

  return(res)
}

processProduct <- function(product_data, attrname){
  extract_one <- function(alldata, year) {
    var_name <- paste(attrname, year, sep = "")

    result <- alldata %>%
      dplyr::select_(.dots = c("ALLCOLROW", "SCENARIO", "YEAR", "VALUE")) %>%
      dplyr::filter_(~YEAR == year) %>%
      dplyr::select_(.dots = c("ALLCOLROW", "VALUE"))

    colnames(result) <- c("colrow", var_name)
    return(result)
  }

  years <- unique(product_data$YEAR)

  res <- unique(product_data["ALLCOLROW"])
  colnames(res) <- c("colrow")

  for(y in years) {
    res1 <- extract_one(product_data, y)
    res <- dplyr::left_join(res, res1, by = "ID")
  }

  return(res)
}

#' @title Read CR from a CSV file to tibble
#' @description Read a CSV file as a tibble. The column names are identified automatically by
#' colrow package.
#' @param directory Path where the data is stored. Note that the last directory name will be the scenario's name.
#' @param product Name of the product to be read. The file name will be 'product_scenario.CSV'
#' @export
readCR <- function(directory, product){
  data_file <- paste0(directory, "/", product, "_", basename(directory), ".CSV")

  readr::read_csv(data_file, col_names = attr_names[[product]], progress = FALSE)
}

processScenario <- function(datafile, scenario, output){
  shp <- rgdal::readOGR(datafile, encoding = "ESRI Shapefile", verbose = FALSE)

  uses <- c("ACR_COMPARE", "Land_Compare3")

  for(product in uses){
    cat(paste0("Parsing '", product, "'\n"))

    data <-readCR(scenario, product)
    result <- processUse(data)

    result

    shp <- sp::merge(shp, result, by = "ID")
  }

  products <- c("FOREST_PA", "DEFORESTATION", "LIVE_BOV", "LIVE_SGT", "NATLAND_PA")

  names <- list(
    FOREST_PA = "FOR_PA",
    DEFORESTATION = "DEFOR",
    LIVE_BOV = "LV_BOV",
    LIVE_SGT = "LV_SGT",
    NATLAND_PA = "NATLD"
  )

  for(product in products){
    cat(paste0("Parsing '", product, "'\n"))

    data <-readCR(scenario, product)
    res <- processProduct(data, names[product])
    shp <- sp::merge(shp, res, by = "ID")
  }

  shp@data[is.na(shp@data)] <- 0.0

  cat(paste0("Writing file '", basename(scenario), ".shp'\n"))

  rgdal::writeOGR(shp, output, basename(scenario), driver = "ESRI Shapefile", overwrite_layer = TRUE)
}

#' @title Return the scenarios of a directory
#' @description This function returns the scenarios of a given directory.
#' It removes the directory "results" from the list if it exists.
#' @param directory Name of the directory
#' @export
getScenarios <- function(directory){
  directories <- list.dirs(directory, recursive = FALSE)[-1]

  results <- which(endsWith(directories, "results"))
  if(any(results)) directories <- directories[-results]

  directories
}

#' @title Process a given directory with results from different scenarios
#' @description Each scenario is represented as a directory, and its results
#' are CSV files within them.
#' @param directory The directory where the scenarios are stored.
#' @param output The directory where the output is going to be saved. It
#' automatically appends "/results" to this directory. As default, the
#' output will be in the same directory where the scenarios are stored.
#' @param datafile A string with the shapefile that has all the CRs of
#' the output data. For each scenario, one shapefile will be created
#' containing this file plus the output of the respective scenario.
#' @export
processDirectory <- function(datafile, directory, output = directory){
  output = paste0(output, "/results")
  dir.create(output, showWarnings = FALSE)

  output <- normalizePath(output)
  directory <- normalizePath(directory)
  scenarios <- getScenarios(directory)

  if(length(scenarios) == 0) stop(paste0("Could not find any scenario in directory '", directory, "'"))

  for(scenario in scenarios){
    cat(paste0("Processing scenario '", basename(scenario), "'\n"))

    processScenario(datafile, scenario, output)
  }

  cat(paste0("The results were written in ", output, "'\n"))
}
