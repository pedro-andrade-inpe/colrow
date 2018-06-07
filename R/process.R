
extractUse <- function (alldata, use, year) {
  var_name <- paste(use, year, sep = "")

  result <- alldata %>%
    select(ALLCOLROW, USE, SCENARIO, YEAR, VALUE) %>%
    filter(YEAR == year) %>%
    filter(USE == use) %>%
    select(ALLCOLROW, VALUE)

  colnames(result) <- c("colrow", var_name)
  return(result)
}

processUse <- function(alldata){
  years <- unique(alldata$YEAR)
  crops <- unique(alldata$USE)

  res <- unique(alldata["ALLCOLROW"])
  colnames(res) <- c("colrow")

  for (c in crops) {
    for (y in years) {
      res1 <- extractUse(alldata, c, y)
      res <- left_join(res, res1, by = "colrow")
    }
  }

  return(res)
}

processProduct <- function(product_data, attrname){
  extract_one <- function(alldata, year) {
    var_name <- paste(attrname, year, sep = "")

    result <- alldata %>%
      select(ALLCOLROW, SCENARIO, YEAR, VALUE) %>%
      filter(YEAR == year) %>%
      select(ALLCOLROW, VALUE)

    colnames(result) <- c("colrow", var_name)
    return(result)
  }

  years <- unique(product_data$YEAR)

  res <- unique(product_data["ALLCOLROW"])
  colnames(res) <- c("colrow")

  for(y in years) {
    res1 <- extract_one(product_data, y)
    res <- left_join(res, res1, by = "colrow")
  }

  return(res)
}

processScenario <- function(scenario, output){
  shp <- readOGR(system.file("extdata/shape", "brasil_cr.shp", package = "simu"), encoding = "ESRI Shapefile")
  names(shp) = "colrow"

  uses <- c("ACR_COMPARE", "Land_Compare3")

  for(product in uses){
    data_file <- paste0(scenario, "/", product, "_", basename(scenario), ".CSV")
    data <- read_csv(data_file, col_names = attr_names[[product]])
    result <- processUse(data)

    result

    shp <- sp::merge(shp, result, by = "colrow")
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
    data_file <- paste(scenario, "/", product, "_", basename(scenario), ".CSV", sep = "")
    product_data <- read_csv(data_file, col_names = attr_names[[product]])
    res <- processProduct(product_data, names[product])

    shp <- sp::merge(shp, res, by = "colrow")
  }

  shp@data[is.na(shp@data)] <- 0.0

  writeOGR(shp, output, basename(scenario), driver = "ESRI Shapefile", overwrite_layer = TRUE)
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
#' @export
processDirectory <- function(directory, output = paste0(directory, "/results")){
  dir.create(output, showWarnings = FALSE)

  output <- normalizePath(output)

  scenarios <- getScenarios(directory)

  if(length(scenarios) == 0) stop(paste0("Could not find any scenario in directory '", directory, "'"))

  for(scenario in scenarios){
    cat(paste0("Processing scenario '", basename(scenario), "'\n"))

    processScenario(scenario, output)
  }

  cat(paste0("The results were written in ", output, "'\n"))
}

