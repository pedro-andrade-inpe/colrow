
#' @title Creates a vector of strings from variable names
#' @description Convert a set of variable names in a vector of strings with their names.
#' This function is useful to name the attributes to be read from CSV files.
#' @param ... Names of the variables.
#' @export
attrs <-function(...) paste(substitute(list(...)))[-1]

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

#' @title Convert a given output csv file to a shapefile.
#' @description Process a fiven csv and convert it to a shapefile.
#' @param datafile Name of the input shapefile. This file must be created using getCR,
#' getLU, or getSimU.
#' @param csvfile l The output csv file to be exported.
#' @param outputFile Name of the shapefile to be saved.
#' @param description A vector of strings with the attribute names.
#' @param convertList A list whose indexes are attribute values from the csv file and whose
#' values are going to replace the attribute values from the csv file. Both attributes and
#' values must be string.
#' @export
processFile <- function(shapefile, csvfile, description, outputFile = NULL, convertList = NULL){
  cat(paste0("Reading shapefile: ", shapefile, "\n"))
  shp <- sf::read_sf(shapefile)

  cat(paste0("Reading data file: ", csvfile, "\n"))

  data <- suppressMessages(readr::read_csv(csvfile, col_names = description, progress = FALSE))
  ## error when the amount of attributes do not match the description

  # message when the amount of objects do not match the shp file
  shpid <- shp$ID
  dataid <- data$ID

  if(!dplyr::setequal(shpid, dataid)){
    diff1 <- dplyr::setdiff(shpid, dataid)

    ldiff1 <- length(diff1)
    if(ldiff1 > 0)
      message(ldiff1, " objects belong to the shapefile but not to the csv file: ", paste(diff1, collapse = ", "))

    diff2 <- dplyr::setdiff(dataid, shpid)

    ldiff2 <- length(diff2)
    if(ldiff2 > 0)
      message(ldiff2, "objects belong to the csv file but not to the shapefile: ", paste(diff2, collapse = ", "))
  }

  counts <- data %>% dplyr::summarise_all(dplyr::funs(dplyr::n_distinct(.)))

  removePos <- which(counts == 1)
  removed <- names(counts)[removePos]
  cat(paste0("Ignored attributes: ", paste(removed, collapse = ", "), "\n"))

  toBeJoined <- dplyr::setdiff(names(counts), c(removed, "ID", "VALUE"))

  cat(paste0("Attributes to be joined: ", paste(toBeJoined, collapse = ", "), "\n"))

  for(attr in toBeJoined){
    cat(paste0(attr, ": ", paste(unique(data[[attr]]), collapse = ", ")), "\n")
  }

  cat(paste0("Spreading the data\n"))

  result <- dplyr::select(data, -removePos) %>%
    tidyr::unite(KEY, toBeJoined, sep = "") %>%
    ## aqui criar um group_by e aplicar operacao de soma/media caso seja necessario
    tidyr::spread(key = KEY, VALUE)

  cat(paste0(dim(result)[2], " attributes were created\n"))

  cat(paste0("Merging the data\n"))
  output <- suppressMessages(dplyr::left_join(shp, result))

  nas <- length(which(is.na(output)))

  if(nas > 0){
    cat(paste0("Replacing ", nas, " NA values by zero\n"))

    output[is.na(output)] <- 0
  }

  if(is.null(outputFile))
     return(output)
  else{
    cat(paste0("Renaming attributes according to convertList\n"))
    mynames = names(output)

    for(name in names(convertList)){
      mynames <- stringr::str_replace_all(mynames, name, convertList[[name]])
    }

    names(output) <- mynames

    tooLarge <- mynames[nchar(mynames) > 10]

    if(length(tooLarge) > 0){
      cat(paste0(length(tooLarge), " attributes have more than 10 characters and will be truncated\n"))
      if(length(tooLarge) <= 20)
        cat(paste0("They are: ", paste(tooLarge, collapse = ", "), "\n"))
      else
        cat(paste0("The first 20 are: ", paste(tooLarge[1:20], collapse = ", "), "\n"))
    }
    else
      cat(paste0("No attribute has more than 10 characters\n"))

    cat(paste0("Writing output file: ", outputFile), "\n")
    sf::write_sf(output, outputFile, delete_layer = TRUE)
  }
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
