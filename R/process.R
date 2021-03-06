
#' @title Creates a vector of strings from variable names
#' @description Convert a set of variable names in a vector of strings with their names.
#' This function is useful to name the attributes to be read from CSV files.
#' @param ... Names of the variables.
#' @export
attrs <-function(...) {
  result <- paste(substitute(list(...)))[-1]

  if("VALUE" %in% result) return(result)
  else stop("There should exist one attribute called 'VALUE'.")
}

#' @title Read CR from a CSV file to tibble
#' @description Read a CSV file as a tibble. The column names are identified automatically by
#' colrow package.
#' @param csvfile Path where the data is stored. Note that the last directory name will be the scenario's name.
#' @param product Name of the product to be read. The file name will be 'product_scenario.CSV'
#' @export
readCSV <- function(csvfile, product){
  suppressMessages(readr::read_csv(csvfile, col_names = product, progress = FALSE))
}

#' @title Convert a given output csv file to a shapefile.
#' @description Process a fiven csv and convert it to a shapefile.
#' @param shapefile Name of the input shapefile. This file must be created using getCR,
#' getLU, or getSimU.
#' @param csvfile l The output csv file to be exported.
#' @param outputfile Name of the shapefile to be saved.
#' @param description A vector of strings with the attribute names.
#' It is possible to use attrs() to help creating this description.
#' @param convertList A list whose indexes are attribute values from the csv file and whose
#' values are going to replace the attribute values from the csv file. Both attributes and
#' values must be string.
#' @param aggregate A function to be used to join values with the same ID and attributes
#' to be joinded. As default, it will use sum, but it is possible to use functions such as
#' average.
#' @param ignore A vector of attributes to be ignored. For example, when processing the
#' whole World, usually there is a column with the country name, which is not unique and
#' therefore it will not be ignored as default. Such attribute must be included in this
#' argument to avoid creating one attribute for each country. Default is NULL.
#' @param toAggregate Attributes to be used only to aggregate, ignoring their values in
#' the created attribute names.
#' @export
processFile <- function(shapefile, csvfile, description, outputfile = NULL, convertList = NULL, aggregate = sum, ignore = NULL, toAggregate = NULL) {
  if(class(description) != "character") stop(paste0("Argument 'description' should be 'character', got '", class(description), "'."))

  cat(paste0("Reading shapefile: ", shapefile, "\n"))
  shp <- sf::read_sf(shapefile)

  cat(paste0("Reading data file: ", csvfile, "\n"))

  data <- readCSV(csvfile, product = description)

  # message when the amount of objects do not match the shp file
  shp$ID <- as.character(shp$ID)
  data$ID <- as.character(data$ID)

  shpid <- shp$ID
  dataid <- data$ID

  if(!dplyr::setequal(shpid, dataid)){
    diff1 <- dplyr::setdiff(shpid, dataid)

    ldiff1 <- length(diff1)

    if(ldiff1 == length(shpid))
      stop("No ID from shapefile matches CSV data. Please verify your representation or spatia area.")

    if(ldiff1 > 0)
      message(ldiff1, " objects belong to the shapefile but not to the csv file: ", paste(diff1, collapse = ", "))

    diff2 <- dplyr::setdiff(dataid, shpid)

    ldiff2 <- length(diff2)
    if(ldiff2 > 0)
      message(ldiff2, "objects belong to the csv file but not to the shapefile: ", paste(diff2, collapse = ", "))
  }

  counts <- data %>% dplyr::summarise_all(dplyr::n_distinct)

  removePos <- which(counts == 1)
  removed <- c(names(counts)[removePos], ignore)
  cat(paste0("Ignored attributes: ", paste(removed, collapse = ", "), "\n"))

  toBeJoined <- dplyr::setdiff(names(counts), c(removed, "ID", "VALUE", toAggregate))

  cat(paste0("Attributes to be joined: ", paste(toBeJoined, collapse = ", "), "\n"))

  for(attr in toBeJoined){
    cat(paste0(attr, ": ", paste(unique(data[[attr]]), collapse = ", ")), "\n")
  }

  cat(paste0("Spreading the data\n"))

  if(length(toBeJoined) == 0){
    result <- dplyr::select(data, -removePos) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(VALUE = aggregate(VALUE))
  }else{
    result <- dplyr::select(data, -removePos) %>%
      tidyr::unite(KEY, toBeJoined, sep = "") %>%
      dplyr::group_by(ID, KEY) %>%
      dplyr::summarise(VALUE = aggregate(VALUE)) %>%
      tidyr::pivot_wider(names_from = KEY, values_from = VALUE)
  }

  cat(paste0(dim(result)[2], " attributes were created\n"))

  cat(paste0("Merging the data\n"))
  output <- suppressMessages(dplyr::left_join(shp, result))

  nas <- length(which(is.na(output)))

  if(nas > 0){
    cat(paste0("Replacing ", nas, " NA values by zero\n"))

    output[is.na(output)] <- 0
  }

  if(is.null(outputfile))
     return(output)
  else{
    cat(paste0("Renaming attributes according to convertList\n"))
    mynames <- names(output)

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

    cat(paste0("Writing output file: ", outputfile), "\n")
    sf::write_sf(output, outputfile, delete_layer = TRUE)
  }
}

processScenario <- function(shapefile, scenario, output){
  shp <- rgdal::readOGR(shapefile, encoding = "ESRI Shapefile", verbose = FALSE)

  shp@data[is.na(shp@data)] <- 0.0

  cat(paste0("Writing file '", basename(scenario), ".shp'\n"))

  # FIXME use sf instead
  rgdal::writeOGR(shp, output, basename(scenario), driver = "ESRI Shapefile", overwrite_layer = TRUE)
}

#' @title Return the scenarios of a directory
#' @description This function returns the scenarios of a given directory. Each
#' scenario is stored as a directory.
#' It only removes the directory "results" from the list of directories if it exists.
#' @param directory Name of the directory
#' @export
getScenarios <- function(directory){
  directories <- list.dirs(directory, recursive = FALSE)

  results <- which(endsWith(directories, "results"))
  if(any(results)) directories <- directories[-results]

  basename(directories)
}

#' @title Process a given directory with results from different scenarios
#' @description Each scenario is represented as a directory, and its results
#' are CSV files within them.
#' @param shapefile A string with the shapefile that has all the CRs of
#' the output data. For each scenario, one shapefile will be created
#' containing this file plus the output of the respective scenario.
#' @param directory The directory where the scenarios are stored.
#' @param output The directory where the output is going to be saved. It
#' automatically appends "/results" to this directory. As default, the
#' output will be in the same directory where the scenarios are stored.
#' @export
processDirectory <- function(shapefile, directory, output = directory){
  output <- paste0(output, "/results")
  dir.create(output, showWarnings = FALSE)

  output <- normalizePath(output)
  directory <- normalizePath(directory)
  scenarios <- getScenarios(directory)

  if(length(scenarios) == 0)
    stop(paste0("Could not find any scenario in directory '", directory, "'"))

  for(scenario in scenarios){
    cat(paste0("Processing scenario '", basename(scenario), "'\n"))

    processScenario(shapefile, scenario, output)
  }

  cat(paste0("The results were written in ", output, "'\n"))
}
