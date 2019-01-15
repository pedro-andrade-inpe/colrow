
data <- "c:/Users/pedro/Dropbox/colrow/"

countries <- colrow::getCountries(data)

total <- length(countries)

count <- 1
country <- countries[count]
cat(paste0(">>> Processing ", count, "/", total, ": ", country, "\n"))
oneCountry <- colrow::getCR(country, data, FALSE)
oneCountry[,"Country"] <- country

allCountries <- oneCountry

for(count in 2:total){
  country <- countries[count]
  cat(paste0(">>> Processing ", count, "/", total, ": ", country, "\n"))
  oneCountry <- colrow::getCR(country, data, FALSE)

  if(!is.null(oneCountry)){
    oneCountry[,"Country"] <- country

    allCountries <- rbind(allCountries, oneCountry)
  }
}

sf::write_sf(allCountries, "worldCR.shp")

#############################################################################

countries <- colrow::getCountries(data)

total <- length(countries)

count <- 1
country <- countries[count]
cat(paste0(">>> Processing ", count, "/", total, ": ", country, "\n"))
oneCountry <- colrow::getLU(country, data, FALSE)
oneCountry[,"Country"] <- country

allCountries <- oneCountry

for(count in 2:total){
  country <- countries[count]
  cat(paste0(">>> Processing ", count, "/", total, ": ", country, "\n"))
  oneCountry <- colrow::getLU(country, data, FALSE)

  if(!is.null(oneCountry)){
    oneCountry[,"Country"] <- country

    allCountries <- rbind(allCountries, oneCountry)
  }
}

sf::write_sf(allCountries, "worldLU.shp")

