

data <- "c:/Users/pedro/Dropbox/colrow/"

simu <- sf::read_sf(paste0(data, "SimU_all.shp"))

countries <- colrow::getCountries(data)

total <- length(countries)

count <- 1
country <- countries[count]
cat(paste0(">>> Processing ", count, "/", total, ": ", country, "\n"))
oneCountry <- colrow::getCR(country, data, FALSE, simu)
oneCountry[,"Country"] <- country

allCountries <- oneCountry

for(count in 2:total){
  country <- countries[count]
  cat(paste0(">>> Processing ", count, "/", total, ": ", country, "\n"))
  oneCountry <- colrow::getCR(country, data, FALSE, simu)

  if(!is.null(oneCountry)){
    oneCountry[,"Country"] <- country

    allCountries <- rbind(allCountries, oneCountry)
  }
}

sf::write_sf(allCountries, paste0(data, "worldCR.shp"))

#############################################################################

countries <- colrow::getCountries(data)

total <- length(countries)

count <- 1
country <- countries[count]
cat(paste0(">>> Processing ", count, "/", total, ": ", country, "\n"))
oneCountry <- colrow::getLU(country, data, FALSE, simu)
oneCountry[,"Country"] <- country

allCountries <- oneCountry

for(count in 2:total){
  country <- countries[count]
  cat(paste0(">>> Processing ", count, "/", total, ": ", country, "\n"))
  oneCountry <- colrow::getLU(country, data, FALSE, simu)

  if(!is.null(oneCountry)){
    oneCountry[,"Country"] <- country

    allCountries <- rbind(allCountries, oneCountry)
  }
}

sf::write_sf(allCountries, paste0(data, "worldLU.shp"))
