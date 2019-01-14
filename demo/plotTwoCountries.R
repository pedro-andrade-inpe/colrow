
data <- "c:/Users/pedro/Dropbox/colrow/"

country <- "Brazil"
brazil   <- colrow::getCR(country, data)

country <- "Colombia"

colombia   <- colrow::getCR(country, data)

brazil_colombia <- rbind(brazil, colombia)

require(tmap)

tm_shape(brazil_colombia) + tm_polygons()
