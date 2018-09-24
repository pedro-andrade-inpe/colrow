
# First download data from:
# https://www.dropbox.com/sh/sqocqe45jwmug2p/AAAbv-IAg24a_R4vYsP9zqV_a?dl=0

data <- "c:/Users/pedro/Dropbox/colrow/"

#colrow::getCountries(data) # All the available countries

country <- "Brazil"

myLU   <- colrow::getLU  (country, data)
myCR   <- colrow::getCR  (country, data)
mySimU <- colrow::getSimU(country, data)

par(mfrow = c(1, 3), mar = c(5, 0.1, 5, 0.1))

plot(sf::st_geometry(mySimU), main = "SimU", col = "red"); box()
plot(sf::st_geometry(myCR),   main = "CR", col = "blue");   box()
plot(sf::st_geometry(myLU),   main = "LU", col ="green");   box()

sf::write_sf(myLU, paste0(country, "LU.shp"))
sf::write_sf(myCR, paste0(country, "CR.shp"))
sf::write_sf(mySimU, paste0(country, "SimU.shp"))

