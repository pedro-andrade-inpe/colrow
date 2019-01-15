
require(tmap)

data <- "c:/Users/pedro/Dropbox/colrow/"

amazon <- c("Brazil", "Peru", "Colombia", "Venezuela", "Ecuador", "Bolivia",
            "Guyana", "Suriname", "French Guiana")

cr <- colrow::getCR(amazon, data)

tm_shape(cr) +
  tm_fill(col = "Country") +
  tm_borders(lwd = 1, col = "black")

lu <- colrow::getLU(amazon, data)

tm_shape(lu) +
  tm_fill(col = "Country") +
  tm_borders(lwd = 1, col = "black")

su <- colrow::getSimU(amazon[7:9], data)

tm_shape(su) +
  tm_fill(col = "Country") +
  tm_borders(lwd = 1, col = "black")

