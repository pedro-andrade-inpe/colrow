
require(colrow)
dataDirectory <- "c:/Users/pedro/Dropbox/colrow/"
countryName <- "Brazil"

coords <- getCRcentroids(countryName, dataDirectory) %>%
  as.data.frame %>%
  dplyr::select(XCOORD, YCOORD)

coords$XCOORD <- round(coords$XCOORD, 2)
coords$YCOORD <- round(coords$YCOORD, 2)

write.csv(coords, "coords-cr-brazil.csv")
