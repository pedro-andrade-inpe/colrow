
require(colrow)
dataDirectory <- "c:/Users/pedro/Dropbox/colrow/"
countryName <- "Brazil"

coords <- getCRcentroids(countryName, dataDirectory) %>%
  as.data.frame %>%
  dplyr::select(XCOORD, YCOORD)

write.csv(coords, "coords-cr-brazil.csv")
