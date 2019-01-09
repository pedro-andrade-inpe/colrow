require(tmap)
require(dplyr)

result <- colrow::processFile(
  "BrazilCR.shp",
  system.file("extdata/csv/YIELD_COMPARE2.CSV", package = "colrow"),
  colrow::attrs(ID,CROP,ScenYear,VALUE),
  aggregate = sum
)

max(result$Corn2010)

rdPu <- RColorBrewer::brewer.pal(6, "RdPu")

biomes <- system.file("extdata/shape", "br_biomes.shp", package = "colrow") %>% sf::read_sf()

cerradoBox <- sf::st_bbox(c(xmin = -60.3, xmax = -41.4, ymin = -25, ymax = -2), crs = st_crs(4326))
amazBox <- sf::st_bbox(c(xmin = -74.5, xmax = -43, ymin = -17, ymax = 5.5), crs = st_crs(4326))
amazCerradoBox <- sf::st_bbox(c(xmin = -74.5, xmax = -41.4, ymin = -25, ymax = 5.5), crs = st_crs(4326))

tm_shape(result, bbox = amazCerradoBox) +
  tm_fill(col = "Corn2010", palette = rdPu, title = "Corn 2020") +
  tm_shape(biomes) +
  tm_borders(lwd = 1, col = "black")
