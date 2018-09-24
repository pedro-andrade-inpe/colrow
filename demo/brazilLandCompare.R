
require(tmap)

result <- colrow::processFile(
  "BrazilCR.shp",
  system.file("extdata/scenarios/FC/Land_Compare3_FC.csv", package = "colrow"),
  colrow::attrs(COUNTRY, ID, ALTICLASS, SLPCLASS, SOILCLASS, USE, SCENARIO, YEAR, VALUE)
)

rdPu <- RColorBrewer::brewer.pal(6, "RdPu")
cuts <-c (0, 50, 100, 150, 200, 250, 305)

biomes <- system.file("extdata/shape", "br_biomes.shp", package = "colrow") %>% sf::read_sf()

tm_shape(result) +
  tm_fill(col = "GrsLnd2010", palette = rdPu, breaks = cuts, title = "Grass 2010") +
  tm_shape(biomes) +
  tm_borders(lwd = 1, col = "black")
