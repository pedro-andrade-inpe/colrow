require(tmap)

system.time(result <- colrow::processFile(
  "BrazilCR.shp",
  "CROP_DATA_COMPARE_5yr_FC_Had_rcp8p5.CSV",
  colrow::attrs(COUNTRY, ID, ALTICLASS, SLPCLASS, SOILCLASS, XX1, USE, XX2, USE2, XXX3, SCENARIO, SCENARIO2, YEAR, VALUE)
))

names(result)

biomes <- system.file("extdata/shape", "br_biomes.shp", package = "colrow") %>% sf::read_sf()

rdPu <- RColorBrewer::brewer.pal(7, "RdPu")
cuts <-c (0, 1, 2, 3, 4, 5, 6, 7)

tm_shape(result) +
  tm_fill(col = "ARIDBeaDHIBeaD2040", palette = rdPu, breaks = cuts) +
  tm_shape(biomes) +
  tm_borders(lwd = 1, col = "black")
