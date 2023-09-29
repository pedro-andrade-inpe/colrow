
require(colrow)
require(magrittr)

brazil_cr <- system.file("extdata/results", "IDC_Amazon.shp",   package = "colrow") %>% sf::read_sf()
biomes    <- system.file("extdata/shape", "br_biomes.shp",      package = "colrow") %>% sf::read_sf()
states    <- system.file("extdata/shape", "br_states.shp",      package = "colrow") %>% sf::read_sf()
amzlegal  <- system.file("extdata/shape", "legal_amazonia.shp", package = "colrow") %>% sf::read_sf()

brazil_cr <- sf::st_make_valid(brazil_cr)

oranges      <- RColorBrewer::brewer.pal(9, "Oranges")
blues        <- RColorBrewer::brewer.pal(9, "Blues")
greenOranges <- c("green", RColorBrewer::brewer.pal(8, "Oranges"))
reds         <- RColorBrewer::brewer.pal(9, "Reds")
greys        <- RColorBrewer::brewer.pal(9, "Greys")
ylGn         <- RColorBrewer::brewer.pal(9, "YlGn")
rdPu         <- RColorBrewer::brewer.pal(9, "RdPu")

cuts <-c (0.0, 5.92, 16.76, 37.33, 71.11, 130.38, 211.54, 260, 294, 308)

tmap::tm_shape(brazil_cr) +
  tmap::tm_fill(col = "Rice2000", breaks = cuts) +
  tmap::tm_layout(legend.outside = TRUE) +
  tmap::tm_shape(amzlegal) +
  tmap::tm_borders(lwd = 2) +
  tmap::tm_shape(biomes) +
  tmap::tm_borders(lwd = 1)

cuts <- colrow::equalSteps(data = brazil_cr[, "Rice2000"], slices = 6)
cuts

tmap::tm_shape(brazil_cr) +
  tmap::tm_fill(col = "Rice2000", breaks = cuts) +
  tmap::tm_layout(legend.outside = TRUE) +
  tmap::tm_shape(amzlegal) +
  tmap::tm_borders(lwd = 2) +
  tmap::tm_shape(biomes) +
  tmap::tm_borders(lwd = 1)
