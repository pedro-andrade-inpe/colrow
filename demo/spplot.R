
require(rgdal)
require(RColorBrewer)
require(colrow)

brazil_cr <- system.file("extdata/results", "IDC_Amazon.shp",   package = "colrow") %>% readOGR()
biomes    <- system.file("extdata/shape", "br_biomes.shp",      package = "colrow") %>% readOGR()
states    <- system.file("extdata/shape", "br_states.shp",      package = "colrow") %>% readOGR()
amzlegal  <- system.file("extdata/shape", "legal_amazonia.shp", package = "colrow") %>% readOGR()

statessp   <- list("sp.polygons", states,   fill = "transparent", col = "black", add = TRUE)
amzlegalsp <- list("sp.polygons", amzlegal, fill = "transparent", col = "black", add = TRUE, lty = 2, first = FALSE)
biomessp   <- list("sp.polygons", biomes,   fill = "transparent", col = "black", add = TRUE, first = FALSE)

oranges      <- brewer.pal(9, "Oranges")
blues        <- brewer.pal(9, "Blues")
greenOranges <- c("green",brewer.pal(8, "Oranges"))
reds         <- brewer.pal(9, "Reds")
greys        <- brewer.pal(9, "Greys")
ylGn         <- brewer.pal(9, "YlGn")
rdPu         <- brewer.pal(9, "RdPu")

cuts <-c (0.0, 5.92, 16.76, 37.33, 71.11, 130.38, 211.54, 260, 294, 308)

spplot(
  brazil_cr[,"Rice2000"],
  at = cuts,
  col = "transparent", # remove this line to draw the border of each CR
  col.regions = oranges,
  sp.layout = list(biomessp, amzlegalsp)
)

cuts <- equalSteps(data = brazil_cr[,"Rice2000"], slices = 6)
cuts

spplot(
  brazil_cr[,"Rice2000"],
  at = cuts,
  col = "transparent", # remove this line to draw the border of each CR
  col.regions = oranges,
  sp.layout = list(biomessp, amzlegalsp)
)

