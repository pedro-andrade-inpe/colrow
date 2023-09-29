
convert = list(
  Corn = "CO", Rice = "RI", Soya = "SO", Cass = "CA", SugC = "SU",
  BeaD = "BE", SwPo = "SP", Cott = "CT", Gnut = "GT", OPAL = "OP",
  Srgh = "SR", Whea = "WH", Pota = "PO", Barl = "BA",
  BaseArea = "BA", IR_basin = "IRB", IR_furrow = "IRF",
  IR_drip = "IRD", IR_sprink = "IRS",
  TEMP = "TM",  OTHR = "OT",  HUMI = "HU",  ARID = "AR"
)

for(year in paste(seq(2000, 2050, 5)))
  convert[[year]] = substr(year, 3, 4)

result <- colrow::processFile(
  "BrazilCR.shp",
  "CROP_DATA_COMPARE_5yr_FC_Had_rcp8p5.CSV",
  colrow::attrs(COUNTRY, ID, ALTICLASS, SLPCLASS, SOILCLASS, XX1, USE, XX2, USE2, XXX3, SCENARIO, SCENARIO2, YEAR, VALUE)
)

data <- sf::as_Spatial(result)

biomes   <- sf::read_sf(system.file("extdata/shape", "br_biomes.shp", package = "colrow"))

biomessp <- list("sp.polygons", biomes,   fill = "transparent", col = "black", add = TRUE, first = FALSE)

ylGn <- RColorBrewer::brewer.pal(7, "RdPu")

cuts <-c (0, 1, 2, 3, 4, 5, 6, 7)

spplot(
  data[,"ARIDBeaDHIBeaD2040"],
  at = cuts,
  col = "transparent", # remove this line to draw the border of each CR
  col.regions = ylGn,
  sp.layout = list(biomessp)
)
