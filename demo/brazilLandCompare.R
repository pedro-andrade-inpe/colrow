
result <- colrow::processFile(
  "BrazilCR.shp",
  system.file("extdata/scenarios/FC/Land_Compare3_FC.csv", package = "colrow"),
  colrow::attrs(COUNTRY, ID, ALTICLASS, SLPCLASS, SOILCLASS, USE, SCENARIO, YEAR, VALUE)
)

plot(result["CrpLnd2000"])
names(result)
