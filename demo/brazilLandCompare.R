
result <- colrow::processFile(
  "BrazilCR.shp",
  system.file("extdata/scenarios/FC/Land_Compare3_FC2.csv", package = "colrow"),
  c("COUNTRY", "ID", "ALTICLASS","SLPCLASS", "SOILCLASS", "USE", "SCENARIO", "YEAR", "VALUE")
)

plot(result["CrpLnd2000"])
