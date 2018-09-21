
options(readr.num_columns = 0)

convert = list(
  GrsLnd = "gl", CrpLnd = "cr", WetLnd = "wl",
  MngFor = "mf", NatLnd = "nl", NotRel = "nr",
  PriFor = "pr", PltFor = "pl", OagLnd = "oa",
  TEMP = "TM",  OTHR = "OT",  HUMI = "HU",  ARID = "AR"
)

colrow::processFile(
  "ColombiaLU.shp",
  "colombia/FC/Land_Compare3_FC.CSV",
  colrow::attrs(COUNTRY, ID, ALTICLASS, SLPCLASS, SOILCLASS, XX1, XX2, XX3, USE, SCENARIO, YEAR, VALUE),
  "myOutput2.shp",
  convertList = convert
)
