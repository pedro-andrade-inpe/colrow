
require(ggplot2)

data <- colrow::readCSV(
  system.file("extdata/scenarios/FC/Land_Compare3_FC.csv", package = "colrow"),
  colrow::attrs(COUNTRY, ID, ALTICLASS, SLPCLASS, SOILCLASS, USE, SCENARIO, YEAR, VALUE)
)

unique(data$USE)
unique(data$YEAR)

result <- data %>%
  dplyr::group_by(YEAR, USE) %>%
  dplyr::summarise(VALUE = sum(VALUE)) %>%
  dplyr::mutate(Year = YEAR, Use = USE) %>%
  dplyr::mutate(Use = dplyr::recode(Use,
    CrpLnd = "Crop Land",
    ForReg = "Forest Regrowth",
    GrsLnd = "Grass Land",
    MngFor = "Managed Forest",
    NatLnd = "Natural Land",
    PltFor = "Planted Forest",
    PriFor = "Primary Forest"))

ggplot(result) +
  aes(x = Year, y = VALUE, colour = Use) +
  geom_line(lwd = 1.5) +
  theme_bw() +
  ylab("Total (ha)")
