## ---- eval = FALSE------------------------------------------------------------
#  devtools::install_github("pedro-andrade-inpe/colrow")

## -----------------------------------------------------------------------------
require(colrow)

## -----------------------------------------------------------------------------
dataDir <- "c:/Users/pedro/Dropbox/colrow"

## -----------------------------------------------------------------------------
list.files(dataDir)

## -----------------------------------------------------------------------------
colrow::getCountries(dataDir)[1:10]

## -----------------------------------------------------------------------------
country <- "Brazil"

myLU   <- colrow::getLU(country, dataDir)
myCR   <- colrow::getCR(country, dataDir)
mySimU <- colrow::getSimU(country, dataDir)

## -----------------------------------------------------------------------------
par(mfrow = c(1, 3), mar = c(5, 0.1, 5, 0.1))

sf::sf_use_s2(FALSE)

plot(sf::st_geometry(mySimU), main = "SimU (Grouped HRU in .5°x.5°)", col = "red"); box()
plot(sf::st_geometry(myCR),   main = "CR (.5°x.5°)", col = "blue"); box()
plot(sf::st_geometry(myLU),   main = "LU (2°x2°)", col ="green"); box()

## -----------------------------------------------------------------------------
sf::write_sf(myLU, paste0(country, "LU.shp"))
sf::write_sf(myCR, paste0(country, "CR.shp"))
sf::write_sf(mySimU, paste0(country, "SimU.shp"))

## -----------------------------------------------------------------------------
amazon <- c("Brazil", "Peru", "Colombia", "Venezuela",
            "Ecuador", "Bolivia", "Guyana",
            "Suriname", "French Guiana")

lu <- colrow::getLU(amazon, dataDir)

## -----------------------------------------------------------------------------
sf::write_sf(lu, "amazonLU.shp")

## -----------------------------------------------------------------------------
require(tmap)

tm_shape(lu) +
  tm_fill(col = "Country") +
  tm_borders(lwd = 1, col = "black")

## -----------------------------------------------------------------------------
lu %>%
  dplyr::filter(ID == "LU08515") %>%
  as.data.frame() %>%
  dplyr::select(Country)

## -----------------------------------------------------------------------------
csvfile <- system.file("extdata/scenarios/FC/Land_Compare3_FC.csv", package = "colrow")
attributes <- colrow::attrs(COUNTRY, ID, ALTI, SLP, SOIL, USE, SCENARIO, YEAR, VALUE)

result <- colrow::processFile("BrazilCR.shp", csvfile, attributes)

## -----------------------------------------------------------------------------
names(result)

## -----------------------------------------------------------------------------
tm_shape(result) +
  tm_fill(col = "CrpLnd2010")

## -----------------------------------------------------------------------------
colrow::processFile(
  "BrazilCR.shp",
  csvfile,
  attributes,
  "brazilLandCompare.shp"
)

## -----------------------------------------------------------------------------
convert <- list(
  CrpLnd = "cr", PriFor = "pr",
  NatLnd = "nl", ForReg = "fr",
  GrsLnd = "gl", MngFor = "mf",
  PltFor = "pl" 
)

## -----------------------------------------------------------------------------
for(year in paste(seq(2000, 2050, 10))) # from 2000, 2010, ..., 2050
  convert[[year]] = substr(year, 3, 4) # to 00, 10, ..., 50

unlist(convert)

## -----------------------------------------------------------------------------
colrow::processFile(
  "BrazilCR.shp",
  csvfile,
  attributes,
  "brazilOutput.shp",
  convert
)

## -----------------------------------------------------------------------------
result <- colrow::processFile(
  "BrazilCR.shp",
  system.file("extdata/csv/YIELD_COMPARE2.CSV", package = "colrow"),
  colrow::attrs(ID, CROP, ScenYear, VALUE),
  aggregate = sum # default value, could be omitted
)

## -----------------------------------------------------------------------------
brazil <- sf::read_sf("brazilLandCompare.shp")
biomes <- system.file("extdata/shape", "br_biomes.shp", package = "colrow") %>% sf::read_sf()

## -----------------------------------------------------------------------------
max(brazil$GrsLnd2010)

cuts <- c(0, 50, 100, 150, 200, 250, 305)
rdPu <- RColorBrewer::brewer.pal(6, "RdPu")

## -----------------------------------------------------------------------------
tm_shape(brazil) +
  tm_fill(col = "GrsLnd2010", palette = rdPu, breaks = cuts, title = "Grass 2010") +
  tm_shape(biomes) +
  tm_borders(lwd = 1, col = "black")

## -----------------------------------------------------------------------------
amazCerradoBox <- c(xmin = -74.5, xmax = -41.4, ymin = -25, ymax = 5.5) %>%
  sf::st_bbox(crs = st_crs(4326))

tm_shape(brazil, bbox = amazCerradoBox) +
  tm_fill(col = "GrsLnd2010", palette = rdPu, title = "Grass 2010") +
  tm_shape(biomes) +
  tm_borders(lwd = 1, col = "black")

## -----------------------------------------------------------------------------
result <- tm_shape(brazil, bbox = amazCerradoBox) +
  tm_fill(col = "GrsLnd2010", palette = rdPu, title = "Grass 2010") +
  tm_shape(biomes) +
  tm_borders(lwd = 1, col = "black")

tmap_save(result, "amaz-cerrado.png")

## -----------------------------------------------------------------------------
require(tmap)

plotAll(brazil, "GrsLnd", palette = "RdPu", title = "Grass", additional =     
  tm_shape(biomes) +
  tm_borders(lwd = 1, col = "black")
)

## -----------------------------------------------------------------------------
data <- colrow::readCSV(
  system.file("extdata/scenarios/FC/Land_Compare3_FC.csv", package = "colrow"),
  colrow::attrs(COUNTRY, ID, ALTICLASS, SLPCLASS, SOILCLASS, USE, SCENARIO, YEAR, VALUE)
)

## -----------------------------------------------------------------------------
unique(data$USE)
unique(data$YEAR)

## -----------------------------------------------------------------------------
result <- data %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(VALUE = sum(VALUE))

result

## -----------------------------------------------------------------------------
result <- data %>%
  dplyr::group_by(YEAR, USE) %>%
  dplyr::summarise(VALUE = sum(VALUE)) %>%  # sum by year/use
  dplyr::mutate(Year = YEAR, Use = USE) %>% # rename variables
  dplyr::mutate(Use = dplyr::recode(Use,    # rename attributes
    CrpLnd = "Crop Land",
    ForReg = "Forest Regrowth",
    GrsLnd = "Grass Land",
    MngFor = "Managed Forest",
    NatLnd = "Natural Land",
    PltFor = "Planted Forest",
    PriFor = "Primary Forest"))

result

## -----------------------------------------------------------------------------
require(ggplot2)

ggplot(result) +
  aes(x = Year, y = VALUE, colour = Use) +
  geom_line(lwd = 1.5) +
  theme_bw() +
  ylab("Total (ha)")

## -----------------------------------------------------------------------------
amaz <- biomes[1,] %>% sf::st_transform(crs = sf::st_crs(myLU))
subset <- myLU[apply(sf::st_intersects(myLU, amaz), 1, any),]

result <- tm_shape(subset) +
  tm_borders(lwd = 1, col = "black") +
  tm_fill(col = "blue") +
  tm_shape(amaz) +
  tm_borders(lwd = 2, col = "black")

result

