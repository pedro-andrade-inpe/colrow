context("relations")

test_that("relations", {
  dataDir <- "c:/Users/pedro/Dropbox/colrow"
  country <- "Brazil"

  myLU   <- getLU(country, dataDir)

  biomes <- system.file("extdata/shape", "br_biomes.shp", package = "colrow") %>% sf::read_sf()

  biomes$CD_LEGEN1[1] <- "AMAZONIA"
  biomes$CD_LEGEN1[6] <- "MATA_ATLANTICA"

  result <- buildRelations(myLU, biomes, "CD_LEGEN1")

  expect(all(names(result) %in% c("ID1", "ID2", "area")), "names do not match")

  expect_equal(sum(result$area), units::set_units(837.3038, "Mha"), 0.0001)
})
