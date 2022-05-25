context("relations")

test_that("relations", {
  dataDir <- "c:/Users/pedro/Dropbox/colrow"
  country <- "Brazil"

  myLU   <- getLU(country, dataDir)

  biomes <- system.file("extdata/shape", "br_biomes.shp", package = "colrow") %>% sf::read_sf()

  biomes$CD_LEGEN1[1] <- "AMAZONIA"
  biomes$CD_LEGEN1[6] <- "MATA_ATLANTICA"

  result <- buildRelations(myLU, biomes, "CD_LEGEN1", normalize = FALSE)

  expect(all(names(result) %in% c("ID1", "ID2", "area")), "names do not match")

  expect_equal(sum(result$area), units::set_units(837.3038, "Mha"), 0.0001)

  result <- normalizeRelationsByArea(result)



  View(result)

  ###############
  result <- result %>% dplyr::filter(ID1 == "LU09245")

  result2 <- result %>%
    mutate(area = units::drop_units(area)) %>%
    dplyr::group_by(ID1) %>%
    dplyr::mutate(area = area / sum(area)) %>%
    dplyr::arrange(ID1)

  View(result2)

  write.csv(result2, "result.csv")
  result3 <- result2 %>%
    group_by(ID1) %>%
    summarize(total = sum(new))

  all(result3$total == 1)

  result3$total[which(result3$total < 1)] - 1

  resultByLU <- result %>%
    group_by(ID1) %>%
    summarise(sarea = sum(new))

  max(resultByLU$sarea)

  resultByLU <- resultByLU %>% dplyr::transmute(ID = ID1, interArea = sarea)
  myLU$area <- sf::st_area(myLU) %>% units::set_units("Mha")

  myLU <- myLU %>%
    inner_join(resultByLU, by = "ID") %>%
    dplyr::mutate(coveredArea = units::drop_units(interArea / area))


  plot(myLU[,"coveredArea"])

  hist(myLU$coveredArea)

  small <- which(myLU$coveredArea < 0.9)

  selected <- myLU[small,]

  sf::write_sf(selected, "selected.shp")

  sf::write_sf(myLU, "mylu.shp")

})
