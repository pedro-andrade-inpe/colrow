
require(colrow)

options(readr.num_columns = 0)

mydir <- system.file("extdata/scenarios/FC", package = "colrow")

data <- readCR(mydir, "ACR_COMPARE")

data
