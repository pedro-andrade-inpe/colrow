require(colrow)

options(readr.num_columns = 0)

myshape <- system.file("extdata/shape", "brazil_cr.shp", package = "colrow")

# processDirectory(myshape, "~/Dropbox/REDD-PAC-INPE/WFS_data/R48", ".")

processDirectory(myshape, system.file("extdata/scenarios", package = "colrow"), ".")
