require(simu)

options(readr.num_columns = 0)

myshape <- system.file("extdata/shape", "simus.shp", package = "simu")

#scen = processDirectory(myshape, "~/Dropbox/REDD-PAC-INPE/WFS_data/R48", ".")

scen = processDirectory(myshape, system.file("extdata/results", package = "simu"), ".")

scen
