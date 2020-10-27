
.onAttach <- function(lib, pkg){
    packageStartupMessage("colrow - Handling SimU, CR, and LU data")
    packageStartupMessage(sprintf("Version %s is now loaded",
                                  utils::packageDescription("colrow")$Version))
}

utils::globalVariables(c("%>%", ".", "ALLCOUNTRY", "COUNTRY", "ColRow", "Country", "ID", "KEY",
                       "SimU", "SimUID", "VALUE", "X", "Y", "LU", "values"))
