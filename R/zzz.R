
.onAttach <- function(lib, pkg){
    packageStartupMessage("colrow - Handling SimU, CR, and LU data")
    packageStartupMessage(sprintf("Version %s is now loaded", utils::packageDescription("colrow")$Version))
}

utils::globalVariables("%>%")

