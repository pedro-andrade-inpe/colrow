
.onAttach <- function(lib, pkg){
    packageStartupMessage("colrow - Handling ColRow (CR) data")
    packageStartupMessage(sprintf("Version %s is now loaded", utils::packageDescription("colrow")$Version))
}

utils::globalVariables("%>%")

