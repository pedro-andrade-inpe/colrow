
.onAttach <- function(lib, pkg){
    packageStartupMessage("simu - Handling SimU and CR data")
    packageStartupMessage(sprintf("Version %s is now loaded", utils::packageDescription("simu")$Version))

}

utils::globalVariables("%>%")
