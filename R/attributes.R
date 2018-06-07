

attribute_names <- function() return(list(
    ACR_COMPARE = c("COUNTRY", "ALLCOLROW", "ALTICLASS","SLPCLASS", "SOILCLASS",
                    "AEZCLASS", "SPECIES", "CROPTECH", "AllMacroScen", "AllBioenScen",
                    "IEA_SCEN", "ALLScenYear", "Value"),
    CROP_DATA_COMPARE = c("COUNTRY", "ALLCOLROW", "ALTICLASS", "SLPCLASS", "SOILCLASS",
                          "AEZCLASS", "SPECIES", "ALLTECH", "ALLITEM", "MacroScen",
                          "BioenScen", "IEA_SCEN", "ScenYear", "Value"),
    YLD_COMPARE = c("COUNTRY", "ALLCOLROW", "ALTICLASS", "SLPCLASS", "SOILCLASS",
                    "AEZCLASS", "SPECIES", "CROPTECH", "ALLPRODUCT", "AllMacroScen",
                    "AllBioenScen", "IEA_SCEN", "ALLScenYear", "Value"),
    Land_Compare3 = c("COUNTRY", "ALLCOLROW", "ALTICLASS", "SLPCLASS", "SOILCLASS",
                      "AEZCLASS", "LC_TYPE", "AllMacroScen", "AllBioenScen", "IEA_SCEN",
                      "ALLScenYear", "Value"),
    LUCDET_Compare = c("COUNTRY", "ALLCOLROW", "ALTICLASS", "SLPCLASS", "SOILCLASS",
                       "AEZCLASS", "LC_TYPE", "LC_CRNT", "AllMacroScen", "AllBioenScen",
                       "IEA_SCEN", "ALLScenYear", "Value"),
    LIVE_COMPARE = c("ANYREGION", "ALLCOLROW", "ALTICLASS", "SLPCLASS", "SOILCLASS",
                     "AEZCLASS", "LIVE_SYSTEM", "ANIMALS", "AllMacroScen", "AllBioenScen",
                     "IEA_SCEN", "ALLScenYear", "Value")
))

read_output_csv <- function(file)
{
    csv = read.csv(paste("OutputData/", file, ".CSV", sep=""), head=F, as.is=TRUE)
    colnames(csv) = attribute_names()[[file]]
    return(csv)
}


