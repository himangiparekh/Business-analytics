library(dplyr)
library(rlang)
if (!require("devtools")) install.packages("devtools")
devtools::has_devel()
library(devtools)
devtools::install_github("soerenkuenzel/forestry")
devtools::install_github("forestry-labs/causalToolbox")
library(forestry)

library(causalToolbox)
packageVersion("causalToolbox")

load("/Users/himangiparekh/Desktop/GitHub/Supply_Chain-/Data/Clean World Bank Data/finalDF_analysis_noNeg.RData")



