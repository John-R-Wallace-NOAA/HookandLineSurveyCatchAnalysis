

remotes::install_github("John-R-Wallace-NOAA/HookandLineSurveyCatchAnalysis") 

# Required packages listed here for help with installation (other packages may be installed by these packages).
require(lattice)
require(maptools)
require(gtools) 
require(MASS) 
require(gam)
require(MCMCpack)
require(Hmisc)
require(chron)
require(RCurl)
require(HookandLineSurveyCatchAnalysis) 

   
# Take an intial look at the data
Grand.2019 <- importData(grandPathCSV = "qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
Grand.2019[1:3, ]

# Find a common name to run the analysis on
table(Grand.2019$common_name, useNA = "ifany")

     
# Micro to test.  Unfortunately, 'reducedFormula' needs to TRUE for lack of variables in the current database.
Boc.2019.NFT.1k.121 <- HandL.stepAIC.MCMC(Y.Name = "NumBoc", common_name = "Bocaccio", Area = c('Orig121', 'CCA', 'ALL')[1], reducedFormula = TRUE, Include.FishTime = FALSE,
                             buffer = c(15, 45), tune = 0.14, mcmc = 1000, burnin = 10, thin = 2, verbose = 100, 
                             grandPathCSV = "W:/ALL_USR/JRW/Hook & Line Survey/2020/DATA/qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
  
# 150k MCMC iterations
Boc.2019.NFT.150k.121 <- HandL.stepAIC.MCMC(Y.Name = "NumBoc", common_name = "Bocaccio", Area = c('Orig121', 'CCA', 'ALL')[1], reducedFormula = TRUE, Include.FishTime = FALSE,
                             buffer = c(15, 45), tune = 0.14, mcmc = 150000, burnin = 1000, thin = 150, verbose = 1000, 
                             grandPathCSV = "W:/ALL_USR/JRW/Hook & Line Survey/2020/DATA/qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
                                             

