

# Early requires to check for packages

require(lattice)
require(maptools)
require(gtools) 
require(MASS) 
require(gam)
require(MCMCpack)
require(Hmisc)


# Take an intial look at the data
Grand.2019 <- importData(grandPathCSV = "qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
Grand.2019[1:3, ]

# Find a common name to run the analysis on
table(Grand.2019$common_name, , useNA = "ifany")

     
# Micro to test.  Unfortunately, 'reducedFormula' needs to TRUE for lack of variables in the current database.
Boc.2019.NFT.1k.121 <- HandL.stepAIC.MCMC(Y.Name = "NumBoc", common_name = "Bocaccio", Area = c('Orig121', 'CCA', 'ALL')[1], Include.FishTime = FALSE, 
                             propHookCutOffAggr = ifelse(Area == "ALL", 0.002, 0.003), propHookCutOffMirage = propHookCutOffAggr, propHookCutOffToro = 3 * propHookCutOffAggr,
                             reducedFormula = TRUE, buffer = c(15, 45), tune = 0.14, mcmc = 1000, burnin = 10, thin = 2, verbose = 100, 
                             grandPathCSV = "W:/ALL_USR/JRW/Hook & Line Survey/2020/DATA/qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
  
                                           
# 105k MCMC - use GLM.FINAL.AIC from the micro run above
Boc.2019.NFT.150k.121 <- HandL.stepAIC.MCMC(GLM.FINAL.AIC = Boc.Final.Glm, tune = 0.14, mcmc = 150000, burnin = 1000, thin = 150, verbose = 1000)
  
  
  
# --- Full runs with all args ---- 
  
# Micro to test
Boc.2019.NFT.1k.121 <- HandL.stepAIC.MCMC(Y.Name = "NumBoc", common_name = "Bocaccio", Area = c('Orig121', 'CCA', 'ALL')[1], Include.FishTime = FALSE, Restrict.6min = FALSE, Sites = NULL, 
                             propHookCutOffAggr = ifelse(Area == "ALL", 0.002, 0.003), propHookCutOffMirage = propHookCutOffAggr, propHookCutOffToro = 3 * propHookCutOffAggr,
                             VermComplex = FALSE, Interaction = TRUE, reducedFormula = TRUE, buffer = c(15, 45), contrast = 'treatment', tune = 0.14, mcmc = 1000, burnin = 10, 
                             thin = 2, verbose = 100, Stop.before.MCMC = FALSE, MAIN.STEP.AIC = NULL, STEP.AIC = NULL, GLM.FINAL.AIC = NULL, 
                             grandPathCSV = "W:/ALL_USR/JRW/Hook & Line Survey/2020/DATA/qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
                             
# 150k MCMC   
Boc.2019.NFT.150k.121 <- HandL.stepAIC.MCMC(Y.Name = "NumBoc", common_name = "Bocaccio", Area = c('Orig121', 'CCA', 'ALL')[1], Include.FishTime = FALSE, Restrict.6min = FALSE, Sites = NULL, 
                             propHookCutOffAggr = ifelse(Area == "ALL", 0.002, 0.003), propHookCutOffMirage = propHookCutOffAggr, propHookCutOffToro = 3 * propHookCutOffAggr,
                             VermComplex = FALSE, Interaction = TRUE, reducedFormula = TRUE, buffer = c(15, 45), contrast = 'treatment', tune = 0.14, mcmc = 150000, burnin = 1000, 
                             thin = 150, verbose = 1000, Stop.before.MCMC = FALSE, MAIN.STEP.AIC = NULL, STEP.AIC = NULL, GLM.FINAL.AIC = NULL, 
                             grandPathCSV = "W:/ALL_USR/JRW/Hook & Line Survey/2020/DATA/qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
      
 
