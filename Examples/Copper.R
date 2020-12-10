
##############################################################################################
######## See the 'Squarespot.R example for more comments about the similar code there. ########  
##############################################################################################

remotes::install_github("John-R-Wallace-NOAA/HookandLineSurveyCatchAnalysis") # One time only, per R version, per machine

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
    
   
   
# ---- Copper, Area = 121 (standard buffer = c(15, 45)); default value of 0.009 for propHookCutOffToro  ----
dir.create('Copp.2019.NFT.1k.121', showWarnings = FALSE)
setwd('Copp.2019.NFT.1k.121'); getwd()
# Running 1k to find the correct proportion of total hooks cut off for each of the 3 vessels. 
Copp.2019.NFT.1k.121 <- HandL.stepAIC.MCMC(Y.Name = "NumCopp", common_name = "Copper Rockfish", Area = c('Orig121', 'CCA', 'ALL')[1], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, buffer = c(15, 45), tune = 0.14, mcmc = 1000, burnin = 10, thin = 2, verbose = 100, 
                             grandPathCSV = "W:/ALL_USR/JRW/Hook & Line Survey/2020/DATA/qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")

Copp.2019.NFT.1k.121$Final.Model

  
dir.create('Copp.2019.NFT.1m.121', showWarnings = FALSE)
setwd('Copp.2019.NFT.1m.121'); getwd()                        
Copp.2019.NFT.1m.121 <- HandL.stepAIC.MCMC(Y.Name = "NumCopp", common_name = "Copper Rockfish", Area = c('Orig121', 'CCA', 'ALL')[1], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, buffer = c(15, 45), tune = 0.14, mcmc = 1e6, burnin = 1000, thin = 1000, verbose = 1000, 
                             grandPathCSV = "../qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")

                             
# Check the MCMC autocorrelation                             
dev.new()
plot(MCMC, ask = TRUE)
dev.new()
autocorr.plot(MCMC, ask = TRUE)  

# Looks resonable, so save some autocorr plots
dir.create("Auto Corr Plots 1mil MCMC", showWarnings = FALSE)
setwd("Auto Corr Plots 1mil MCMC"); getwd()
png(filename = "Copp.1m.MCMC.autocorr%03d.png", width = 960, height = 960)
autocorr.plot(MCMC, ask = FALSE)  
dev.off()
setwd('..'); getwd()
 
Copp.2019.NFT.1m.121$Final.Model

NumCopp ~ year2005 + year2006 + year2007 + year2008 + year2009 + 
    year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + 
    year2016 + year2017 + year2018 + year2019 + site_number114 + 
    site_number119 + site_number130 + site_number146 + site_number148 + 
    site_number149 + site_number151 + site_number152 + site_number180 + 
    site_number185 + site_number186 + site_number187 + site_number189 + 
    site_number193 + site_number197 + site_number200 + site_number226 + 
    site_number228 + site_number233 + site_number234 + site_number252 + 
    site_number277 + site_number287 + site_number289 + site_number291 + 
    site_number292 + site_number293 + site_number298 + site_number299 + 
    site_number315 + site_number317 + site_number365 + site_number374 + 
    site_number375 + site_number379 + site_number383 + site_number389 + 
    site_number390 + site_number395 + site_number396 + site_number397 + 
    site_number398 + site_number399 + site_number402 + site_number407 + 
    site_number45 + site_number54 + site_number59 + site_number62 + 
    site_number66 + site_number68 + site_number71 + site_number84 + 
    site_number92 + site_number97 + CrewStaffAdrianChavez + CrewStaffAlexEdwards + 
    CrewStaffAndrewPerez + CrewStaffApolloKollias + CrewStaffBlairWilliams + 
    CrewStaffBrianDunham + CrewStaffBrianKelly + CrewStaffBrianOldmen + 
    CrewStaffBrittneyCsorba + CrewStaffCaseyVaccarezza + CrewStaffChaseHolt + 
    CrewStaffChrisArnold + CrewStaffDanFellows + CrewStaffDavidMurdoch + 
    CrewStaffDustinDustin2006AG + CrewStaffIanNicholson + CrewStaffIchiroShiatsubo + 
    CrewStaffJacobSchwank + CrewStaffJakeTollison + CrewStaffJeffMarkland + 
    CrewStaffJeffNester + CrewStaffJoelBuonassissi + CrewStaffJoeVillareal + 
    CrewStaffJohnBlackman + CrewStaffJohnnyChastine + CrewStaffJoshTanner + 
    CrewStaffKevinMartin + CrewStaffLarryUgale + CrewStaffMattMatt2005AG + 
    CrewStaffMattThomas + CrewStaffMikeMayberry + CrewStaffMinorAnglerAggressor + 
    CrewStaffMinorAnglerMirage + CrewStaffNickEscobedo + CrewStaffNickNguyen + 
    CrewStaffPatrickPham + CrewStaffPaulHansen + CrewStaffPaulPennello + 
    CrewStaffRayBlakeslee + CrewStaffRonMueller + CrewStaffRyanConlin + 
    CrewStaffRyanGriffin + CrewStaffScottGautreau + CrewStaffSeanHagerty + 
    CrewStaffSeanHartigan + CrewStaffShaunVanderpol + CrewStaffShawnTerrell + 
    CrewStaffShonRoberts + CrewStaffSteveChesley + CrewStaffTimLonghorn + 
    CrewStaffTimTim2006AG + CrewStaffTomVeloz + CrewStaffTroySteranko + 
    CrewStaffTuckerMcCombs + CrewStaffVictorRamirez + drop_number2 + 
    drop_number3 + drop_number4 + drop_number5 + hook_number2 + 
    hook_number3 + hook_number4 + hook_number5_Top + swell_height_m
                             
                              
dev.new()   
par(mfrow = c(3, 2))
plot.Gam(glm(NumCopp ~ year + site_number + CrewStaff + drop_number + hook_number + swell_height_m, data = DATA), se = TRUE, rugplot = TRUE, scale = 0)

dev.new()
par(mfrow = c(3, 2))
plot.Gam(glm(NumCopp ~ year + site_number + CrewStaff + drop_number + hook_number + swell_height_m, data = DATA), se = TRUE, rugplot = TRUE, scale = 0.13)


                             

# ---- Copper, Area = All; propHookCutOffToro = 0.015 ----

dir.create('Copp.2019.NFT.1k.ALL', showWarnings = FALSE)
setwd('Copp.2019.NFT.1k.ALL'); getwd()
Copp.2019.NFT.1k.ALL <- HandL.stepAIC.MCMC(Y.Name = "NumCopp", common_name = "Copper Rockfish", Area = c('Orig121', 'CCA', 'ALL')[3], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, propHookCutOffToro = 0.015, buffer = c(15, 45), tune = 0.14, mcmc = 1000, burnin = 10, thin = 2, verbose = 100, 
                             grandPathCSV = "W:/ALL_USR/JRW/Hook & Line Survey/2020/DATA/qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")

Copp.2019.NFT.1k.ALL$Final.Model

dir.create('Copp.2019.NFT.1m.ALL', showWarnings = FALSE)
setwd('Copp.2019.NFT.1m.ALL'); getwd()
                             
Copp.2019.NFT.1m.ALL <- HandL.stepAIC.MCMC(Y.Name = "NumCopp", common_name = "Copper Rockfish", Area = c('Orig121', 'CCA', 'ALL')[3], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, propHookCutOffToro = 0.015, buffer = c(15, 45), tune = 0.14, mcmc = 1e6, burnin = 1000, thin = 1000, verbose = 1000, 
                             grandPathCSV = "../qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
 
 
# Check the MCMC autocorrelation                        
dev.new()
plot(MCMC, ask = TRUE)
dev.new()
autocorr.plot(MCMC, ask = TRUE)  

# There is some autocorrelation so more MCMC iterations could be done, but at this level the change to the result is very small
dir.create("Auto Corr Plots 1mil MCMC", showWarnings = FALSE)
setwd("Auto Corr Plots 1mil MCMC"); getwd()
png(filename = "Copp.1m.MCMC.autocorr%03d.png", width = 960, height = 960)
autocorr.plot(MCMC, ask = FALSE)  
dev.off()
setwd('..'); getwd()
                                
   
dev.new()   
par(mfrow = c(3, 2))
plot.Gam(glm(NumCopp ~ year + site_number + CrewStaff + drop_number + hook_number + swell_height_m, data = DATA), se = TRUE, rugplot = TRUE, scale = 0)

dev.new()
par(mfrow = c(3, 2))
plot.Gam(glm(NumCopp ~ year + site_number + CrewStaff + drop_number + hook_number + swell_height_m, data = DATA), se = TRUE, rugplot = TRUE, scale = 0.13)

Copp.2019.NFT.1m.ALL$Final.Model
NumCopp ~ year2005 + year2006 + year2007 + year2008 + year2009 + 
    year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + 
    year2016 + year2017 + year2018 + year2019 + site_number114 + 
    site_number119 + site_number130 + site_number146 + site_number148 + 
    site_number149 + site_number151 + site_number152 + site_number180 + 
    site_number185 + site_number186 + site_number187 + site_number189 + 
    site_number193 + site_number197 + site_number200 + site_number226 + 
    site_number228 + site_number233 + site_number234 + site_number252 + 
    site_number277 + site_number287 + site_number289 + site_number291 + 
    site_number292 + site_number293 + site_number298 + site_number299 + 
    site_number315 + site_number317 + site_number365 + site_number374 + 
    site_number375 + site_number379 + site_number383 + site_number389 + 
    site_number390 + site_number395 + site_number396 + site_number397 + 
    site_number398 + site_number399 + site_number402 + site_number407 + 
    site_number45 + site_number514 + site_number516 + site_number517 + 
    site_number520 + site_number533 + site_number537 + site_number538 + 
    site_number539 + site_number54 + site_number540 + site_number576 + 
    site_number59 + site_number593 + site_number594 + site_number600 + 
    site_number62 + site_number66 + site_number68 + site_number71 + 
    site_number84 + site_number92 + site_number97 + CrewStaffAdrianChavez + 
    CrewStaffAlexBrickell + CrewStaffAlexEdwards + CrewStaffAndrewPerez + 
    CrewStaffApolloKollias + CrewStaffBlairWilliams + CrewStaffBrianCedillo + 
    CrewStaffBrianDunham + CrewStaffBrianKelly + CrewStaffBrianOldmen + 
    CrewStaffBrittneyCsorba + CrewStaffCaseyVaccarezza + CrewStaffChaseHolt + 
    CrewStaffChrisArnold + CrewStaffDanFellows + CrewStaffDavidMurdoch + 
    CrewStaffDustinDustin2006AG + CrewStaffIanNicholson + CrewStaffIchiroShiatsubo + 
    CrewStaffJacobSchwank + CrewStaffJakeTollison + CrewStaffJeffMarkland + 
    CrewStaffJeffNester + CrewStaffJoelBuonassissi + CrewStaffJoeVillareal + 
    CrewStaffJohnBlackman + CrewStaffJohnnyChastine + CrewStaffJoshTanner + 
    CrewStaffKevinMartin + CrewStaffLarryUgale + CrewStaffMattMatt2005AG + 
    CrewStaffMattThomas + CrewStaffMikeMayberry + CrewStaffMinorAnglerAggressor + 
    CrewStaffMinorAnglerMirage + CrewStaffMinorAnglerToronado + 
    CrewStaffNickEscobedo + CrewStaffNickNguyen + CrewStaffOscarRodriguez + 
    CrewStaffPatrickPham + CrewStaffPaulHansen + CrewStaffPaulPennello + 
    CrewStaffRayBlakeslee + CrewStaffRonMueller + CrewStaffRyanConlin + 
    CrewStaffRyanGriffin + CrewStaffScottGautreau + CrewStaffSeanHagerty + 
    CrewStaffShaunVanderpol + CrewStaffShawnTerrell + CrewStaffShonRoberts + 
    CrewStaffSteveChesley + CrewStaffTimLonghorn + CrewStaffTimTim2006AG + 
    CrewStaffTomVeloz + CrewStaffTroySteranko + CrewStaffTuckerMcCombs + 
    CrewStaffVictorRamirez + drop_number2 + drop_number3 + drop_number4 + 
    drop_number5 + hook_number2 + hook_number3 + hook_number4 + 
    hook_number5_Top + swell_height_m
 
 
dev.new()   
par(mfrow = c(3, 2))
plot.Gam(glm(NumCopp ~ year + site_number + CrewStaff + drop_number + hook_number + swell_height_m, data = DATA), se = TRUE, rugplot = TRUE, scale = 0)

dev.new()
par(mfrow = c(3, 2))
plot.Gam(glm(NumCopp ~ year + site_number + CrewStaff + drop_number + hook_number + swell_height_m, data = DATA), se = TRUE, rugplot = TRUE, scale = 0.14)

  
                             
  

# ---- Copper, Area = CCA; propHookCutOffAggr = 0.040, propHookCutOffMirage = 0.010, propHookCutOffToro = 0.010 ----
dir.create('Copp.2019.NFT.1k.CCA', showWarnings = FALSE)
setwd('Copp.2019.NFT.1k.CCA'); getwd()
Copp.2019.NFT.1k.CCA <- HandL.stepAIC.MCMC(Y.Name = "NumCopp", common_name = "Copper Rockfish", Area = c('Orig121', 'CCA', 'ALL')[2], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, propHookCutOffAggr = 0.040, propHookCutOffMirage = 0.010, propHookCutOffToro = 0.010, buffer = c(15, 45), 
                             tune = 0.14, mcmc = 1000, burnin = 10, thin = 2, verbose = 100, 
                             grandPathCSV = "W:/ALL_USR/JRW/Hook & Line Survey/2020/DATA/qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")

Copp.2019.NFT.1k.CCA$Final.Model                            
 
# MCMC tune needed to be changed to 0.30
dir.create('Copp.2019.NFT.1m.CCA', showWarnings = FALSE)
setwd('Copp.2019.NFT.1m.CCA'); getwd()
Copp.2019.NFT.1m.CCA <- HandL.stepAIC.MCMC(Y.Name = "NumCopp", common_name = "Copper Rockfish", Area = c('Orig121', 'CCA', 'ALL')[2], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, propHookCutOffAggr = 0.040, propHookCutOffMirage = 0.010, propHookCutOffToro = 0.010, buffer = c(15, 45), 
                             tune = 0.30, mcmc = 1e6, burnin = 1000, thin = 1000, verbose = 1000,
                             grandPathCSV = "../qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
                             
                             

# Check the MCMC autocorrelation                          
dev.new()
plot(MCMC, ask = TRUE)
dev.new()
autocorr.plot(MCMC, ask = TRUE)  

# Looks resonable, so save some autocorr plots
dir.create("Auto Corr Plots 1mil MCMC", showWarnings = FALSE)
setwd("Auto Corr Plots 1mil MCMC"); getwd()
png(filename = "Copp.1m.MCMC.autocorr%03d.png", width = 960, height = 960)
autocorr.plot(MCMC, ask = FALSE)  
dev.off()
setwd('..'); getwd()

                             
               
Copp.2019.NFT.1m.CCA$Final.Model
NumCopp ~ year2015 + year2016 + year2017 + year2018 + year2019 + 
    site_number516 + site_number517 + site_number520 + site_number533 + 
    site_number537 + site_number538 + site_number539 + site_number540 + 
    site_number576 + site_number593 + site_number594 + site_number600 + 
    CrewStaffBrianKelly + CrewStaffCaseyVaccarezza + CrewStaffChaseHolt + 
    CrewStaffCoreyNakano + CrewStaffDavidMurdoch + CrewStaffJohnnyChastine + 
    CrewStaffKevinMartin + CrewStaffMattThomas + CrewStaffMinorAnglerAggressor + 
    CrewStaffPatrickPham + CrewStaffPaulHansen + CrewStaffPaulPennello + 
    CrewStaffRyanConlin + CrewStaffShaunVanderpol + CrewStaffShawnTerrell + 
    CrewStaffTomVeloz + CrewStaffTroySteranko + CrewStaffVictorRamirez + 
    drop_number2 + drop_number3 + drop_number4 + drop_number5 + 
    hook_number2 + hook_number3 + hook_number4 + hook_number5_Top
  
    
dev.new()   
par(mfrow = c(3, 2))
plot.Gam(glm(NumCopp ~ year + site_number + CrewStaff + drop_number + hook_number, data = DATA), se = TRUE, rugplot = TRUE, scale = 0)

dev.new()
par(mfrow = c(3, 2))
plot.Gam(glm(NumCopp ~ year + site_number + CrewStaff + drop_number + hook_number, data = DATA), se = TRUE, rugplot = TRUE, scale = 0.07)

