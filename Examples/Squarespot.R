

# ---------------------------------------------------------------------------------------------
# ------ Squarespot, Area = 121 (standard buffer = c(15, 45)); propHookCutOffToro = 0.015 -----
# ---------------------------------------------------------------------------------------------


# -- For Tantalus --
# /opt/R/64-bit/R-4.0.1_MKL/bin/R
# options(width = 160)
  
# For R-MKL multithreading (see https://github.com/John-R-Wallace-NOAA/R_4.X_MRO_Windows_and_R_MKL_Linux)
# RhpcBLASctl::blas_set_num_threads(6)
# RhpcBLASctl::blas_get_num_procs() 

remotes::install_github("John-R-Wallace-NOAA/HookandLineSurveyCatchAnalysis") # One time only, per R version, per machine


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
 
 
dir.create('SqSpot.2019.NFT.50k.121', showWarnings = FALSE)
setwd('SqSpot.2019.NFT.50k.121'); getwd()

SqSpot.2019.NFT.50k.121 <- HandL.stepAIC.MCMC(Y.Name = "NumSqSpot", common_name = "Squarespot Rockfish", Area = c('Orig121', 'CCA', 'ALL')[1], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, propHookCutOffToro = 0.015, buffer = c(15, 45), tune = 0.14, mcmc = 50000, burnin = 500, thin = 50, verbose = 500, 
                             grandPathCSV = "../qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")


# Check the MCMC autocorrelation. If the MCMC diagnostic plots are gone, or you want to step through them with 'ask = TRUE', run the following four lines:                              
dev.new()
plot(MCMC, ask = TRUE)
dev.new()
autocorr.plot(MCMC, ask = TRUE)


# Autocorrelation in the MCMC is seen in the 50k run, so do a million iterations
setwd('..'); getwd()
dir.create('SqSpot.2019.NFT.1m.CCA', showWarnings = FALSE)
setwd('SqSpot.2019.NFT.1m.CCA'); getwd()
                         
SqSpot.2019.NFT.1m.121 <- HandL.stepAIC.MCMC(Y.Name = "NumSqSpot", common_name = "Squarespot Rockfish", Area = c('Orig121', 'CCA', 'ALL')[1], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, propHookCutOffToro = 0.015, buffer = c(15, 45), tune = 0.14, mcmc = 1e6, burnin = 1000, thin = 1000, verbose = 1000, 
                             grandPathCSV = "../qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")

# Check the MCMC autocorrelation again                             
dev.new()
plot(MCMC, ask = TRUE)
dev.new()
autocorr.plot(MCMC, ask = TRUE)

# Site 367 and a few others to a lesser degree has some autocorrelation so more MCMC iterations could be done, but at this level the change to the result is very small
# Save some autocorr plots
png(filename = "SqSpot.121.1m.MCMC.autocorr%03d.png", width = 960, height = 960)
autocorr.plot(MCMC, ask = FALSE)  
dev.off()


# Look at the names in the runs output list
names(SqSpot.2019.NFT.1m.121)
# # [1] "Final.Model"    "AIC.DIC"        "Final.glm.coef" "Q.MCMC"         "SS.Table"       "MCMC"  


# Look at the long style formula of the design matrix needed for MCMClogit() and create the shorthand form for glm() 
SqSpot.2019.NFT.1m.121$Final.Model

# # NumSqSpot ~ year2005 + year2006 + year2007 + year2008 + year2009 + 
# #     year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + 
# #     year2016 + year2017 + year2018 + year2019 + site_number109 + 
# #     site_number11 + site_number114 + site_number119 + site_number130 + 
# #     site_number133 + site_number15 + site_number151 + site_number152 + 
# #     site_number157 + site_number16 + site_number162 + site_number167 + 
# #     site_number168 + site_number180 + site_number187 + site_number189 + 
# #     site_number193 + site_number197 + site_number2 + site_number217 + 
# #     site_number226 + site_number229 + site_number233 + site_number234 + 
# #     site_number249 + site_number252 + site_number277 + site_number291 + 
# #     site_number292 + site_number293 + site_number298 + site_number304 + 
# #     site_number315 + site_number317 + site_number318 + site_number326 + 
# #     site_number33 + site_number365 + site_number367 + site_number374 + 
# #     site_number379 + site_number380 + site_number383 + site_number389 + 
# #     site_number390 + site_number395 + site_number396 + site_number397 + 
# #     site_number399 + site_number40 + site_number402 + site_number407 + 
# #     site_number413 + site_number43 + site_number45 + site_number5 + 
# #     site_number52 + site_number54 + site_number59 + site_number62 + 
# #     site_number66 + site_number68 + site_number71 + site_number79 + 
# #     site_number84 + site_number92 + site_number97 + CrewStaffAlexEdwards + 
# #     CrewStaffAndrewPerez + CrewStaffApolloKollias + CrewStaffBlairWilliams + 
# #     CrewStaffBrianCedillo + CrewStaffBrianDunham + CrewStaffBrianKelly + 
# #     CrewStaffBrianOldmen + CrewStaffBrittneyCsorba + CrewStaffBryanHoltan + 
# #     CrewStaffCaseyVaccarezza + CrewStaffChaseHolt + CrewStaffChrisArnold + 
# #     CrewStaffDanFellows + CrewStaffDavidMurdoch + CrewStaffDustinDustin2006AG + 
# #     CrewStaffIanNicholson + CrewStaffIchiroShiatsubo + CrewStaffJacobSchwank + 
# #     CrewStaffJakeTollison + CrewStaffJeffMarkland + CrewStaffJeffNester + 
# #     CrewStaffJoelBuonassissi + CrewStaffJoeVillareal + CrewStaffJohnBlackman + 
# #     CrewStaffJohnnyChastine + CrewStaffJoshTanner + CrewStaffKevinMartin + 
# #     CrewStaffLarryUgale + CrewStaffMattMatt2005AG + CrewStaffMattThomas + 
# #     CrewStaffMikeMayberry + CrewStaffMinorAnglerAggressor + CrewStaffMinorAnglerMirage + 
# #     CrewStaffMinorAnglerToronado + CrewStaffNickEscobedo + CrewStaffOscarRodriguez + 
# #     CrewStaffPatrickPham + CrewStaffPaulHansen + CrewStaffPhillipEbert + 
# #     CrewStaffRyanConlin + CrewStaffRyanGriffin + CrewStaffSeanHagerty + 
# #     CrewStaffShaunVanderpol + CrewStaffShawnTerrell + CrewStaffShonRoberts + 
# #     CrewStaffSteveChesley + CrewStaffTimLonghorn + CrewStaffTimTim2006AG + 
# #     CrewStaffTomVeloz + CrewStaffTroySteranko + CrewStaffTuckerMcCombs + 
# #     CrewStaffVictorRamirez + drop_number2 + drop_number3 + drop_number4 + 
# #     drop_number5 + hook_number2 + hook_number3 + hook_number4 + 
# #     hook_number5_Top + swell_height_m.P1 + swell_height_m.P2


# Use the shorthand formula in plot.Gam() and then select a proper value for 'scale'. From plot.Gam()'s help:
#     "a lower limit for the number of units covered by the limits on the ‘y’ for each plot. The default is scale=0, in which case each plot uses the range of the functions 
#     being plotted to create their ylim. By setting scale to be the maximum value of diff(ylim) for all the plots, then all subsequent plots will produced in the same 
#     vertical units. This is essential for comparing the importance of fitted terms in additive models."
#     The 'P1', 'P2' are the polynomial levels for the continuous variable.  The maximum 'P' level is the level to give in the poly() function in the shorthand formula below.
#     'DATA' is saved to .GlobalEnv by the run above.


GLM <- glm(NumSqSpot ~ year + site_number + CrewStaff + drop_number + hook_number + poly(swell_height_m, 2), data = SqSpot.Final.Model.MCMC.2019$DATA, family = 'binomial')

dev.new()
par(mfrow = c(3, 2))
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 0)


# Select the scale at 7.6, the max diff(ylim), and replot

dev.new()
par(mfrow = c(3, 2))
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 7.6)

# Save this figure to 'SqSplot.121.GAM.Fig.png' manaully. For this figure, saving to PNG with a manual save from the Windows R console gives a better result than using png().
 
# ANOVA-like table   
(ANOVA <- anova(GLM, test = 'Chisq'))

# AIC table (requires CRAN package: AICcmodavg)
(AICTAB <- JRWToolBox::icTableGlm(GLM))
 
capture.output(cat("\n\n"), ANOVA, cat("\n\n"), AICTAB, file = paste0(substring(as.character(SqSplot.Final.Model.MCMC.2019$Final.Model)[2], 4), ".Analysis.of.Deviance.and.AIC.Tables.txt"))
 

# ---------------------------------------------------------------------------------------------
# ------ Squarespot, Area = ALL; standard buffer = c(15, 45); propHookCutOffToro = 0.015 ------
# ---------------------------------------------------------------------------------------------


# -- For Tantalus --
# /opt/R/64-bit/R-4.0.1_MKL/bin/R
# options(width = 160)
  
# For R-MKL multithreading  (see https://github.com/John-R-Wallace-NOAA/R_4.X_MRO_Windows_and_R_MKL_Linux)
# RhpcBLASctl::blas_set_num_threads(6)
# RhpcBLASctl::blas_get_num_procs() 


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


dir.create('SqSpot.2019.NFT.50k.ALL', showWarnings = FALSE)
setwd('SqSpot.2019.NFT.50k.ALL'); getwd()

SqSpot.2019.NFT.50k.ALL <- HandL.stepAIC.MCMC(Y.Name = "NumSqSpot", common_name = "Squarespot Rockfish", Area = c('Orig121', 'CCA', 'ALL')[3], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, propHookCutOffToro = 0.015, buffer = c(15, 45), tune = 0.14, mcmc = 50000, burnin = 500, thin = 50, verbose = 500, 
                             grandPathCSV = "../qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")


# Check the MCMC autocorrelation. If the MCMC diagnostic plots are gone, or you want to step through them with 'ask = TRUE', run the following four lines:                              
dev.new()
plot(MCMC, ask = TRUE)
dev.new()
autocorr.plot(MCMC, ask = TRUE)

# Autocorrelation in the MCMC is seen in the 50k run, so do a million iterations
setwd('..'); getwd()
dir.create('SqSpot.2019.NFT.1m.ALL', showWarnings = FALSE)
setwd('SqSpot.2019.NFT.1m.ALL'); getwd()
                             
SqSpot.2019.NFT.1m.ALL <- HandL.stepAIC.MCMC(Y.Name = "NumSqSpot", common_name = "Squarespot Rockfish", Area = c('Orig121', 'CCA', 'ALL')[3], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, propHookCutOffToro = 0.015, buffer = c(15, 45), tune = 0.14, mcmc = 1e6, burnin = 1000, thin = 1000, verbose = 1000, 
                             grandPathCSV = "../qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
                           

# Check the MCMC autocorrelation again                             
dev.new()
plot(MCMC, ask = TRUE)
dev.new()
autocorr.plot(MCMC, ask = TRUE)  

# There is some autocorrelation so more MCMC iterations could be done, but at this level the change to the result is very small
# Save some autocorr plots
png(filename = "SqSpot.ALL.1m.MCMC.autocorr%03d.png", width = 960, height = 960)
autocorr.plot(MCMC, ask = FALSE)  
dev.off()


# Look at the long style formula of the design matrix needed for MCMClogit() and create the shorthand form for glm() 
SqSpot.2019.NFT.1m.ALL$Final.Model

# # NumSqSpot ~ year2005 + year2006 + year2007 + year2008 + year2009 + 
# #     year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + 
# #     year2016 + year2017 + year2018 + year2019 + site_number109 + 
# #     site_number11 + site_number114 + site_number119 + site_number130 + 
# #     site_number133 + site_number15 + site_number151 + site_number152 + 
# #     site_number157 + site_number16 + site_number162 + site_number167 + 
# #     site_number168 + site_number180 + site_number187 + site_number189 + 
# #     site_number193 + site_number197 + site_number2 + site_number217 + 
# #     site_number226 + site_number229 + site_number233 + site_number234 + 
# #     site_number249 + site_number252 + site_number277 + site_number291 + 
# #     site_number292 + site_number293 + site_number298 + site_number304 + 
# #     site_number315 + site_number317 + site_number318 + site_number326 + 
# #     site_number33 + site_number365 + site_number367 + site_number374 + 
# #     site_number379 + site_number380 + site_number383 + site_number389 + 
# #     site_number390 + site_number395 + site_number396 + site_number397 + 
# #     site_number399 + site_number40 + site_number402 + site_number407 + 
# #     site_number413 + site_number43 + site_number45 + site_number5 + 
# #     site_number501 + site_number503 + site_number507 + site_number508 + 
# #     site_number509 + site_number512 + site_number514 + site_number516 + 
# #     site_number517 + site_number519 + site_number52 + site_number520 + 
# #     site_number521 + site_number522 + site_number523 + site_number524 + 
# #     site_number528 + site_number533 + site_number537 + site_number538 + 
# #     site_number539 + site_number54 + site_number540 + site_number546 + 
# #     site_number547 + site_number548 + site_number550 + site_number556 + 
# #     site_number560 + site_number561 + site_number562 + site_number566 + 
# #     site_number570 + site_number571 + site_number573 + site_number574 + 
# #     site_number575 + site_number576 + site_number577 + site_number59 + 
# #     site_number590 + site_number591 + site_number592 + site_number593 + 
# #     site_number594 + site_number595 + site_number596 + site_number600 + 
# #     site_number606 + site_number611 + site_number613 + site_number62 + 
# #     site_number66 + site_number68 + site_number71 + site_number79 + 
# #     site_number84 + site_number92 + site_number97 + CrewStaffAlexBrickell + 
# #     CrewStaffAlexEdwards + CrewStaffAndrewPerez + CrewStaffApolloKollias + 
# #     CrewStaffBlairWilliams + CrewStaffBrianCedillo + CrewStaffBrianDunham + 
# #     CrewStaffBrianKelly + CrewStaffBrianOldmen + CrewStaffBrittneyCsorba + 
# #     CrewStaffBryanHoltan + CrewStaffCaseyVaccarezza + CrewStaffChaseHolt + 
# #     CrewStaffChrisArnold + CrewStaffCoreyNakano + CrewStaffDanFellows + 
# #     CrewStaffDavidMurdoch + CrewStaffDustinDustin2006AG + CrewStaffIanNicholson + 
# #     CrewStaffIchiroShiatsubo + CrewStaffJacobSchwank + CrewStaffJakeTollison + 
# #     CrewStaffJeffMarkland + CrewStaffJeffNester + CrewStaffJoelBuonassissi + 
# #     CrewStaffJoeVillareal + CrewStaffJohnBlackman + CrewStaffJohnnyChastine + 
# #     CrewStaffJoshTanner + CrewStaffKevinMartin + CrewStaffLarryUgale + 
# #     CrewStaffMattMatt2005AG + CrewStaffMattThomas + CrewStaffMikeMayberry + 
# #     CrewStaffMinorAnglerAggressor + CrewStaffMinorAnglerMirage + 
# #     CrewStaffMinorAnglerToronado + CrewStaffNickEscobedo + CrewStaffOscarRodriguez + 
# #     CrewStaffPatrickPham + CrewStaffPaulHansen + CrewStaffPaulPennello + 
# #     CrewStaffPhillipEbert + CrewStaffRyanConlin + CrewStaffRyanGriffin + 
# #     CrewStaffSeanHagerty + CrewStaffShaunVanderpol + CrewStaffShawnTerrell + 
# #     CrewStaffShonRoberts + CrewStaffSteveChesley + CrewStaffTimLonghorn + 
# #     CrewStaffTimTim2006AG + CrewStaffTomVeloz + CrewStaffTroySteranko + 
# #     CrewStaffTuckerMcCombs + CrewStaffVictorRamirez + drop_number2 + 
# #     drop_number3 + drop_number4 + drop_number5 + hook_number2 + 
# #     hook_number3 + hook_number4 + hook_number5_Top + moon_percent_fullness_r.P1 + 
# #     moon_percent_fullness_r.P2 + moon_percent_fullness_r.P3


# Use the shorthand formula in plot.Gam() and then select a proper value for 'scale'. From plot.Gam()'s help:
#     "a lower limit for the number of units covered by the limits on the ‘y’ for each plot. The default is scale=0, in which case each plot uses the range of the functions 
#     being plotted to create their ylim. By setting scale to be the maximum value of diff(ylim) for all the plots, then all subsequent plots will produced in the same 
#     vertical units. This is essential for comparing the importance of fitted terms in additive models."
#     The 'P1', 'P2' are the polynomial levels for the continuous variable.  The maximum 'P' level is the level to give in the poly() function in the shorthand formula below.
#     'DATA' is saved to .GlobalEnv by the run above.


 
GLM <- glm(NumSqSpot ~ year + site_number + CrewStaff + drop_number + hook_number + poly(moon_percent_fullness_r, 3), data = SqSpot.Final.Model.MCMC.2019$DATA, family = 'binomial')

dev.new()
par(mfrow = c(3, 2))
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 0)


# Select the scale at 7.6, the max diff(ylim), and replot
dev.new()
par(mfrow = c(3, 2))
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 7.6)

# Save this figure to 'SqSplot.ALL.GAM.Fig.png' manaully. For this figure, saving to PNG with a manual save from the Windows R console gives a better result than using png().
 
# ANOVA-like table   
(ANOVA <- anova(GLM, test = 'Chisq'))

# AIC table (requires CRAN package: AICcmodavg)
(AICTAB <- JRWToolBox::icTableGlm(GLM))
 
capture.output(cat("\n\n"), ANOVA, cat("\n\n"), AICTAB, file = paste0(substring(as.character(SqSpot.Final.Model.MCMC.2019$Final.Model)[2], 4), ".Analysis.of.Deviance.and.AIC.Tables.txt"))
 
 

# ---------------------------------------------------------------------------------------------                           
# ------ Squarespot, Area = CCA -----
# ---------------------------------------------------------------------------------------------


# Note here tune = 0.17. From MCMClogit()'s help for 'tune' arg: Make sure that the acceptance rate is satisfactory (typically between 0.20 and 0.5) before using the posterior sample for inference.
# The default for tune is 0.17, but for '121' and 'ALL' areas for Squarespot, tune = 0.14. 

# -- For Tantalus --
# /opt/R/64-bit/R-4.0.1_MKL/bin/R
# options(width = 160)
  
# For R-MKL multithreading (see https://github.com/John-R-Wallace-NOAA/R_4.X_MRO_Windows_and_R_MKL_Linux) 
# RhpcBLASctl::blas_set_num_threads(6)
# RhpcBLASctl::blas_get_num_procs() 

remotes::install_github("John-R-Wallace-NOAA/HookandLineSurveyCatchAnalysis") 

# Required packages listed here for help with installation (other packages may be installed by these packages).
require(lattice)
require(maptools)
require(gtools) 
require(MASS)  # Do I need mass or just the hacked stepAIC.buffer() ??
require(gam)
require(MCMCpack)
require(Hmisc)
require(chron)
require(RCurl)
require(HookandLineSurveyCatchAnalysis) 
   

dir.create('SqSpot.2019.NFT.50k.CCA', showWarnings = FALSE)
setwd('SqSpot.2019.NFT.50k.CCA'); getwd()

SqSpot.2019.NFT.50k.CCA <- HandL.stepAIC.MCMC(Y.Name = "NumSqSpot", common_name = "Squarespot Rockfish", Area = c('Orig121', 'CCA', 'ALL')[2], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, propHookCutOffAggr = 0.010, propHookCutOffMirage = 0.010, propHookCutOffToro = 0.010, buffer = c(15, 45), 
                             tune = 0.17, mcmc = 50000, burnin = 500, thin = 50, verbose = 500, 
                             grandPathCSV = "../qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")


# Check the MCMC autocorrelation. If the MCMC diagnostic plots are gone, or you want to step through them with 'ask = TRUE', run the following four lines:                              
dev.new()
plot(MCMC, ask = TRUE)
dev.new()
autocorr.plot(MCMC, ask = TRUE)

# Autocorrelation in the MCMC is seen in the 50k run, so do a million iterations
setwd('..'); getwd()
dir.create('SqSpot.2019.NFT.1m.CCA', showWarnings = FALSE)
setwd('SqSpot.2019.NFT.1m.CCA'); getwd()

SqSpot.2019.NFT.1m.CCA <- HandL.stepAIC.MCMC(Y.Name = "NumSqSpot", common_name = "Squarespot Rockfish", Area = c('Orig121', 'CCA', 'ALL')[2], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, propHookCutOffAggr = 0.010, propHookCutOffMirage = 0.010, propHookCutOffToro = 0.010, buffer = c(15, 45), 
                             tune = 0.17, mcmc = 1e6, burnin = 1000, thin = 1000, verbose = 1000,
                             grandPathCSV = "../qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
 

# Check the MCMC autocorrelation again                             
dev.new()
plot(MCMC, ask = TRUE)
dev.new()
autocorr.plot(MCMC, ask = TRUE)  

# Looks resonable, so save some autocorr plots
png(filename = "SqSpot.1m.MCMC.autocorr%03d.png", width = 960, height = 960)
autocorr.plot(MCMC, ask = FALSE)  
dev.off()

# Look at the names in the runs output list
names(SqSpot.2019.NFT.1m.CCA)
# # [1] "Final.Model"    "AIC.DIC"        "Final.glm.coef" "Q.MCMC"         "SS.Table"       "MCMC"  


# Look at the long style formula of the design matrix needed for MCMClogit() and create the shorthand form for glm() 
SqSpot.2019.NFT.1m.CCA$Final.Model

# # NumSqSpot ~ year2015 + year2016 + year2017 + year2018 + year2019 + 
# #     site_number503 + site_number507 + site_number508 + site_number509 + 
# #     site_number512 + site_number514 + site_number516 + site_number517 + 
# #     site_number519 + site_number520 + site_number521 + site_number522 + 
# #     site_number523 + site_number524 + site_number528 + site_number533 + 
# #     site_number537 + site_number538 + site_number539 + site_number540 + 
# #     site_number546 + site_number547 + site_number548 + site_number550 + 
# #     site_number556 + site_number560 + site_number561 + site_number562 + 
# #     site_number566 + site_number570 + site_number571 + site_number573 + 
# #     site_number574 + site_number575 + site_number576 + site_number577 + 
# #     site_number590 + site_number591 + site_number592 + site_number593 + 
# #     site_number594 + site_number595 + site_number596 + site_number600 + 
# #     site_number606 + site_number611 + site_number613 + CrewStaffAlexBrickell + 
# #     CrewStaffAlexEdwards + CrewStaffAndrewPerez + CrewStaffBlairWilliams + 
# #     CrewStaffBrianCedillo + CrewStaffBrianKelly + CrewStaffBrittneyCsorba + 
# #     CrewStaffCaseyVaccarezza + CrewStaffChaseHolt + CrewStaffChrisArnold + 
# #     CrewStaffCoreyNakano + CrewStaffDanFellows + CrewStaffJacobSchwank + 
# #     CrewStaffJoeVillareal + CrewStaffJohnnyChastine + CrewStaffKevinMartin + 
# #     CrewStaffLarryUgale + CrewStaffMattThomas + CrewStaffMinorAnglerAggressor + 
# #     CrewStaffMinorAnglerMirage + CrewStaffMinorAnglerToronado + 
# #     CrewStaffOscarRodriguez + CrewStaffPatrickPham + CrewStaffPaulHansen + 
# #     CrewStaffPaulPennello + CrewStaffRyanConlin + CrewStaffSeanHagerty + 
# #     CrewStaffShaunVanderpol + CrewStaffShawnTerrell + CrewStaffSteveChesley + 
# #     CrewStaffTomVeloz + CrewStaffTroySteranko + CrewStaffTuckerMcCombs + 
# #     CrewStaffVictorRamirez + drop_number2 + drop_number3 + drop_number4 + 
# #     drop_number5 + hook_number2 + hook_number3 + hook_number4 + 
# #     hook_number5_Top + moon_percent_fullness_r.P1 + moon_percent_fullness_r.P2 + 
# #     moon_percent_fullness_r.P3
# # 

# Use the shorthand formula in plot.Gam() and then select a proper value for 'scale'. From plot.Gam()'s help:
#     "a lower limit for the number of units covered by the limits on the ‘y’ for each plot. The default is scale=0, in which case each plot uses the range of the functions 
#     being plotted to create their ylim. By setting scale to be the maximum value of diff(ylim) for all the plots, then all subsequent plots will produced in the same 
#     vertical units. This is essential for comparing the importance of fitted terms in additive models."
#     ('DATA' is saved to .GlobalEnv by the run above.)


GLM <- glm(NumSqSpot ~ year + site_number + CrewStaff + drop_number + hook_number + poly(moon_percent_fullness_r, 3), data = SqSpot.Final.Model.MCMC.2019$DATA, family = 'binomial')

par(mfrow = c(3, 2))
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 0)

# Select the scale at 7.6, the max diff(ylim), and replot
par(mfrow = c(3, 2))
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 7.6)

# Save this figure to 'SqSplot.CCA.GAM.Fig.png' manaully. For this figure, saving to PNG with a manual save from the Windows R console gives a better result than using png().


# ANOVA-like table   
(ANOVA <- anova(GLM, test = 'Chisq'))

# AIC table (requires CRAN package: AICcmodavg)
(AICTAB <- JRWToolBox::icTableGlm(GLM))
 
capture.output(cat("\n\n"), ANOVA, cat("\n\n"), AICTAB, file = paste0(substring(as.character(SqSpot.Final.Model.MCMC.2019$Final.Model)[2], 4), ".Analysis.of.Deviance.and.AIC.Tables.txt"))
 
 
 
# Looking at the design plot (the one with "Mean of NumSqSpot" vs factors in the model) we see that MinorAnglerAggressor does very well.
dev.new()
plot.design.jrw(DATA[, c("year", "site_number", "vessel", "drop_number", "hook_number", "angler_number", "moon_phase_r", "CrewStaff")], DATA[, 'NumSqSpot'], ylab = paste("Mean of NumSqSpot"))
 
 
 
 
