
#####################################################################################################
######## See the 'Squarespot.R example for more embedded comments in the similar code there. ########  
#####################################################################################################

remotes::install_github("John-R-Wallace-NOAA/HookandLineSurveyCatchAnalysis") # One time only, per R version, per machine or after an update

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
    
   
# -------------------------------------------------------------------------------------------------------------   
# ---- Vermilion, Area = 121 (standard buffer = c(15, 45)); default value of 0.009 for propHookCutOffToro  ----
# ------------------------------------------------------------------------------------------------------------- 

dir.create('Verm.2019.NFT.1k.121', showWarnings = FALSE)
setwd('Verm.2019.NFT.1k.121'); getwd()
# Running 1k to find the correct proportion of total hooks cut off for each of the 3 vessels. 
Verm.2019.NFT.1k.121 <- HandL.stepAIC.MCMC(Y.Name = "NumVerm", common_name = "Vermilion Rockfish", Area = c('Orig121', 'CCA', 'ALL')[1], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, buffer = c(15, 45), tune = 0.14, mcmc = 1000, burnin = 10, thin = 2, verbose = 100, 
                             grandPathCSV = "W:/ALL_USR/JRW/Hook & Line Survey/2020/DATA/qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")


# Run the 1 mil MCMC run using Verm.STEP.AIC from above
dir.create('Verm.2019.NFT.1m.121', showWarnings = FALSE)
setwd('Verm.2019.NFT.1m.121'); getwd() 
# If run on Tantalus
   # /opt/R/64-bit/R-4.0.1_MKL/bin/R
   # options(width = 160) 
   # load('Verm.121.Image.RData') # Renamed from '.RData'
Verm.2019.NFT.1m.121 <- HandL.stepAIC.MCMC(Y.Name = "NumVerm", common_name = "Vermilion Rockfish", Area = c('Orig121', 'CCA', 'ALL')[1], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, buffer = c(15, 45), tune = 0.14, mcmc = 1e6, burnin = 1000, thin = 1000, verbose = 1000, STEP.AIC = Verm.STEP.AIC) 

                             
# Check the MCMC autocorrelation                             
dev.new()
plot(MCMC, ask = TRUE)

dev.new()
autocorr.plot(MCMC, ask = TRUE)  

# Looks resonable, so save some autocorr plots
dir.create("Auto Corr Plots 1mil MCMC", showWarnings = FALSE)
setwd("Auto Corr Plots 1mil MCMC"); getwd()
png(filename = "Verm.1m.MCMC.autocorr%03d.png", width = 960, height = 960)
autocorr.plot(MCMC, ask = FALSE)  
dev.off()
setwd('..'); getwd()
 
 
Verm.Final.Model.MCMC.2019$Final.Model 

NumVerm ~ year2005 + year2006 + year2007 + year2008 + year2009 + 
    year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + 
    year2016 + year2017 + year2018 + year2019 + site_number109 + 
    site_number11 + site_number114 + site_number119 + site_number130 + 
    site_number133 + site_number136 + site_number137 + site_number139 + 
    site_number140 + site_number145 + site_number146 + site_number147 + 
    site_number148 + site_number149 + site_number15 + site_number151 + 
    site_number152 + site_number154 + site_number157 + site_number16 + 
    site_number162 + site_number167 + site_number168 + site_number17 + 
    site_number18 + site_number180 + site_number181 + site_number182 + 
    site_number184 + site_number185 + site_number186 + site_number187 + 
    site_number189 + site_number193 + site_number197 + site_number2 + 
    site_number200 + site_number205 + site_number209 + site_number21 + 
    site_number215 + site_number217 + site_number22 + site_number226 + 
    site_number228 + site_number229 + site_number231 + site_number232 + 
    site_number233 + site_number234 + site_number24 + site_number243 + 
    site_number249 + site_number252 + site_number27 + site_number277 + 
    site_number287 + site_number289 + site_number29 + site_number291 + 
    site_number292 + site_number293 + site_number298 + site_number299 + 
    site_number304 + site_number31 + site_number315 + site_number317 + 
    site_number318 + site_number326 + site_number33 + site_number333 + 
    site_number342 + site_number346 + site_number35 + site_number350 + 
    site_number352 + site_number36 + site_number365 + site_number367 + 
    site_number374 + site_number375 + site_number377 + site_number379 + 
    site_number380 + site_number383 + site_number385 + site_number389 + 
    site_number390 + site_number391 + site_number395 + site_number396 + 
    site_number397 + site_number398 + site_number399 + site_number402 + 
    site_number407 + site_number413 + site_number414 + site_number419 + 
    site_number43 + site_number45 + site_number48 + site_number52 + 
    site_number54 + site_number59 + site_number6 + site_number62 + 
    site_number66 + site_number68 + site_number71 + site_number77 + 
    site_number79 + site_number84 + site_number92 + site_number97 + 
    CrewStaffAdrianChavez + CrewStaffAlexBrickell + CrewStaffAlexEdwards + 
    CrewStaffAndrewPerez + CrewStaffApolloKollias + CrewStaffBlairWilliams + 
    CrewStaffBrianCedillo + CrewStaffBrianDunham + CrewStaffBrianFrankenfield + 
    CrewStaffBrianKelly + CrewStaffBrianOldmen + CrewStaffBrittneyCsorba + 
    CrewStaffBryanHoltan + CrewStaffCaseyVaccarezza + CrewStaffChaseHolt + 
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
    CrewStaffSeanHartigan + CrewStaffShaunVanderpol + CrewStaffShawnTerrell + 
    CrewStaffShonRoberts + CrewStaffSteveChesley + CrewStaffTimLonghorn + 
    CrewStaffTimTim2006AG + CrewStaffTomVeloz + CrewStaffTroySteranko + 
    CrewStaffTuckerMcCombs + CrewStaffVictorRamirez + drop_number2 + 
    drop_number3 + drop_number4 + drop_number5 + hook_number2 + 
    hook_number3 + hook_number4 + hook_number5_Top + moon_percent_fullness_r.P1 + 
    moon_percent_fullness_r.P2 + drop_time_proportion_of_solar_day.P1 + 
    drop_time_proportion_of_solar_day.P2 + drop_time_proportion_of_solar_day.P3 + 
    wave_height_m.P1 + wave_height_m.P2

                             
# Use the shorthand formula in plot.Gam() and then select a proper value for 'scale'. From plot.Gam()'s help:
#     "a lower limit for the number of units covered by the limits on the ‘y’ for each plot. The default is scale=0, in which case each plot uses the range of the functions 
#     being plotted to create their ylim. By setting scale to be the maximum value of diff(ylim) for all the plots, then all subsequent plots will produced in the same 
#     vertical units. This is essential for comparing the importance of fitted terms in additive models."
#     The 'P1', 'P2' are the polynomial levels for the continuous variable.  The maximum 'P' level is the level to give in the poly() function in the shorthand formula below.
#     'DATA' is saved to .GlobalEnv by the run above.

 
GLM <- glm(NumVerm ~ year + site_number + CrewStaff + drop_number + hook_number + poly(moon_percent_fullness_r, 2) + poly(drop_time_proportion_of_solar_day, 3) + 
                       poly(wave_height_m, 2), data = Verm.Final.Model.MCMC.2019$DATA, family = 'binomial') 
 
dev.new()   
par(mfrow = c(3, 3))
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 0)
                       
# Select the scale at 10.4, the max diff(ylim), and replot
dev.new()
par(mfrow = c(3, 3))
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 10.4)

# Save this figure to 'Verm.121.GAM.Fig.png' manaully. For this figure, saving to PNG with a manual save from the Windows R console gives a better result than using png().

# ANOVA-like table   
(ANOVA <- anova(GLM, test = 'Chisq'))

# AIC table (requires CRAN package: AICcmodavg)
(AICTAB <- JRWToolBox::icTableGlm(GLM))
 
capture.output(cat("\n\n"), ANOVA, cat("\n\n"), AICTAB, file = paste0(substring(as.character(Verm.Final.Model.MCMC.2019$Final.Model)[2], 4), ".Analysis.of.Deviance.and.AIC.Tables.txt"))
 

                             
# ------------------------------------------------------------------------------------------------------------- 
# ----- Vermilion, Area = All; propHookCutOffToro = 0.015 -----
# ------------------------------------------------------------------------------------------------------------- 

# Jump straight to the 1m MCMC 
dir.create('Verm.2019.NFT.1m.ALL', showWarnings = FALSE)
setwd('Verm.2019.NFT.1m.ALL'); getwd()
Verm.2019.NFT.1m.ALL <- HandL.stepAIC.MCMC(Y.Name = "NumVerm", common_name = "Vermilion Rockfish", Area = c('Orig121', 'CCA', 'ALL')[3], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, buffer = c(15, 45), tune = 0.14, mcmc = 1e6, burnin = 1000, thin = 1000, verbose = 1000, 
                             grandPathCSV = "../qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
 
 
# Check the MCMC autocorrelation                        
dev.new()
plot(MCMC, ask = TRUE)
dev.new()
autocorr.plot(MCMC, ask = TRUE)  

# There is some autocorrelation so more MCMC iterations could be done, but at this level the change to the result is very small
dir.create("Auto Corr Plots 1mil MCMC", showWarnings = FALSE)
setwd("Auto Corr Plots 1mil MCMC"); getwd()
png(filename = "Verm.1m.MCMC.autocorr%03d.png", width = 960, height = 960)
autocorr.plot(MCMC, ask = FALSE)  
dev.off()
setwd('..'); getwd()
 
 
Verm.Final.Model.MCMC.2019$Final.Model

NumVerm ~ year2005 + year2006 + year2007 + year2008 + year2009 + 
    year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + 
    year2016 + year2017 + year2018 + year2019 + site_number109 + 
    site_number11 + site_number114 + site_number119 + site_number130 + 
    site_number133 + site_number136 + site_number137 + site_number139 + 
    site_number140 + site_number145 + site_number146 + site_number147 + 
    site_number148 + site_number149 + site_number15 + site_number151 + 
    site_number152 + site_number154 + site_number157 + site_number16 + 
    site_number162 + site_number167 + site_number168 + site_number17 + 
    site_number18 + site_number180 + site_number181 + site_number182 + 
    site_number184 + site_number185 + site_number186 + site_number187 + 
    site_number189 + site_number193 + site_number197 + site_number2 + 
    site_number200 + site_number205 + site_number209 + site_number21 + 
    site_number215 + site_number217 + site_number22 + site_number226 + 
    site_number228 + site_number229 + site_number231 + site_number232 + 
    site_number233 + site_number234 + site_number24 + site_number243 + 
    site_number249 + site_number252 + site_number27 + site_number277 + 
    site_number287 + site_number289 + site_number29 + site_number291 + 
    site_number292 + site_number293 + site_number298 + site_number299 + 
    site_number304 + site_number31 + site_number315 + site_number317 + 
    site_number318 + site_number326 + site_number33 + site_number333 + 
    site_number342 + site_number346 + site_number35 + site_number350 + 
    site_number352 + site_number36 + site_number365 + site_number367 + 
    site_number374 + site_number375 + site_number377 + site_number379 + 
    site_number380 + site_number383 + site_number385 + site_number389 + 
    site_number390 + site_number391 + site_number395 + site_number396 + 
    site_number397 + site_number398 + site_number399 + site_number402 + 
    site_number407 + site_number413 + site_number414 + site_number419 + 
    site_number43 + site_number45 + site_number48 + site_number501 + 
    site_number506 + site_number507 + site_number508 + site_number509 + 
    site_number512 + site_number514 + site_number516 + site_number518 + 
    site_number519 + site_number52 + site_number520 + site_number521 + 
    site_number523 + site_number524 + site_number525 + site_number526 + 
    site_number527 + site_number528 + site_number531 + site_number533 + 
    site_number537 + site_number538 + site_number539 + site_number54 + 
    site_number540 + site_number543 + site_number546 + site_number547 + 
    site_number548 + site_number549 + site_number550 + site_number551 + 
    site_number553 + site_number556 + site_number557 + site_number558 + 
    site_number559 + site_number560 + site_number561 + site_number562 + 
    site_number563 + site_number564 + site_number566 + site_number568 + 
    site_number569 + site_number573 + site_number574 + site_number575 + 
    site_number577 + site_number581 + site_number582 + site_number583 + 
    site_number584 + site_number586 + site_number587 + site_number59 + 
    site_number590 + site_number591 + site_number592 + site_number593 + 
    site_number594 + site_number595 + site_number596 + site_number597 + 
    site_number598 + site_number6 + site_number601 + site_number606 + 
    site_number607 + site_number609 + site_number611 + site_number613 + 
    site_number62 + site_number66 + site_number68 + site_number71 + 
    site_number77 + site_number79 + site_number84 + site_number92 + 
    site_number97 + CrewStaffAdrianChavez + CrewStaffAlexBrickell + 
    CrewStaffAlexEdwards + CrewStaffAndrewPerez + CrewStaffApolloKollias + 
    CrewStaffBlairWilliams + CrewStaffBrianCedillo + CrewStaffBrianDunham + 
    CrewStaffBrianFrankenfield + CrewStaffBrianKelly + CrewStaffBrianOldmen + 
    CrewStaffBrittneyCsorba + CrewStaffBryanHoltan + CrewStaffCaseyVaccarezza + 
    CrewStaffChaseHolt + CrewStaffChrisArnold + CrewStaffCoreyNakano + 
    CrewStaffDanFellows + CrewStaffDavidMurdoch + CrewStaffDustinDustin2006AG + 
    CrewStaffIanNicholson + CrewStaffIchiroShiatsubo + CrewStaffJacobSchwank + 
    CrewStaffJakeTollison + CrewStaffJeffMarkland + CrewStaffJeffNester + 
    CrewStaffJoelBuonassissi + CrewStaffJoeVillareal + CrewStaffJohnBlackman + 
    CrewStaffJohnnyChastine + CrewStaffJoshTanner + CrewStaffKevinMartin + 
    CrewStaffLarryUgale + CrewStaffMattMatt2005AG + CrewStaffMattThomas + 
    CrewStaffMikeMayberry + CrewStaffMinorAnglerAggressor + CrewStaffMinorAnglerMirage + 
    CrewStaffMinorAnglerToronado + CrewStaffNickEscobedo + CrewStaffNickNguyen + 
    CrewStaffOscarRodriguez + CrewStaffPatrickPham + CrewStaffPaulHansen + 
    CrewStaffPaulPennello + CrewStaffRayBlakeslee + CrewStaffRonMueller + 
    CrewStaffRyanConlin + CrewStaffRyanGriffin + CrewStaffScottGautreau + 
    CrewStaffSeanHagerty + CrewStaffSeanHartigan + CrewStaffShaunVanderpol + 
    CrewStaffShawnTerrell + CrewStaffShonRoberts + CrewStaffSteveChesley + 
    CrewStaffTimLonghorn + CrewStaffTimTim2006AG + CrewStaffTomVeloz + 
    CrewStaffTroySteranko + CrewStaffTuckerMcCombs + CrewStaffVictorRamirez + 
    drop_number2 + drop_number3 + drop_number4 + drop_number5 + 
    hook_number2 + hook_number3 + hook_number4 + hook_number5_Top + 
    drop_time_proportion_of_solar_day.P1 + drop_time_proportion_of_solar_day.P2 + 
    drop_time_proportion_of_solar_day.P3 + moon_percent_fullness_r.P1 + 
    moon_percent_fullness_r.P2 + moon_percent_fullness_r.P3 + 
    wave_height_m.P1 + wave_height_m.P2 + moon_percent_fullness_r.P1:wave_height_m.P1 + 
    moon_percent_fullness_r.P2:wave_height_m.P1 + moon_percent_fullness_r.P3:wave_height_m.P1 + 
    moon_percent_fullness_r.P1:wave_height_m.P2 + moon_percent_fullness_r.P2:wave_height_m.P2 + 
    moon_percent_fullness_r.P3:wave_height_m.P2 + drop_time_proportion_of_solar_day.P1:moon_percent_fullness_r.P1 + 
    drop_time_proportion_of_solar_day.P2:moon_percent_fullness_r.P1 + 
    drop_time_proportion_of_solar_day.P3:moon_percent_fullness_r.P1 + 
    drop_time_proportion_of_solar_day.P1:moon_percent_fullness_r.P2 + 
    drop_time_proportion_of_solar_day.P2:moon_percent_fullness_r.P2 + 
    drop_time_proportion_of_solar_day.P3:moon_percent_fullness_r.P2 + 
    drop_time_proportion_of_solar_day.P1:moon_percent_fullness_r.P3 + 
    drop_time_proportion_of_solar_day.P2:moon_percent_fullness_r.P3 + 
    drop_time_proportion_of_solar_day.P3:moon_percent_fullness_r.P3

 
# Binomial model with default logit link
GLM <- glm(NumVerm ~ year + site_number + CrewStaff + drop_number + hook_number + poly(drop_time_proportion_of_solar_day, 3) + poly(moon_percent_fullness_r, 3) + poly(wave_height_m, 2) + 
             poly(moon_percent_fullness_r, 3):poly(wave_height_m, 2) + poly(drop_time_proportion_of_solar_day, 3):poly(moon_percent_fullness_r, 3), 
             data = Verm.Final.Model.MCMC.2019$DATA, family = 'binomial')

dev.new()   
par(mfrow = c(3, 3))             
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 0)

# Select the scale at 11, the max diff(ylim), and replot
dev.new()
par(mfrow = c(3, 3))
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 11)

# # Warning message:
# # In preplot.Gam(x, terms = terms) :
# #   No terms saved for "a:b" style interaction terms


# Save this figure to 'Verm.ALL.GAM.Fig.png' manaully. For this figure, saving to PNG with a manual save from the Windows R console gives a better result than using png().
 

# ANOVA-like table   
(ANOVA <- anova(GLM, test = 'Chisq'))

# AIC table (requires CRAN package: AICcmodavg)
(AICTAB <- JRWToolBox::icTableGlm(GLM))
 
capture.output(cat("\n\n"), ANOVA, cat("\n\n"), AICTAB, file = paste0(substring(as.character(Verm.Final.Model.MCMC.2019$Final.Model)[2], 4), ".Analysis.of.Deviance.and.AIC.Tables.txt"))
 


# -----------------------------------------------------------------------------------------------------------------------
# ===== Vermilion, Area = CCA; propHookCutOffAggr = 0.040, propHookCutOffMirage = 0.010, propHookCutOffToro = 0.010 -----
# -----------------------------------------------------------------------------------------------------------------------

# Jump straight to the 1m MCMC  
dir.create('Verm.2019.NFT.1m.CCA', showWarnings = FALSE)
setwd('Verm.2019.NFT.1m.CCA'); getwd()
Verm.2019.NFT.1m.CCA <- HandL.stepAIC.MCMC(Y.Name = "NumVerm", common_name = "Vermilion Rockfish", Area = c('Orig121', 'CCA', 'ALL')[2], Include.FishTime = FALSE, 
                             reducedFormula = TRUE, propHookCutOffMirage = 0.025, buffer = c(15, 45), tune = 0.14, mcmc = 1e6, burnin = 1000, thin = 1000, verbose = 1000,
                             grandPathCSV = "../qryGrandUnifiedThru2019_For2021Assessments_DWarehouse version.csv")
                                           

# Check the MCMC autocorrelation                          
dev.new()
plot(MCMC, ask = TRUE)

dev.new()
autocorr.plot(MCMC, ask = TRUE)  

# Looks resonable, so save some autocorr plots
dir.create("Auto Corr Plots 1mil MCMC", showWarnings = FALSE)
setwd("Auto Corr Plots 1mil MCMC"); getwd()
png(filename = "Verm.1m.MCMC.autocorr%03d.png", width = 960, height = 960)
autocorr.plot(MCMC, ask = FALSE)  
dev.off()
setwd('..'); getwd()


Verm.Final.Model.MCMC.2019$Final.Model

NumVerm ~ year2015 + year2016 + year2017 + year2018 + year2019 + 
    site_number506 + site_number507 + site_number508 + site_number509 + 
    site_number512 + site_number514 + site_number516 + site_number518 + 
    site_number519 + site_number520 + site_number521 + site_number523 + 
    site_number524 + site_number525 + site_number526 + site_number527 + 
    site_number528 + site_number531 + site_number533 + site_number537 + 
    site_number538 + site_number539 + site_number540 + site_number543 + 
    site_number546 + site_number547 + site_number548 + site_number549 + 
    site_number550 + site_number551 + site_number553 + site_number556 + 
    site_number557 + site_number558 + site_number559 + site_number560 + 
    site_number561 + site_number562 + site_number563 + site_number564 + 
    site_number566 + site_number568 + site_number569 + site_number573 + 
    site_number574 + site_number575 + site_number577 + site_number581 + 
    site_number582 + site_number583 + site_number584 + site_number586 + 
    site_number587 + site_number590 + site_number591 + site_number592 + 
    site_number593 + site_number594 + site_number595 + site_number596 + 
    site_number597 + site_number598 + site_number601 + site_number606 + 
    site_number607 + site_number609 + site_number611 + site_number613 + 
    CrewStaffAlexEdwards + CrewStaffAndrewPerez + CrewStaffBlairWilliams + 
    CrewStaffBrianCedillo + CrewStaffBrianKelly + CrewStaffBrittneyCsorba + 
    CrewStaffCaseyVaccarezza + CrewStaffChaseHolt + CrewStaffCoreyNakano + 
    CrewStaffDanFellows + CrewStaffDavidMurdoch + CrewStaffJacobSchwank + 
    CrewStaffJoeVillareal + CrewStaffJohnnyChastine + CrewStaffKevinMartin + 
    CrewStaffLarryUgale + CrewStaffMattThomas + CrewStaffMinorAnglerAggressor + 
    CrewStaffMinorAnglerMirage + CrewStaffMinorAnglerToronado + 
    CrewStaffNickEscobedo + CrewStaffOscarRodriguez + CrewStaffPatrickPham + 
    CrewStaffPaulHansen + CrewStaffPaulPennello + CrewStaffRyanConlin + 
    CrewStaffSeanHagerty + CrewStaffShaunVanderpol + CrewStaffShawnTerrell + 
    CrewStaffSteveChesley + CrewStaffTomVeloz + CrewStaffTroySteranko + 
    CrewStaffTuckerMcCombs + CrewStaffVictorRamirez + drop_number2 + 
    drop_number3 + drop_number4 + drop_number5 + hook_number2 + 
    hook_number3 + hook_number4 + hook_number5_Top + moon_percent_fullness_r.P1 + 
    moon_percent_fullness_r.P2 + moon_percent_fullness_r.P3 + 
    drop_time_proportion_of_solar_day.P1 + drop_time_proportion_of_solar_day.P2 + 
    drop_time_proportion_of_solar_day.P3

  
    

GLM <- glm(NumVerm ~ year + site_number + CrewStaff + drop_number + hook_number + poly(moon_percent_fullness_r, 3) + poly(drop_time_proportion_of_solar_day, 3), 
             data = Verm.Final.Model.MCMC.2019$DATA, family = 'binomial')

dev.new()   
par(mfrow = c(3, 3))             
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 0)

# Select the scale at 7.6, the max diff(ylim), and replot
dev.new()
par(mfrow = c(3, 3))
plot.Gam(GLM, se = TRUE, rugplot = TRUE, scale = 7.6)

# Save this figure to 'Verm.CCA.GAM.Fig.png' manaully. For this figure, saving to PNG with a manual save from the Windows R console gives a better result than using png().
 

# ANOVA-like table   
(ANOVA <- anova(GLM, test = 'Chisq'))

# AIC table (requires CRAN package: AICcmodavg)
(AICTAB <- JRWToolBox::icTableGlm(GLM))
 
capture.output(cat("\n\n"), ANOVA, cat("\n\n"), AICTAB, file = paste0(substring(as.character(Verm.Final.Model.MCMC.2019$Final.Model)[2], 4), ".Analysis.of.Deviance.and.AIC.Tables.txt"))
 



