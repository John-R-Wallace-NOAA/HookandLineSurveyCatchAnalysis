# ------ Squarespot, Area = 'Orig121' -----
   To be added soon


# ------ Squarespot, Area = 'ALL' -----
   To be added soon



                           
# ------ Squarespot, Area = 'CCA' -----
# Note here tune = 0.17. From MCMClogit()'s help for 'tune' arg: Make sure that the acceptance rate is satisfactory (typically between 0.20 and 0.5) before using the posterior sample for inference.
# The default for tune is 0.17, but for the non-CCA '121 sites' and the all sites models for Squarespot, tune = 0.14. 

# -- For Tantalus --
# /opt/R/64-bit/R-4.0.1_MKL/bin/R
# options(width = 160)
  
# For R-MKL multithreading   
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

par(mfrow = c(3, 2))
plot.Gam(glm(NumSqSpot ~ year + site_number + CrewStaff + drop_number + hook_number + poly(moon_percent_fullness_r, 3), data = DATA), se = TRUE, rugplot = TRUE, scale = 0)

# Ignoring (for this figure) the extreme variability of MinorAnglerAggressor in CrewStaff, select the scale at 0.2, the max diff(ylim) seen in site_number.
# The crew staff and vessel captains are allowed to fish on rare occasions for morale purposes, but the sites can be better than average.
# Looking back at the design plot (the one with "Mean of NumSqSpot" vs factors in the model) we see that MinorAnglerAggressor does very well. 
# See the help for graphics::plot.design(). This design plot created with JRWToolBox::plot.design.jrw() with has improved labeling.
# Adding other minor anglers to CrewStaffMinorAnglerAggressor could be considered.

par(mfrow = c(3, 2))
plot.Gam(glm(NumSqSpot ~ year + site_number + CrewStaff + drop_number + hook_number + poly(moon_percent_fullness_r, 3), data = DATA), se = TRUE, rugplot = TRUE, scale = 0.2)

# For this figure, saving to PNG with a manual save from the Windows R console gives a better result than using png().

# png("SqSpot.GAM.Fig.png", width = 1000, height = 1000)
# par(mfrow = c(3, 2))
# plot.Gam(glm(NumSqSpot ~ year + site_number + CrewStaff + drop_number + hook_number + poly(moon_percent_fullness_r, 3), data = DATA), se = TRUE, rugplot = TRUE, scale = 0.2)
# dev.off()  
 
dev.new()
plot.design.jrw(DATA[, c("year", "site_number", "vessel", "drop_number", "hook_number", "angler_number", "moon_phase_r", "CrewStaff")], DATA[, 'NumSqSpot'], ylab = paste("Mean of NumSqSpot"))
 
