
sourceFunctionURL <-function(URL) {
          ' # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() '
          require(RCurl)
          File.ASCII <- tempfile()
          on.exit(file.remove(File.ASCII))
          writeLines(paste(readLines(jsonlite::fromJSON((URL))), collapse = "\n"), File.ASCII)
          source(File.ASCII, local = parent.env(environment()))
       }
       
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/plotCI_Yaxis.jrw4.R")
       
require(gtools) 
require(lattice)


load("\\\\nwctantalus.nmfs.local\\jwallace\\h_jwallace\\HandL2020\\Verm.2019.NFT.1m.121\\Verm.Final.Model.MCMC.2019.RData")
load("\\\\nwctantalus.nmfs.local\\jwallace\\h_jwallace\\HandL2020\\Verm.2019.NFT.1m.121\\Verm.MCMC.RData")

# Backup 
# load("W:\\ALL_USR\\JRW\\Hook & Line Survey\\2020\\Models\\Vermilion\\From Tantalus\\Verm.Final.Model.MCMC.2019.RData")

Verm.MM <- model.matrix(NumVerm ~ year + site_number + CrewStaff + drop_number + hook_number + poly(moon_percent_fullness_r, 2) + poly(drop_time_proportion_of_solar_day, 3) + 
                       poly(wave_height_m, 2), data = Verm.Final.Model.MCMC.2019$DATA)

Verm.Pred.MCMC <- inv.logit(as.matrix(MCMC) %*% t(Verm.MM))
dim(Verm.Pred.MCMC) # 1000 126010

Q.Verm.Pred <- t(apply(Verm.Pred.MCMC, 2, quantile, probs = c(0.50, 0.025, 0.975), type = 8))
colnames(Q.Verm.Pred) <- c('NumPred.Median.MCMC', 'Q.MCMC_2.5', 'Q.MCMC_97.5')

DATA <- cbind(Verm.Final.Model.MCMC.2019$DATA, Q.Verm.Pred))
VermCatchBySite.MCMC <- cbind(aggregate(list(RawCatchBySite = DATA$NumVerm), list(Site = site_number, Year = year), mean), 
                         Pred.MCMC.Median.CatchBySite = aggregate(list(DATA$NumPred.Median.MCMC), list(Site = site_number, Year = year), mean)[, 3],
                         Pred.Q.MCMC_2.5 = aggregate(list(DATA$Q.MCMC_2.5), list(Site = site_number, Year = year), mean)[, 3],
                         Pred.Q.MCMC_97.5 = aggregate(list(DATA$Q.MCMC_97.5), list(Site = site_number, Year = year), mean)[, 3])
dim(VermCatchBySite.MCMC) #  1728    6

xyplot(Pred.MCMC.Median.CatchBySite ~ as.num(RawCatchBySite) | Year, data = VermCatchBySite.MCMC, panel=function(...) {panel.xyplot(...); panel.abline(0, 1, col = 'red')})

# All years and sites - too messy
VM <- VermCatchBySite.MCMC
dev.new(width = 14, height = 10)
plotCI_Yaxis.jrw4(VM$RawCatchBySite, VM$Pred.MCMC.Median.CatchBySite, VM$Pred.Q.MCMC_2.5, VM$Pred.Q.MCMC_97.5, lowerLimtCI = 0, upperLimitCI = 1, 
          col = 'red', CI.col = col.alpha('black', 0.5), ylab = 'Predicted Median Catch Rate by Site within Year using MCMC', xlab = 'Observed Catch Rate by Site within Year')
abline(0,1, col = 'dodgerblue')


# By Year is better
(yearList <- list( One = 2004:2009, Two = 2010:2015, Three = 2016:2019))

# Saved directly out of R to the names: paste0("Verm, Pred vs Obs with Credible Interval ", min(yearList[[i]]), "_", max(yearList[[i]]), ".png")
for(i in 1:3) {
   dev.new()
   par(mfrow = c(3, 2))
   for (YEAR in yearList[[i]]) {
       VM <- VermCatchBySite.MCMC[VermCatchBySite.MCMC$Year %in% YEAR, ]
       plotCI_Yaxis.jrw4(VM$RawCatchBySite, VM$Pred.MCMC.Median.CatchBySite, VM$Pred.Q.MCMC_2.5, VM$Pred.Q.MCMC_97.5, lowerLimtCI = 0, upperLimitCI = 1, main = YEAR,
                 ylim = c(-0.02, 0.85), col = 'red', CI.col = col.alpha('black', 0.5), 
                 ylab =  ifelse(YEAR %in% seq(2004, 2018, by = 2), 'MCMC Pred Prop by Site', ''), 
                 xlab = ifelse(YEAR %in% c(2008, 2009, 2014, 2015, 2018, 2019), 'Observed Catch Rate by Site within Year', ''))
       abline(0,1, col = 'dodgerblue')
   }
 }

