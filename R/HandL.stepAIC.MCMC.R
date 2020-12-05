
HandL.stepAIC.MCMC <- function(Y.Name = "NumBoc", common_name = "Bocaccio", Area = c('Orig121', 'CCA', 'ALL')[1], Include.FishTime = FALSE, Restrict.6min = FALSE, Sites = NULL, 
                             propHookCutOffAggr = ifelse(Area == "ALL", 0.002, 0.003), propHookCutOffMirage = propHookCutOffAggr, propHookCutOffToro = 3 * propHookCutOffAggr,
                             VermComplex = FALSE, Interaction = TRUE, reducedFormula = FALSE, buffer = c(15, 45), contrast = 'treatment', tune = 0.14, mcmc = 150000, burnin = 1000, 
                             thin = 150, verbose = 1000, Stop.before.MCMC = FALSE, MAIN.STEP.AIC = NULL, STEP.AIC = NULL, GLM.FINAL.AIC = NULL, grandPathCSV = NULL) {

     #   -------- Import utility Functions --------
     sourceFunctionURL <- function(URL) {
        ' # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() '
        require(RCurl)
        File.ASCII <- tempfile()
        on.exit(file.remove(File.ASCII))
        writeLines(paste(readLines(textConnection(RCurl::getURL(URL))), collapse = "\n"), File.ASCII)
        source(File.ASCII, local = parent.env(environment()))
     }
     
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/plot.design.jrw.R")
     
     require(lattice)
     require(Hmisc)
     
     if(is.null(GLM.FINAL.AIC)) {
     
        # Import the data
        Grand <- importData(grandPathCSV = grandPathCSV)
     
        # Process the data and create minor angler groups
        DATA <- processData(Y.Name = Y.Name, common_name = common_name, Grand = Grand, Include.FishTime = Include.FishTime, Restrict.6min = Restrict.6min, Sites = Sites, Area = Area, 
                             reducedFormula = FALSE, propHookCutOffAggr = propHookCutOffAggr, propHookCutOffMirage = propHookCutOffAggr, propHookCutOffToro = 3 * propHookCutOffAggr)
        
        switch(menu("Do other anglers with a small number of hooks fished need to be added to the minor anglers? (Use zero (0) to skip.)") + 1, cat("\n"), {
            cat("\nChange the proportion of hooks fished cutoff value argument for the correct vessel and rerun HandL.MCMClogit()\n")
            cat(paste0("     The current values are: propHookCutOffAggr = ", propHookCutOffAggr, ", propHookCutOffMirage = ", propHookCutOffMirage, ", propHookCutOffToro = ", propHookCutOffToro, "\n"))
            cat("     Any angler on a given vessel that has fished less hooks than the proportion cutoff times the total hooks fished on that vessel will be put into the minor anglers group.\n") 
            cat("     The total number of hooks is based on the Area argument, is over all years of the data, and is over sites based on species occurence.\n")
            cat("     A site is only in the model if the given species has been caught at that site as least twice over all years.\n\n")
            stop("No error - just stopping for the argument value change...")
        })
         
        # Look at the imported data
        dev.new()
        plot.design.jrw(DATA[, c("year", "site_number", "vessel", "drop_number", "hook_number", "angler_number", "moon_phase_r", "sex", "CrewStaff")], DATA[, Y.Name], ylab = paste("Mean of", Y.Name))
        
        dev.new()
        print(xyplot(weight_kg ~ length_cm | year, groups = ordered(sex, c('M', 'F', 'U')), cex = c(1, 0.4, 1), data = DATA, auto = TRUE)) 
     }
    
     DATA <- DATA[, - grep('weight_kg', names(DATA))]
     DATA <- DATA[, - grep('length_cm', names(DATA))] 
     DATA <- na.omit(DATA)
    
     # Use MCMClogit from MCMCpack package
     stepAICList <- stepAIC.I.MCMC(Y.Name = Y.Name, DATA = DATA, VermComplex = VermComplex, Interaction = Interaction, Include.FishTime = Include.FishTime, 
                      reducedFormula = reducedFormula, buffer = buffer, contrast = contrast, DIC.Check = DIC.Check, tune = tune, mcmc = mcmc, burnin = burnin, 
                      thin = thin, verbose = verbose, Stop.before.MCMC = Stop.before.MCMC, MAIN.STEP.AIC = MAIN.STEP.AIC, STEP.AIC = STEP.AIC, 
                      GLM.FINAL.AIC = GLM.FINAL.AIC) 
                      
    
     
     
     
    # Print results table
    print(stepAICList$SS.Table)
  
    # Print the final two figures
    YEARS <- 2004:max(as.numeric(as.character(DATA$year)))
    
    print(xYplot(Cbind(stepAICList$Q.MCMC[1,], stepAICList$Q.MCMC[2,], stepAICList$Q.MCMC[3,]) ~ YEARS, type='o', ylab=list("Index", cex=1.2), xlab = list("Year", cex = 1.2), 
        panel = function(subscripts=subscripts, ...) {panel.xYplot(subscripts=subscripts, ...); ltext(2004.5, 0.25, "(b)", cex=1.2)}), more = FALSE)
  
    FINAL.GLM <- stepAICList$Final.glm.coef
    E.M <- FINAL.GLM$Estimate
    CI.L <- FINAL.GLM$Estimate - 1.96 * FINAL.GLM$Std.Error
    CI.H <- FINAL.GLM$Estimate + 1.96 * FINAL.GLM$Std.Error
  
   
    #  E.M <- Boc.Final.Model.MCMC.2014$Final.glm.coef$Estimate
    # CI.L <- Boc.Final.Model.MCMC.2014$Final.glm.coef$Estimate - 1.96 * Boc.Final.Model.MCMC.2014$Final.glm.coef$Std.Error
    # CI.H <- Boc.Final.Model.MCMC.2014$Final.glm.coef$Estimate + 1.96 * Boc.Final.Model.MCMC.2014$Final.glm.coef$Std.Error
  
    trellis.device(theme = "col.whitebg")
  
    print(xYplot(Cbind(stepAICList$Q.MCMC[1,], stepAICList$Q.MCMC[2,], stepAICList$Q.MCMC[3,]) ~ YEARS, type='o', ylab=list("Index", cex=1.2), xlab="", 
        ylim=c(-0.01, .25), col='black', scales = list(y=list(at=c(0.0, 0.05, 0.10, 0.15, 0.20, 0.25), 
        labels=c("0.00", "0.05", "0.10", "0.15", "0.20", "0.25"), cex=1), x=list(at=2000, label=""), tck=c(1,0)), 
        panel=function(subscripts=subscripts, ...) {panel.xYplot(subscripts=subscripts, ...); ltext(2004.5, 0.10, "(a)", cex=1.2)}),
        split=c(1,1,1,2), more = TRUE)
    
    print(xYplot(Cbind(E.M, CI.L, CI.H) ~ YEARS, type='o', ylab=list("Year effect coefficients", cex=1.2), xlab=list("Year", cex=1.2), 
        ylim=c(-2, 0.35), col='black', scales = list(y=list(at=c(-2.00, -1.75, -1.50, -1.25, -1.00, -0.75, -0.50, -0.25, 0.00, 0.25),
        labels=c("-2.00", "-1.75", "-1.50", "-1.25", "-1.00", "-0.75", "-0.50", "-0.25", "0.00", "0.25"), cex=1), x=list(at=YEARS, cex=1), tck=c(1,0)),
        panel=function(subscripts=subscripts, ...) {panel.xYplot(subscripts=subscripts, ...); ltext(2004.5, -1.25, "(b)", cex=1.2)}),
        split=c(1,2,1,2), more = FALSE)
    

    stepAICList 

}

