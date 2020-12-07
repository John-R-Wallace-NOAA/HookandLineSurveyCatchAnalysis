
HandL.stepAIC.MCMC <- function(Y.Name = "NumBoc", common_name = "Bocaccio", Area = c('Orig121', 'CCA', 'ALL')[1], Include.FishTime = FALSE, Restrict.6min = FALSE, Sites = NULL, 
                             propHookCutOffAggr = if(Area == "ALL") 0.002 else 0.003, propHookCutOffMirage = propHookCutOffAggr, propHookCutOffToro = 3 * propHookCutOffAggr,
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
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/catf.R")
     
     require(lattice)
     require(Hmisc)
     
     if(is.null(GLM.FINAL.AIC)) {
     
        # Import the data
        Grand <- importData(grandPathCSV = grandPathCSV)
 
        # Process the data and create minor angler groups
        DATA <- processData(Y.Name = Y.Name, common_name = common_name, Grand = Grand, Include.FishTime = Include.FishTime, Restrict.6min = Restrict.6min, Area = Area, 
                             reducedFormula = reducedFormula, propHookCutOffAggr = propHookCutOffAggr, propHookCutOffMirage = propHookCutOffAggr, propHookCutOffToro = propHookCutOffToro)
        
        switch(menu("Enter 1 (one) if anglers with a small number of hooks fished need to be added to the minor anglers, or if the minor anglers group is too small compared with the other anglers? (Enter 0 (zero) to skip.)") + 1, cat("\n"), {
            catf("\nChange the proportion of hooks fished cutoff value argument for the correct vessel and rerun HandL.MCMClogit()\n")
            catf("     Any angler on a given vessel that has fished less hooks than the proportion cutoff times the total hooks fished on that vessel will be put into the minor anglers group.\n") 
            catf("     The total number of hooks is based on the Area argument, is over all years of the data, and is over sites based on species occurence.\n")
            catf("     A site is only in the model if the given species has been caught at that site as least twice over all years.\n\n")
            catf("     If the minor anglers group is too small add in the next angler with the next smallest hooks fished.\n\n")
            # stop("No error - just stopping for the argument value change...")
            return()
        })
         
        # Look at the imported data
        dev.new(width =16, height = 10)
        plot.design.jrw(DATA[, c("year", "site_number", "vessel", "drop_number", "hook_number", "angler_number", "moon_phase_r", "sex", "CrewStaff")], DATA[, Y.Name], ylab = paste("Mean of", Y.Name))
        
        catf("\n\nThe weight by length figure with groups by sex is only for QA/QC. Length, weight, and sex are not used in this model.\n\n")
        dev.new()
        print(xyplot(weight_kg ~ length_cm | year, groups = ordered(sex, c('M', 'F', 'U')), cex = c(1, 0.4, 1), data = DATA, xlab = "Length (cm)", ylab = "Weight (kg)", auto = TRUE)) 
     }
    
     DATA <- DATA[, - grep('weight_kg', names(DATA))]
     DATA <- DATA[, - grep('length_cm', names(DATA))] 
     DATA <- na.omit(DATA)
    
     # Use MCMClogit from MCMCpack package
     stepAICList <- stepAIC.I.MCMC(Y.Name = Y.Name, DATA = DATA, Area = Area, VermComplex = VermComplex, Interaction = Interaction, Include.FishTime = Include.FishTime, 
                      reducedFormula = reducedFormula, buffer = buffer, contrast = contrast, DIC.Check = DIC.Check, tune = tune, mcmc = mcmc, burnin = burnin, 
                      thin = thin, verbose = verbose, Stop.before.MCMC = Stop.before.MCMC, MAIN.STEP.AIC = MAIN.STEP.AIC, STEP.AIC = STEP.AIC, 
                      GLM.FINAL.AIC = GLM.FINAL.AIC) 
    
    
    # Print results table
    catf("\n\n"); print(stepAICList$SS.Table); catf("\n")
    capture.output(print(stepAICList$SS.Table), file = paste0(Y.Name, ".SS.Table.txt"))
    
    
    # Print the final figure
    #      To redo the figure below for a new placement of '(a)' of '(b)' use stepAICList <- < the list this function was saved to >, e.g. stepAICList <- SqSpot.2019.NFT.1k.ALL, 
    #      change 0.10 in "2004.5, yRange[1] + 0.10 * diff(yRange)" below to a different value, and rerun. 
    
    YEARS <- 2004:max(as.numeric(as.character(DATA$year)))
    
    FINAL.GLM <- stepAICList$Final.glm.coef
    E.M <- FINAL.GLM$Estimate
    CI.L <- FINAL.GLM$Estimate - 1.96 * FINAL.GLM$Std.Error
    CI.H <- FINAL.GLM$Estimate + 1.96 * FINAL.GLM$Std.Error
  
    #     Output to current device
    trellis.device(theme = "col.whitebg") # This creates a new plotting device (this may be needed on older versions of R)
   
    yRange <- c(min(c(stepAICList$Q.MCMC[2,], 0)), max(stepAICList$Q.MCMC[3,]))
    yRange <- c(yRange[1] - 0.05 * diff(yRange), yRange[2] + 0.05 * diff(yRange))
    print(xYplot(Cbind(stepAICList$Q.MCMC[1,], stepAICList$Q.MCMC[2,], stepAICList$Q.MCMC[3,]) ~ YEARS, type = 'o', ylab = list("Index", cex = 1.2), xlab = "", 
         ylim = yRange, col = 'black', panel = function(subscripts = subscripts, ...) {panel.xYplot(subscripts = subscripts, ...); 
         ltext(2004.5, yRange[1] + 0.10 * diff(yRange), "(a)", cex = 1.2)}), split = c(1,1,1,2), more = TRUE)
    
    
    yRange <- c(min(c(CI.L, 0)), max(CI.H))
    yRange <- c(yRange[1] - 0.05 * diff(yRange), yRange[2] + 0.05 * diff(yRange))
    print(xYplot(Cbind(E.M, CI.L, CI.H) ~ YEARS, type='o', ylab=list("Year effect coefficients", cex=1.2), xlab = list("Year", cex = 1.2), 
         ylim = yRange, col='black', panel=function(subscripts=subscripts, ...) {panel.xYplot(subscripts=subscripts, ...); 
         ltext(2004.5, yRange[1] + 0.10 * diff(yRange), "(b)", cex=1.2)}), split = c(1,2,1,2), more = FALSE)
    
    
    #     Output to PNG
    png(width = 1024, height = 1024, file = paste0(Y.Name, ".MCMC.Index.and.Year.Effect.Coeff.png"))
       
    yRange <- c(min(c(stepAICList$Q.MCMC[2,], 0)), max(stepAICList$Q.MCMC[3,]))
    yRange <- c(yRange[1] - 0.05 * diff(yRange), yRange[2] + 0.05 * diff(yRange))
    print(xYplot(Cbind(stepAICList$Q.MCMC[1,], stepAICList$Q.MCMC[2,], stepAICList$Q.MCMC[3,]) ~ YEARS, type = 'o', ylab = list("Index", cex = 1.2), xlab = "", 
         ylim = yRange, col = 'black', panel = function(subscripts = subscripts, ...) {panel.xYplot(subscripts = subscripts, ...); 
         ltext(2004.5, yRange[1] + 0.10 * diff(yRange), "(a)", cex = 1.2)}), split = c(1,1,1,2), more = TRUE)
    
    
    yRange <- c(min(c(CI.L, 0)), max(CI.H))
    yRange <- c(yRange[1] - 0.05 * diff(yRange), yRange[2] + 0.05 * diff(yRange))
    print(xYplot(Cbind(E.M, CI.L, CI.H) ~ YEARS, type='o', ylab=list("Year effect coefficients", cex=1.2), xlab = list("Year", cex = 1.2), 
         ylim = yRange, col='black', panel=function(subscripts=subscripts, ...) {panel.xYplot(subscripts=subscripts, ...); 
         ltext(2004.5, yRange[1] + 0.10 * diff(yRange), "(b)", cex=1.2)}), split = c(1,2,1,2), more = FALSE)
    
    dev.off()
    
    stepAICList 

}

