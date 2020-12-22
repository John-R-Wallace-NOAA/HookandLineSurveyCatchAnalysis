
# NFT = No Fishing Time
# vessel is taken out of the model since it is colinear with CrewStaff, but then recreated later

stepAIC.I.MCMC <- function(Y.Name = 'NumBoc', DATA = DATA, Area = c("Orig121", "CCA", "ALL")[1], VermComplex = FALSE, Interaction = TRUE, Include.FishTime = FALSE, 
         reducedFormula = FALSE, buffer = NULL, contrast = 'treatment', DIC.Check = 3, tune = 0.17, mcmc = 500000, burnin = 3000, thin = 500, verbose = 1000, 
         Stop.before.MCMC = FALSE, MAIN.STEP.AIC = NULL, STEP.AIC = NULL, GLM.FINAL.AIC = NULL) {
  
  
    #   -------- Import utility Functions --------
       sourceFunctionURL <- function(URL) {
          ' # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() '
          require(RCurl)
          File.ASCII <- tempfile()
          on.exit(file.remove(File.ASCII))
          writeLines(paste(readLines(textConnection(RCurl::getURL(URL))), collapse = "\n"), File.ASCII)
          source(File.ASCII, local = parent.env(environment()))
       }
       
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/printf.R")
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/catf.R")
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/DIC.R")
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
       
    require(gtools) 
    require(MASS)
    require(gam)
    require(MCMCpack)
    
    fix.poly.names <- function(xx) {
    
     single.poly <- function(x) {
         if(length(grep("poly", c("poly", x))) == 2) {
             x <- get.subs(substring(x, 6))
             paste(x[1], paste("P", substring(x[2],4,4), sep=""), sep=".")
          } else 
          x 
      }
    
      apply(matrix(xx, ncol=1),1, function(x) {
    
      if(length(grep("poly", c("poly", x))) == 2) {
    
         if(length(grep(":", c(":", x))) == 2) {
    
            x <- get.subs(x, ":")
            paste(single.poly(x[1]), single.poly(x[2]), sep=":")    
    
         
         } else {
    
            x <- get.subs(substring(x, 6))
            paste(x[1], paste("P", substring(x[2],4,4), sep=""), sep=".")
         }
       } else 
          x 
     })
    }
   
   
    Test.poly.level <- function(x = 'TideFlow2', FORMULA = BASE.FORMULA, data = DATA, tolAIC = 1.9) {
    
      catf("\nVariable:", x, "\n\n")  
    
      printf(D <- AIC(glm(formula(FORMULA), family = binomial, data = data)))
    
      P1.VAR <- paste(" + ", x, sep="")
      printf(formula(paste(FORMULA, P1.VAR, sep="")))
      printf(P1 <- AIC(glm(formula(paste(FORMULA, P1.VAR, sep="")), family = binomial, data = data)))
    
      P2.VAR <- paste(" + poly(", x, ",2)", sep="")
      printf(formula(paste(FORMULA, P2.VAR, sep="")))
      printf(P2 <- AIC(glm(formula(paste(FORMULA, P2.VAR, sep="")), family = binomial, data = data)))
    
      P3.VAR <- paste(" + poly(", x, ",3)", sep="")
      printf(formula(paste(FORMULA, P3.VAR, sep="")))
      printf(P3 <- AIC(glm(formula(paste(FORMULA, P3.VAR, sep="")), family = binomial, data = data)))
    
      
      if(min(c(P1, P2, P3) > D - tolAIC))
          return(NULL)
    
      if(P2 > P1 - tolAIC & P3 > P1 - tolAIC)
       return(P1.VAR)
    
      if(P3 > P2 - tolAIC)
       return(P2.VAR)
      else
       P3.VAR   
    
    }
   
   
    reduce.formula <- function(FORMULA, NUM) {
   
      FC <- as.character(FORMULA)
      SUBS <- get.subs(as.character(FC)[3], sep="+")
      formula(paste(FC[2], FC[1],  paste(SUBS[-((length(SUBS) - NUM + 1):length(SUBS))], collapse="+")))
    }
   
   

    stepAIC.buffer <- function (object, scope, scale = 0, buffer = 1.95, direction = c("both", "backward", 
                          "forward"), trace = 1, keep = NULL, steps = 1000, use.start = FALSE, k = 2, ...) {
        
        # stepAIC is from the MASS packaage with the buffer argument added 
        mydeviance <- function(x, ...) {
            dev <- deviance(x)
            if (!is.null(dev)) 
                dev
            else extractAIC(x, k = 0)[2L]
        }
        cut.string <- function(string) {
            if (length(string) > 1L) 
                string[-1L] <- paste("\n", string[-1L], sep = "")
            string
        }
        re.arrange <- function(keep) {
            namr <- names(k1 <- keep[[1L]])
            namc <- names(keep)
            nc <- length(keep)
            nr <- length(k1)
            array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, 
                namc))
        }
        step.results <- function(models, fit, object, usingCp = FALSE) {
            change <- sapply(models, "[[", "change")
            rd <- sapply(models, "[[", "deviance")
            dd <- c(NA, abs(diff(rd)))
            rdf <- sapply(models, "[[", "df.resid")
            ddf <- c(NA, abs(diff(rdf)))
            AIC <- sapply(models, "[[", "AIC")
            heading <- c("Stepwise Model Path \nAnalysis of Deviance Table", 
                "\nInitial Model:", deparse(as.vector(formula(object))), 
                "\nFinal Model:", deparse(as.vector(formula(fit))), 
                "\n")
            aod <- if (usingCp) 
                data.frame(Step = change, Df = ddf, Deviance = dd, 
                    `Resid. Df` = rdf, `Resid. Dev` = rd, Cp = AIC, 
                    check.names = FALSE)
            else data.frame(Step = change, Df = ddf, Deviance = dd, 
                `Resid. Df` = rdf, `Resid. Dev` = rd, AIC = AIC, 
                check.names = FALSE)
            attr(aod, "heading") <- heading
            class(aod) <- c("Anova", "data.frame")
            fit$anova <- aod
            fit
        }
        Terms <- terms(object)
        object$formula <- Terms
        if (inherits(object, "lme")) 
            object$call$fixed <- Terms
        else if (inherits(object, "gls")) 
            object$call$model <- Terms
        else object$call$formula <- Terms
        if (use.start) 
            warning("'use.start' cannot be used with R's version of glm")
        md <- missing(direction)
        direction <- match.arg(direction)
        backward <- direction == "both" | direction == "backward"
        forward <- direction == "both" | direction == "forward"
        if (missing(scope)) {
            fdrop <- numeric(0)
            fadd <- attr(Terms, "factors")
            if (md) 
                forward <- FALSE
        }
        else {
            if (is.list(scope)) {
                fdrop <- if (!is.null(fdrop <- scope$lower)) 
                    attr(terms(update.formula(object, fdrop)), "factors")
                else numeric(0)
                fadd <- if (!is.null(fadd <- scope$upper)) 
                    attr(terms(update.formula(object, fadd)), "factors")
            }
            else {
                fadd <- if (!is.null(fadd <- scope)) 
                    attr(terms(update.formula(object, scope)), "factors")
                fdrop <- numeric(0)
            }
        }
        models <- vector("list", steps)
        if (!is.null(keep)) 
            keep.list <- vector("list", steps)
        if (is.list(object) && (nmm <- match("nobs", names(object), 
            0)) > 0) 
            n <- object[[nmm]]
        else n <- length(residuals(object))
        fit <- object
        bAIC <- extractAIC(fit, scale, k = k, ...)
        edf <- bAIC[1L]
        bAIC <- bAIC[2L]
        if (is.na(bAIC)) 
            stop("AIC is not defined for this model, so stepAIC cannot proceed")
        nm <- 1
        Terms <- terms(fit)
        if (trace) {
            cat("Start:  AIC=", format(round(bAIC, 2)), "\n", cut.string(deparse(as.vector(formula(fit)))), 
                "\n\n", sep = "")
            utils::flush.console()
        }
        models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - 
            edf, change = "", AIC = bAIC)
        if (!is.null(keep)) 
            keep.list[[nm]] <- keep(fit, bAIC)
        usingCp <- FALSE
        while (steps > 0) {
            steps <- steps - 1
            AIC <- bAIC
            fit.keep <- fit
            ffac <- attr(Terms, "factors")
            if (!is.null(sp <- attr(Terms, "specials")) && !is.null(st <- sp$strata)) 
                ffac <- ffac[-st, ]
            scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
            aod <- NULL
            change <- NULL
            if (backward && length(scope$drop)) {
                aod <- dropterm(fit, scope$drop, scale = scale, trace = max(0, 
                    trace - 1), k = k, ...)
                rn <- row.names(aod)
                row.names(aod) <- c(rn[1L], paste("-", rn[-1L], sep = " "))
                if (any(aod$Df == 0, na.rm = TRUE)) {
                    zdf <- aod$Df == 0 & !is.na(aod$Df)
                    nc <- match(c("Cp", "AIC"), names(aod))
                    nc <- nc[!is.na(nc)][1L]
                    ch <- abs(aod[zdf, nc] - aod[1, nc]) > 0.01
                    if (any(ch)) {
                      warning("0 df terms are changing AIC")
                      zdf <- zdf[!ch]
                    }
                    if (length(zdf) > 0L) 
                      change <- rev(rownames(aod)[zdf])[1L]
                }
            }
            if (is.null(change)) {
                if (forward && length(scope$add)) {
                    aodf <- addterm(fit, scope$add, scale = scale, 
                      trace = max(0, trace - 1), k = k, ...)
                    rn <- row.names(aodf)
                    row.names(aodf) <- c(rn[1L], paste("+", rn[-1L], 
                      sep = " "))
                    aod <- if (is.null(aod)) 
                      aodf
                    else rbind(aod, aodf[-1, , drop = FALSE])
                }
                attr(aod, "heading") <- NULL
                if (is.null(aod) || ncol(aod) == 0) 
                    break
                nzdf <- if (!is.null(aod$Df)) 
                    aod$Df != 0 | is.na(aod$Df)
                aod <- aod[nzdf, ]
                if (is.null(aod) || ncol(aod) == 0) 
                    break
                nc <- match(c("Cp", "AIC"), names(aod))
                nc <- nc[!is.na(nc)][1L]
                o <- order(aod[, nc])
                if (trace) {
                    printf(aod[o, ])
                    utils::flush.console()
                }
                if (o[1L] == 1) 
                    break
                change <- rownames(aod)[o[1L]]
            }
            usingCp <- match("Cp", names(aod), 0) > 0
            fit <- update(fit, paste("~ .", change), evaluate = FALSE)
            fit <- eval.parent(fit)
            if (is.list(fit) && (nmm <- match("nobs", names(fit), 
                0)) > 0) 
                nnew <- fit[[nmm]]
            else nnew <- length(residuals(fit))
            if (nnew != n) 
                stop("number of rows in use has changed: remove missing values?")
            Terms <- terms(fit)
            bAIC <- extractAIC(fit, scale, k = k, ...)
            edf <- bAIC[1L]
            bAIC <- bAIC[2L]
            if (trace) {
                cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n", cut.string(deparse(as.vector(formula(fit)))), "\n\n", sep = "")
                utils::flush.console()
            }
           # The new candidate model has to be better (smaller) than (AIC - buffer) of the current model for the break of the loop NOT to occur.
           # So if the buffer = 1.95 (the default), then the new candidate model has to be almost 2 AIC uints better for the AIC stepping to continue. 
           # If the buffer is significantly greater than 2 AIC units then the new model has to be a lot better than the current model for the AIC stepping to continue.
           # Hook and Line survey binary models appear to need a value far greater than 2 AIC units to be parsimonies and not have
           # too many biologically dubious variables and interactions terms - this is not fully understood.
           if (!is.null(buffer) && bAIC >= AIC - buffer) { 
                break
           } else {
                if (bAIC >= AIC + 1e-07)  # Original code in MASS::stepAIC()
                   break
           }
                           
            nm <- nm + 1
            models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - edf, change = change, AIC = bAIC)
            if (!is.null(keep)) 
                keep.list[[nm]] <- keep(fit, bAIC)
        }
        if (!is.null(keep)) 
            fit$keep <- re.arrange(keep.list[seq(nm)])
        step.results(models = models[seq(nm)], fit.keep, object, usingCp)
    }
    
 # ======================================================================= END FUNCTIONS =============================================================================

 
    # source('DIC & BIC.R')
    # source('stepAIC.buffer.R')
    assign('stepAIC.buffer', stepAIC.buffer, pos = 1)
    
    if(!substring(version$platform, 8, 9) == "pc")
       options(width=180, digits=9)
   
   
    # Pick out wanted columns

  
    # --------------- BROKEN ------------------------
    
    #    if(VermComplex) {
    
    #      DATA <- Grand.2014[,c("NUMVERM", "NUMSUN", "NUMVERMC", "YEAR", "SITENAME", "VESNAME", "DROPNUM", "HOOKNUM", "ANGNUM",  "depth_meters", "FISHTIME", "wave_height_m", 
    #		"SSTCENT", "PCTLITE", "moon_phase_r", "TIDEPHAS", "TideFlow2", " WindSpeedDrop.k", "DriftSpeedDrop.k", "swell_height_m")]
    
    #      dimnames(DATA)[[2]] <- c("NumVerm", "NumSun", "NumVermc", "year", "site_number", "vessel", "drop_number", "hook_number", "angler_number",  "depth_meters", "FishTime", "wave_height_m", 
    #		"SSTDrop.C", "drop_time_proportion_of_solar_day", "moon_phase_r", "TidePhas", "TideFlow2", " WindSpeedDrop.k", "DriftSpeedDrop.k", "swell_height_m")
    
    #    } else {
    
    #      DATA <- Grand.2014[ ,c(toupper(Y.Name), "YEAR", "SITENAME", "VESNAME", "DROPNUM", "HOOKNUM", "ANGNUM",  "depth_meters", "FISHTIME", "wave_height_m", 
    #		"SSTCENT", "PCTLITE", "moon_phase_r", "TIDEPHAS", "TideFlow2", " WindSpeedDrop.k", "DriftSpeedDrop.k", "swell_height_m")]
    
    #      dimnames(DATA)[[2]] <- c(Y.Name, "year", "site_number", "vessel", "drop_number", "hook_number", "angler_number",  "depth_meters", "FishTime", "wave_height_m", 
    #		"SSTDrop.C", "drop_time_proportion_of_solar_day", "moon_phase_r", "TidePhas", "TideFlow2", " WindSpeedDrop.k", "DriftSpeedDrop.k", "swell_height_m")
    #    }
     

    # ================================ assign() DATA to .GlobalEnv" =====================================
    assign("DATA", DATA, pos = 1)

   
    


    if(is.null(GLM.FINAL.AIC)) {
     
     if(is.null(STEP.AIC)) { 
    
        if(is.null(MAIN.STEP.AIC)) {   
    
           # ------------------- Find poly level for each continuous variable --------------------------------
    
           catf("\nFind poly level for each continuous variable\n\n")
    
          # Full formula includes SSTDrop.C, TideFlow2, TideHt, and DriftSpeedDrop.k 
          if(reducedFormula) {
          
             assign('BASE.FORMULA', paste(Y.Name, "~ year + site_number + CrewStaff + drop_number + hook_number"), pos = 1)
          
             if(Include.FishTime) {
               
               CONT.FORMULA <- paste(Test.poly.level('FishTime', tolAIC = 0.5), Test.poly.level('drop_time_proportion_of_solar_day', tolAIC = 0.5), 
                                     Test.poly.level('swell_height_m', tolAIC = 0.5), Test.poly.level('wave_height_m', tolAIC = 0.5), 
                                     Test.poly.level('moon_percent_fullness_r', tolAIC = 0.5))
             } else {
             
               CONT.FORMULA <- paste(Test.poly.level('drop_time_proportion_of_solar_day', tolAIC = 0.5), Test.poly.level('swell_height_m', tolAIC = 0.5), 
                                     Test.poly.level('wave_height_m', tolAIC = 0.5), Test.poly.level('moon_percent_fullness_r', tolAIC = 0.5))
             }
             
          } else {
            
             assign('BASE.FORMULA', paste(Y.Name, "~ year + site_number + CrewStaff + drop_number + hook_number + poly(DriftSpeedDrop.k,3)"), pos = 1)
            
             if(Include.FishTime) {
             
               CONT.FORMULA <- paste(Test.poly.level('FishTime', tolAIC = 0.5), Test.poly.level('SSTDrop.C'), Test.poly.level('drop_time_proportion_of_solar_day'), 
                                     Test.poly.level('TideFlow2'), Test.poly.level('swell_height_m'), Test.poly.level('TideHt'), 
                                     Test.poly.level('wave_height_m'), Test.poly.level('moon_percent_fullness_r'))
             } else {
            
               CONT.FORMULA <- paste(Test.poly.level('SSTDrop.C'), Test.poly.level('drop_time_proportion_of_solar_day'), Test.poly.level('TideFlow2'), 
                                     Test.poly.level('swell_height_m'), Test.poly.level('TideHt'), Test.poly.level('wave_height_m'), 
                                     Test.poly.level('moon_percent_fullness_r'))
             }          
          }
          
    
          catf("\nContinuous variables added to model: ", CONT.FORMULA, "\n\n")
    
         # rm(BASE.FORMULA)
    
    
         # --------------------  stepAIC for main effects ------------------------------------------------------------------
    
         catf("\nstepAIC for main effects\n\n")
    
         # 2016
         # ADDITIONAL.FACTORS.FORMULA <- "+ TidePhase"
         # catf("\nAdditional factor variables added to model: ", ADDITIONAL.FACTORS.FORMULA, "\n\n")
         # In full formula: list(upper = ...  + poly(DriftSpeedDrop.k, 3)", ADDITIONAL.FACTORS.FORMULA, CONT.FORMULA))
    
         if(reducedFormula) {
    
            # No poly(DriftSpeedDrop.k, 3) nor additional factors
            MAIN.STEP.AIC <- stepAIC.buffer(glm(formula(BASE.FORMULA), family = binomial, data = DATA), 
               list(upper = formula(paste("~ year + site_number + CrewStaff + drop_number + hook_number", CONT.FORMULA)), 
                    lower = ~ year + site_number + CrewStaff + drop_number + hook_number), buffer = buffer[1], trace = 2)
         } else {
    
            MAIN.STEP.AIC <- stepAIC.buffer(glm(formula(BASE.FORMULA), family = binomial, data = DATA), 
               list(upper = formula(paste("~ year + site_number + CrewStaff + drop_number + hook_number + poly(DriftSpeedDrop.k, 3)", CONT.FORMULA)) , 
              lower = ~ year + site_number + CrewStaff + drop_number + hook_number + poly(DriftSpeedDrop.k, 3)), buffer = buffer[1], trace = 2)
        }
          
          assign(paste(substring(Y.Name, 4), ".MAIN.STEP.AIC", sep=""),  MAIN.STEP.AIC, pos = 1)
    
    
        } else 
    
             MAIN.STEP.AIC <- MAIN.STEP.AIC
          
    
      # -------------- Two-way interaction ----------------------------------------------------------------
    
      catf("\nstepAIC for interactions\n\n")  
    
      if(Interaction) {
    
        SA.FORM <- as.character(MAIN.STEP.AIC$formula)[3]
        printf(SA.FORM)
    
        STEP.AIC <- stepAIC.buffer(MAIN.STEP.AIC, list(upper = formula(paste("~", SA.FORM, "+ (", substring(SA.FORM, 31), ")^2")), lower = formula(paste("~", SA.FORM))), buffer = buffer[2], trace = 2)
    
        assign(paste(substring(Y.Name,4), ".STEP.AIC", sep=""), STEP.AIC, pos = 1)
    
       } 
    
     } else
    
       STEP.AIC <- STEP.AIC
     
    # Convert short version of the model to the long version needed for MCMC:
    
      MM <- as.data.frame(model.matrix(STEP.AIC$formula, DATA))[,-1]
    # printf(MM[1:2,])
      dimnames(MM)[[2]] <- fix.poly.names(dimnames(MM)[[2]])
    # printf(MM[1:2,])
      FORMULA <- formula(paste(Y.Name, " ~ ", paste(dimnames(MM)[[2]], collapse= " + ")))
    # printf(FORMULA)
      MM[[Y.Name]] <- DATA[, Y.Name]
    # printf(MM[1:2,])
      GLM.FINAL.AIC  <- glm(FORMULA, family = binomial, data=MM)
      
      GLM.FINAL.NAME <- paste(substring(Y.Name, 4), ".Final.Glm", sep = "")
    
      assign(GLM.FINAL.NAME, GLM.FINAL.AIC, pos = 1)
      save(list = GLM.FINAL.NAME, file = paste(GLM.FINAL.NAME, ".RData", sep = ""))
      catf("\n", GLM.FINAL.NAME, "saved to .GlobalEnv and working directory\n\n")
     
      if(Stop.before.MCMC)
         stop("Stopped before MCMC")
    
     if(FALSE) {
       if(.Platform$OS.type == "windows")
          save(list = base::ls(1, all=TRUE), file = ".RData") 
       else
        save.image()
     }
    
    }  # Having a GLM.FINAL.AIC argument skips to here
    
    if(Stop.before.MCMC)
         stop("Stopped before MCMC")
    
     # ------------------------------- MCMC --------------------------------------
    
     printf(cbind(names(GLM.FINAL.AIC$coef), colnames(vcov(GLM.FINAL.AIC))))
    
    # stop()
    
      catf("\n\nStarting final model MCMC: ", as.character(Sys.time()), "\n\n") 
      catf(" Starting final model MCMC: ", as.character(Sys.time()), "\n\n", file = 'Run.txt')
    
      MCMC <- MCMClogit(GLM.FINAL.AIC, GLM.FINAL.AIC$data, tune = tune, mcmc = mcmc, burnin = burnin, thin = thin, verbose = verbose)
      assign("MCMC", MCMC, pos = 1)
      save(MCMC, file= paste(substring(Y.Name,4), ".MCMC.RData", sep = ""))
    
      AIC.V <- AIC(GLM.FINAL.AIC)
      DIC.V <- DIC(MCMC, GLM.FINAL.AIC)
      
      AIC.DIC <- data.frame(AIC = AIC.V, DIC = DIC.V)
     
     
      # ---- Index Results --------
    	
      FIT.DF <- as.data.frame(MCMC)
     
      EMM <- FIT.DF[,1] + as.matrix(FIT.DF[,-1]) %*% matrix(apply(GLM.FINAL.AIC$data[,-ncol(GLM.FINAL.AIC$data)], 2, median), ncol=1) # Added Y column (last column of the matrix) needs to taken away here.
    
      ##########################################################
      # Aggressor is now always the vessel whose crew staff is set to 
      # zero since Phillip Ebert's name is changed to 'AAAPhillipEbert'.
      ##########################################################
      
      FIT.DF$vesselAggresor <- apply(FIT.DF[, paste("CrewStaff", apply(matrix(sort(as.character(unique(DATA$CrewStaff[DATA$vessel %in% "Aggressor"]))), ncol = 1), 1, 
                                             function(x) { paste(get.subs(x, sep = " "), collapse = "") }), sep = "")[-1]], 1, mean) # The veseel with crewstaff set to zero needs the '[-1]' here
    
      FIT.DF$vesselMirage <- apply(FIT.DF[, paste("CrewStaff", apply(matrix(sort(as.character(unique(DATA$CrewStaff[DATA$vessel %in% "Mirage"]))), ncol = 1), 1, 
                                             function(x) { paste(get.subs(x, sep = " "), collapse = "") }), sep = "")], 1, mean)
    
      FIT.DF$vesselToronado <- apply(FIT.DF[, paste("CrewStaff", apply(matrix(sort(as.character(unique(DATA$CrewStaff[DATA$vessel %in% "Toronado"]))), ncol = 1), 1, 
                                             function(x) { paste(get.subs(x, sep = " "), collapse = "") }), sep = "")], 1, mean)
    
      FIT.DF$vesselAggresor <- FIT.DF$vesselAggresor - FIT.DF$vesselMirage
      FIT.DF$vesselToronado <- FIT.DF$vesselToronado - FIT.DF$vesselMirage 
      FIT.DF$vesselMirage <- NULL
    
      # Average of vessel effect
      if(contrast == "treatment") {
    
         Site.Col.Num <- grep("site_number", names(FIT.DF))
         N.Sites <- length(Site.Col.Num) + 1 # Including first site that is set to zero in the model
         N.years <- length(grep("year", names(FIT.DF))) + 1
         Index <- array(dim=c(nrow(EMM), N.years, N.Sites)) # nrow(EMM) = nrow(FIT.DF) = nrow(MCMC) = mcmc/thin; Usually looking for 1000 MCMC after thinning, but that is no longer hardwired here
         
         if(Area %in% c("Orig121", "ALL"))  {
         
            SC <- cbind(site_number101 = rep(0, nrow(EMM)), FIT.DF[, Site.Col.Num]) # SC = Site Columns
        
            yearLast <- 2003 + N.years # Inclusive years so for the math use one minus the start of the survey
            Iyear <- paste0("I", 2004:yearLast)  # c("I2004", "I2005", "I2006", "I2007", ...)
            
            for( i in 1:N.Sites) {
              
               Index[,1,i] <- (inv.logit(0 + SC[,i] + EMM) + inv.logit(0 + SC[,i] + EMM + FIT.DF$vesselAggresor))/2
            
               for (j in 2:N.years) {
              
                 yearCol <- paste0('year', j + 2003)
            
                 if(j < 10)
                   Index[,j,i] <- (inv.logit(FIT.DF[[yearCol]] + SC[,i] + EMM) + inv.logit(FIT.DF[[yearCol]] + SC[,i] + EMM + FIT.DF$vesselAggresor))/2
                 else
                   Index[,j,i] <- (inv.logit(FIT.DF[[yearCol]] + SC[,i] + EMM) + inv.logit(FIT.DF[[yearCol]] + SC[,i] + EMM + FIT.DF$vesselAggresor) + inv.logit(FIT.DF[[yearCol]] + SC[,i] + EMM + FIT.DF$vesselToronado))/3
              }
            }
            
            FINAL.GLM.COEF <- data.frame(rbind(year2004 = c(0,0), summary(GLM.FINAL.AIC)$coef[2:(yearLast - 2003), 1:2]))
         }
         
         if(Area == "CCA")  {
         
            SC <- cbind(site_number501 = rep(0, nrow(EMM)), FIT.DF[, Site.Col.Num]) 
         
            yearLast <- 2013 + N.years # Inclusive years so for the math use one minus the start of the survey
            Iyear <- paste0("I", 2014:yearLast)  # c("I2004", "I2005", "I2006", "I2007", ...)
            
            for( i in 1:N.Sites) {
              
               Index[,1,i] <- (inv.logit(0 + SC[,i] + EMM) + inv.logit(0 + SC[,i] + EMM + FIT.DF$vesselAggresor))/2
            
               for (j in 2:N.years) {
              
                 yearCol <- paste0('year', j + 2013)
                 Index[,j,i] <- (inv.logit(FIT.DF[[yearCol]] + SC[,i] + EMM) + inv.logit(FIT.DF[[yearCol]] + SC[,i] + EMM + FIT.DF$vesselAggresor) + inv.logit(FIT.DF[[yearCol]] + SC[,i] + EMM + FIT.DF$vesselToronado))/3
              }
            }
            
            FINAL.GLM.COEF <- data.frame(rbind(year2014 = c(0,0), summary(GLM.FINAL.AIC)$coef[2:(yearLast - 2013), 1:2]))
         }
         
         Index.Over.Sites <- apply(Index, 1:2, mean)
         colnames(Index.Over.Sites) <- Iyear
         FIT.DF <- cbind(FIT.DF, Index.Over.Sites)
    
      } else {
      
        stop("Only treatment contrasts are currently supported") # *******************************************************
      }
    
      assign(paste(substring(Y.Name,4), ".FIT.DF.MCMC.", round(mcmc/1e6,1), "M", ".Burn.", round(burnin/1e6,1), "M", sep=""), FIT.DF, pos = 1)
    
      if(.Platform$OS.type == "windows") {
           dev.new()
           plot(mcmc(FIT.DF[, Iyear]), ask = FALSE)
      }
      
      dev.new()
      autocorr.plot(mcmc(FIT.DF[, Iyear]), ask = FALSE)
        
     
      Q.MCMC <- apply(FIT.DF[, Iyear], 2, quantile, probs = c(0.50, 0.025, 0.975), type = 8)
      
      names(FINAL.GLM.COEF)[[2]] <- "Std.Error"  # This is for a bug in summary.glm() function; it gives "Std. Error"
    
     
      SS.Table <- round(rbind(Median = apply(FIT.DF[, Iyear], 2, median), SD.log = apply(FIT.DF[, Iyear], 2, 
              function(x) sd(log(x))), t(FINAL.GLM.COEF)), 4)
      
      FinalName <- paste0(substring(Y.Name, 4), ".Final.Model.MCMC.", yearLast) 
    
      Final.Formula.empty.env <- GLM.FINAL.AIC$formula
      environment(Final.Formula.empty.env) <- emptyenv()
      
      Out <- list(Final.Model = Final.Formula.empty.env, AIC.DIC = AIC.DIC, Final.glm.coef = FINAL.GLM.COEF, Q.MCMC = Q.MCMC, SS.Table = SS.Table, MCMC = MCMC, DATA = DATA)
      assign(FinalName, Out, pos = 1)
    
      save(list = FinalName, file = paste(FinalName, ".RData", sep=""))  
    
      if(.Platform$OS.type == "windows")
         save(list = base::ls(1, all = TRUE), file = ".RData") 
      else
         save.image()
       
    
    
    # ------------------ DIC Check --------------------------------------
    
    if(FALSE) {
    
      save(AIC.DIC, file = "AIC.DIC.0.RData")
    
      for( i in 1:DIC.Check ) {
        
        catf("\n\nStarting DIC check: ", i, as.character(Sys.time()), "\n\n")
        cat(" Starting DIC check: ", i, as.character(Sys.time()), "\n\n", file = 'Run.txt', append=T)
    
        # MM <- as.data.frame(model.matrix(reduce.formula(STEP.AIC$formula, i), DATA))[,-1]
        # dimnames(MM)[[2]] <- fix.poly.names(dimnames(MM)[[2]])
    
        # FORMULA <- formula(paste(Y.Name, " ~ ", paste(dimnames(MM)[[2]], collapse= " + ")))
    
        # MM[[Y.Name]] <- DATA[, Y.Name]
    
        # GLM.FINAL.AIC.B <- glm(FORMULA, family = binomial, data=MM)
    
        MCMC.B <- MCMClogit(GLM.FINAL.AIC, GLM.FINAL.AIC$data, tune = tune, mcmc=mcmc, burnin=burnin, thin=thin, verbose= verbose)
    
        AIC.V <- c(AIC.V, AIC(GLM.FINAL.AIC))
        DIC.V <- c(DIC.V, DIC(MCMC.B, GLM.FINAL.AIC))
       
        AIC.DIC = data.frame(AIC=AIC.V, DIC=DIC.V)
        save(AIC.DIC, file=paste("AIC.DIC.", i, ".RData", sep=""))
      }
   }
    
    #  Return final results
    
    # Change output after 2016 to MCMC = MCMC - I went with MCMC and dropped FIT.DF
    # In 2016 FIT.DF = FIT.DF for Bocaccio and MCMC = MCMC for Lingcod
    # list(Final.Model = GLM.FINAL.AIC$formula, AIC.DIC = AIC.DIC, Final.glm.coef = FINAL.GLM.COEF, Q.MCMC = Q.MCMC, SS.Table = SS.Table, FIT.DF = FIT.DF)
    
    invisible(Out)
}

