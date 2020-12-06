
processData <- function(Y.Name = "NumBoc", common_name = "Bocaccio", Grand = Grand_2019, reducedFormula = FALSE, Include.FishTime = FALSE, Restrict.6min = FALSE, Sites = NULL, 
                          Area = c("Orig121", "CCA", "ALL")[1], propHookCutOffAggr = if(Area == "ALL") 0.002 else 0.003, propHookCutOffMirage = propHookCutOffAggr, 
                          propHookCutOffToro = 3 * propHookCutOffAggr) {

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
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Table.R")
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/refactor.R")
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/recode.simple.R")
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/r.R")
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R")
       
   if(Restrict.6min) 
        Grand <- Grand[Grand$FishTime <= 360, ]

   if(!is.null(Sites))
         Grand.TMP <- Grand[Grand$site_number %in% Sites, ]  # Restrict to sites given in 'Sites'
   else
        Grand.TMP <- Grand 

   Grand.TMP[Y.Name] <- as.numeric(Grand.TMP$common_name %in% common_name) 
   
   Grand.TMP$CrewStaff <- as.character(apply(matrix(paste(Grand.TMP$angler_first_name, Grand.TMP$angler_last_name), ncol=1)[,,drop=F], 1, function(x) paste(get.subs(x, sep=" "), collapse="")))
   
   # Change Adrian Chavez's name to 'AAAdrian Chavez', so that the Aggressor also has the crew name that gets set to zero
   Grand.TMP$CrewStaff[Grand.TMP$CrewStaff %in% "AdrianChavez"] <- "AAAdrianChavez"  

   # printf(Table(Grand.TMP$CrewStaff); catf("\n\n")

   list.tmp <- list(Grand.TMP[,Y.Name])
   mean.Y.Name <- paste("mean", Y.Name, sep=".")
   names(list.tmp) <- mean.Y.Name
   catf("\n"); printf(r(cbind(aggregate(list.tmp, list(CrewStaff = Grand.TMP$CrewStaff, vessel = Grand.TMP$vessel), mean, na.rm = TRUE), 
                      N = aggregate(list.tmp, list(Grand.TMP$CrewStaff, vessel = Grand.TMP$vessel), length)[,2])[,c("vessel", "CrewStaff", "N", mean.Y.Name)]), 2); catf("\n\n")

   Num.years.by.Site <- data.frame(site_number = dimnames(table(Grand.TMP$site_number, Grand.TMP$year))[[1]], 
	           Numyears=apply(matrix(as.numeric(as.logical(table(Grand.TMP$site_number, Grand.TMP$year))), ncol=ncol(table(Grand.TMP$site_number, Grand.TMP$year))),1,sum))
   Grand.TMP <- match.f(Grand.TMP, Num.years.by.Site, "site_number", "site_number", "Numyears")

   Number <- aggregate(list(NUM = Grand.TMP[[Y.Name[1]]]), list(site_number=Grand.TMP$site_number), sum)
  

  # Full formula includes SSTDrop.C, TideFlow2, TideHt, and DriftSpeedDrop.k - weight_kg and length_cm also added since they are now available
  if(reducedFormula) {
  
      if(Include.FishTime) {
          DATA <- Grand.TMP[Grand.TMP$site_number %in% Number[Number$NUM != 0, "site_number"] & Grand.TMP$Numyears >= 2, c(Y.Name, "year", "site_number", "vessel",
              "drop_number", "hook_number", "angler_number", "sex", "CrewStaff", "depth_meters", "FishTime", "swell_height_m", "wave_height_m", "moon_phase_r", "moon_percent_fullness_r", "drop_time_proportion_of_solar_day",
              "weight_kg", "length_cm")]
      } else {
      
          DATA <- Grand.TMP[Grand.TMP$site_number %in% Number[Number$NUM != 0, "site_number"] & Grand.TMP$Numyears >= 2, c(Y.Name, "year", "site_number", "vessel",
              "drop_number", "hook_number", "angler_number", "sex", "CrewStaff", "depth_meters", "swell_height_m", "wave_height_m", "moon_phase_r", "moon_percent_fullness_r", "drop_time_proportion_of_solar_day", "weight_kg", "length_cm")]
      }
      
  } else {

     if(Include.FishTime) {
          DATA <- Grand.TMP[Grand.TMP$site_number %in% Number[Number$NUM != 0, "site_number"] & Grand.TMP$Numyears >= 2, c(Y.Name, "year", "site_number", "vessel",
              "drop_number", "hook_number", "angler_number", "sex", "CrewStaff", "depth_meters", "FishTime", "swell_height_m", "wave_height_m", "moon_phase_r", "moon_percent_fullness_r", "drop_time_proportion_of_solar_day",
              "SSTDrop.C", "TideFlow2", "TideHt", "DriftSpeedDrop.k", "weight_kg", "length_cm")]
      } else {
      
          DATA <- Grand.TMP[Grand.TMP$site_number %in% Number[Number$NUM != 0, "site_number"] & Grand.TMP$Numyears >= 2, c(Y.Name, "year", "site_number", "vessel",
              "drop_number", "hook_number", "angler_number", "sex", "CrewStaff", "depth_meters", "swell_height_m", "wave_height_m", "moon_phase_r", "moon_percent_fullness_r", "drop_time_proportion_of_solar_day", 
              "SSTDrop.C", "TideFlow2", "TideHt", "DriftSpeedDrop.k", "weight_kg", "length_cm")]
     }
  }     
   
   DATA$hook_number <- as.character(DATA$hook_number)
   DATA$hook_number[DATA$hook_number %in% "1"] <- "1_Bottom"
   DATA$hook_number[DATA$hook_number %in% "5"] <- "5_Top"
   
   DATA$angler_number <- as.character(DATA$angler_number)
   DATA$angler_number[DATA$angler_number %in% "1"] <- "1_Bow"
   DATA$angler_number[DATA$angler_number %in% "2"] <- "2_Midship"
   DATA$angler_number[DATA$angler_number %in% "3"] <- "3_Stern"
   
   DATA$year <- factor(as.character(DATA$year))
   DATA$site_number <- factor(as.character(DATA$site_number))
   DATA$vessel <- factor(as.character(DATA$vessel))
   DATA$drop_number <- factor(as.character(DATA$drop_number))
   DATA$angler_number <- factor(as.character(DATA$angler_number))
   DATA$CrewStaff <- factor(as.character(DATA$CrewStaff))
   DATA$hook_number <- factor(as.character(DATA$hook_number))
   DATA$moon_phase_r <- factor(as.character(DATA$moon_phase_r))
   DATA$sex <- factor(as.character(DATA$sex))
   # DATA$TidePhase <- factor(as.character(DATA$TidePhase))
   
   
   # Create Minor Angler Groups
   if(Area == "Orig121") 
      DATA <- DATA[as.numeric(as.character(DATA$site_number)) < 500, ] 
      
   if(Area == "CCA") 
      DATA <- DATA[as.numeric(as.character(DATA$site_number)) > 500, ]   
   
   MeanCrewStaff <- aggregate(list(Mean = DATA[,Y.Name]), list(CrewStaff = DATA$CrewStaff), mean)
   
   if(length(as.character(MeanCrewStaff[MeanCrewStaff$Mean %in% 0, 'CrewStaff'])) != 0)
      catf("Crew that didn't catch any of this species:", as.character(MeanCrewStaff[MeanCrewStaff$Mean %in% 0, 'CrewStaff']), "\n\n")
      
   DATA <- refactor(DATA[!DATA$CrewStaff %in% MeanCrewStaff[MeanCrewStaff$Mean %in% 0, 'CrewStaff'], ])
   
  
   DATA$CrewStaff <- as.character(DATA$CrewStaff)
   CrewStaffOLD <- DATA$CrewStaff
  
   # Aggressor
   catf("\n\n--- Aggressor ---\n\nAll anglers:\n")
   printf(staffAggr <- sort(Table(DATA$CrewStaff[DATA$vessel %in% 'Aggressor'])))
   catf(paste0("\nProportion of hooks fished cutoff value = ",  propHookCutOffAggr, ", Total number of hooks = ", sum(staffAggr), ", Prop times total = ", propHookCutOffAggr  * sum(staffAggr), "\n\n"))
   DATA$CrewStaff[DATA$CrewStaff %in% c(names(staffAggr[staffAggr < propHookCutOffAggr * sum(staffAggr)]), "UnknownAnglerUnknownAngler", "UnknownAggressor", "") & DATA$vessel %in% "Aggressor"] <- "MinorAnglerAggressor"
   printf(Table(CrewStaffOLD[DATA$CrewStaff %in% "MinorAnglerAggressor"], DATA$CrewStaff[DATA$CrewStaff %in% "MinorAnglerAggressor"])); catf("\n")
   catf("\nMinor Angler group added:\n")
   printf(sort(Table(DATA$CrewStaff[DATA$vessel %in% "Aggressor"])))
    
   # Mirage
   catf("\n\n--- Mirage ---\n\nAll anglers:\n")
   printf(staffMirage <- sort(Table(DATA$CrewStaff[DATA$vessel %in% 'Mirage'])))
   catf(paste0("\nProportion of hooks fished cutoff value = ",  propHookCutOffMirage, ", Total number of hooks = ", sum(staffMirage), ", Prop times total = ", propHookCutOffMirage  * sum(staffMirage), "\n\n"))
   DATA$CrewStaff[DATA$CrewStaff %in% c(names(staffMirage[staffMirage < propHookCutOffMirage  * sum(staffMirage)]), "UnknownAnglerUnknownAngler", "UnknownMirage", "") & DATA$vessel %in% "Mirage"] <- "MinorAnglerMirage"
   printf(Table(CrewStaffOLD[DATA$CrewStaff %in% "MinorAnglerMirage"], DATA$CrewStaff[DATA$CrewStaff %in% "MinorAnglerMirage"])); catf("\n")
   catf("\nMinor Angler group added:\n")
   printf(sort(Table(DATA$CrewStaff[DATA$vessel %in% "Mirage"])))
   
   # Toronado
   catf("\n\n--- Toronado ---\n\nAll anglers:\n")
   printf(staffToro <- sort(Table(DATA$CrewStaff[DATA$vessel %in% 'Toronado'])))
   catf(paste0("\nProportion of hooks fished cutoff value = ",  propHookCutOffToro, ", Total number of hooks = ", sum(staffToro), ", Prop times total = ", propHookCutOffToro  * sum(staffToro), "\n\n"))
   DATA$CrewStaff[DATA$CrewStaff %in% c(names(staffToro[staffToro < propHookCutOffToro  * sum(staffToro)]), "UnknownAnglerUnknownAngler", "UnknownToronado", "") & DATA$vessel %in% "Toronado"] <- "MinorAnglerToronado"
   printf(Table(CrewStaffOLD[DATA$CrewStaff %in% "MinorAnglerToronado"], DATA$CrewStaff[DATA$CrewStaff %in% "MinorAnglerToronado"])); catf("\n")
   catf("\nMinor Angler group added:\n")
   printf(sort(Table(DATA$CrewStaff[DATA$vessel %in% "Toronado"])))

   
   DATA$CrewStaff <- factor(DATA$CrewStaff)
   
   DATA
     
  }

