
importData <- function(grandPathCSV = NULL, PST.For.All.years = FALSE) {

      # For dates in R see "R News 4/1 -- Help Desk" which starts on page 29 here: https://cran.r-project.org/doc/Rnews/Rnews_2004-1.pdf
      # There is a nice summary table on page 32.
      
      #   -------- Import utility Functions --------
     sourceFunctionURL <- function(URL) {
        ' # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() '
        require(RCurl)
        File.ASCII <- tempfile()
        on.exit(file.remove(File.ASCII))
        writeLines(paste(readLines(textConnection(RCurl::getURL(URL))), collapse = "\n"), File.ASCII)
        source(File.ASCII, local = parent.env(environment()))
     }
     
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/recode.simple.R")
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/Imap/master/R/sunRiseSet.R")
     # sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/PacFIN-Data-Extraction/master/R/nameConvertVdrfdToCompFT.R")

     if(is.null(grandPathCSV))
        # read from Data Warehouse
        stop("Data is not on Warehouse yet - please supply a drive file location")
     else    
        Grand <- utils::read.csv(grandPathCSV)
     
     Grand$longitude_in_degrees <- - Grand$longitude_in_degrees
          
     # POSIXct is recommended for date columns in data frames
     Grand$date_yyyymmdd <- as.POSIXct(as.character(Grand$date_yyyymmdd), format = "%Y%m%d")  # "%Y/%m/%d %H:%M"
          
     # Using POSIXlt below so don't need to deal with GMT to get the decimal hours into the local day correct - the result is numeric
     Grand$drop_time_24_hr <- hours(as.POSIXlt(Grand$drop_time_24_hr, format = "%m/%d/%Y %H:%M:%OS")) + minutes(as.POSIXlt(Grand$drop_time_24_hr, format = "%m/%d/%Y %H:%M:%OS"))/60
     Grand$sunrise_time_r <- hours(as.POSIXlt(Grand$sunrise_time_r, format = "%m/%d/%Y %H:%M:%OS")) + minutes(as.POSIXlt(Grand$sunrise_time_r, format = "%m/%d/%Y %H:%M:%OS"))/60
     Grand$sunset_time_r <- hours(as.POSIXlt(Grand$sunset_time_r, format = "%m/%d/%Y %H:%M:%OS")) + minutes(as.POSIXlt(Grand$sunset_time_r, format = "%m/%d/%Y %H:%M:%OS"))/60  
     
     if(PST.For.All.years) {
     
        Grand$drop_time_24_hr <- Grand$drop_time_24_hr - 1
        PST.Rise.Set <- sunRiseSet(data.frame(date_yyyymmdd = Grand$date_yyyymmdd, Lat = Grand$latitude_in_degrees, Long = Grand$longitude_in_degrees), timezone = "ETC/GMT+8")
        Grand$sunrise_time_r <- PST.Rise.Set$sunRise.hours
        Grand$sunset_time_r <- PST.Rise.Set$sunSet.hours
        
     } 
     
     sexTable <- scanIn("
     
          Old   New
           f     F
           m     M
           u     U
           ''    U
     ")
     
     Grand$sex <- recode.simple(Grand$sex, sexTable)
     
     # # Re-doing all the sunrise and sunset times, straight to decimal hours, could be done with the following, which has a long run time - 2004 would be on PST and 2003 onward would be on PDT
     # PST.Rise.Set <- sunRiseSet(data.frame(date_yyyymmdd = Grand$date_yyyymmdd, Lat = Grand$latitude_in_degrees, Long = Grand$longitude_in_degrees), timezone = "America/Los_Angeles")
     # Grand$sunrise_time_r <- PST.Rise.Set$sunRise.hours
     # Grand$sunset_time_r <- PST.Rise.Set$sunSet.hours
     
    Grand

}

