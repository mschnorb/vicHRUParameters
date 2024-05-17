call_make.VIC.param <- function(hrudf,         # HRU data frame object
                                rootdf,        # Rooting depth data frame object
                                vpfile,        # Name of vegetation parameter file
                                sbfile,        # Name of elevation band file                          
                                basin = NULL,  # sub basin short name identifier
                                celldf = NULL, # Cell map data frame object
                                glacid = 22,   # ID of glacier land cover class
                                maxz = 20,     # Maximum number of elevation bands in band file
                                minb = 100,    # BAND_ID of lowest band (i.e. that which includes sea level)
                                nullg = FALSE, # If TRUE, add NULL glaciers to vegetation parameter file and extra bottom band to band file
                                save = FALSE){ # Save function output to *.RData file

  #####################################################################################################
  #USAGE: call_make.VIC.param.r [ARGUMENTS]

  #DESCRIPTION: Write VICGL vegetation and band parameters to file

  #ARGUMENTS:
  # hrudf -   HRU data frame object [required]
  # rootdf -  Rooting depth data frame object [required]
  # vpfile -  Name of vegetation parameter file [required]
  # sbfile -  Name of elevation band file [required]
  # basin -   Sub-basin name [default = NULL]
  # celldf -  Cell map data frame object [default = NULL]
  # glacid -  ID of glacier land cover class [default = 22]
  # maxz -    Maximum number of elevation bands in band file [default = 20]
  # minb -    BAND_ID of lowest band (i.e. that which includes sea level) [default = 100]
  # nullg -   If TRUE, add NULL glaciers to vegetation parameter file and extra bottom band to band file [default = FALSE]
  # save -    Save function output to *.RData file [default = FALSE]

  #DETAILS:
  # Script uses side-effect of function make.VIC.param() to generate VICGL parameters and write the
  # vegetation and band parameter files. The -S or --save flags can also be used to save the return
  # value of the function make.VIC.param() to an *.RData file. See the documentation for make.VIC.param()
  # for a detailed description of the function return value. Script uses tryCatch() to print 'result',
  # which will either be TRUE (if successful), or an error/warning (if not successful).
  #####################################################################################################

  ## Load dependencies
  require("tidyverse")
  
  #Check optional arguments
  if(!is.null(basin)){
    if(is.null(celldf)) stop("Must specifiy cell map data frame object if providing sub-basin name.")
  }
  if(nullg & is.null(minb)) stop("Must specifiy minimum band ID if nullg set.")

  #Load/source file(s)
  source("make.VIC.param.R")

  #Subset main data frame if required; be flexible with celldf column names
  if(!is.null(basin)){
    nind <- min(match(c("name","names","NAME","NAMES","basin","BASIN","basins","BASINS"), names(celldf)), na.rm = TRUE)
    if(!any(celldf[[nind]]==basin, na.rm = TRUE)){
      warning(paste("Sub-basin '", basin, "' could not be found in supplied data frame.", sep=""))
      return(paste("make_VIC_param_ERROR: Sub-basin '", basin, "' could not be found in supplied data frame.", sep=""))
    }
    names(celldf)[nind] <- "BASIN"   # Update name of Basin column
    cind <- min(match(c("cellid","cell_id","CELLID","CELL_ID"), names(celldf)), na.rm = TRUE)
    names(celldf)[cind] <- "CELL_ID" # Update name of Cell ID column
    inFrame <- left_join(hrudf, celldf, by="CELL_ID", relationship = "many-to-many") |>
      filter(BASIN==basin) |> select(CELL_ID,BAND_ID,CLASS,AREA,AREA_FRAC,ELEVATION)
  } else {
    inFrame <- hrudf
  }

  #Construct VICGL parameters
  result <- tryCatch({
    rslt <- make.VIC.param(inFrame, rootdf, vpf_filename=vpfile, snb_filename=sbfile,
                           null_glaciers=nullg, glacierID=glacid, max_bands=maxz, min_band_id=minb)
    if(save) save(rslt, file="param.RData")
    rslt <- TRUE
  }, warning = function(war){
    return(paste("make_VIC_param_WARNING: ", war))
  }, error = function(err){
    return(paste("make_VIC_param_ERROR: ", err))
  }, finally = {
    #do nothing
  }) #End tryCatch

  return(result)
}
