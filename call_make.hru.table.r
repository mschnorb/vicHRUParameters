call_make.hru.table <- function(ply_file,
                                dem_file,
                                veg_file,
                                soil_file,
                                outdir=NULL,
                                cell_file = NULL,
                                root_file = NULL,
                                save=FALSE){

#####################################################################################################
#USAGE: call_make.hru.table.r [ARGUMENTS]

#DESCRIPTION: Write VICGL HRU table

#ARGUMENTS:
# ply_file  - filename of basin polygon shapefile
# dem_file  - filename of elevation raster file
# veg_file  - filename of landcover raster file
# soil_file - filename of VICGL soil cells polygon shapefile
# outdir    - output directory [optional; default is working directory]
# cell_file - filename of VICGL cell-to-basin map text file [optional]
# root_file - filename of rooting depth table as comma-delimited text file [optional] 
# save      - Save output to *.RData file

#DETAILS:
# Script uses function make.hru.table() to generate VICGL HRU table, which is then saved
# as comma-delimited text file. If the svae flag is set, the output will also include the
# combined hru, root_depth and cell_map tables saved to an *.RData file. 
# Script uses tryCatch() to print 'result', which will either be TRUE (if successful),
# or an error/warning (if not successful).
#####################################################################################################

  #Parse arguments
  library("terra")

  CELLMAP <- TRUE
  RDEPTH  <- TRUE
  if(is.null(outdir))  outdir <- getwd()
  if(is.null(cell_file)) {
    CELLMAP <- FALSE
    cell_map <- NULL
    warning("No argument for 'cell_file'. Cell mapping will not be included in *.Rdata output.")
  }
  if(is.null(root_file)) {
    RDEPTH <- FALSE
    root_depth <- NULL
    warning("No argument for 'root_file'. Rooting depth will not be included in *.RData output.")
  }

  #Load/source file(s)
  source("make.hru.table.R")

  #Load data
  rdem <- rast(dem_file)
  rveg <- rast(veg_file)
  bpoly <- vect(ply_file)
  spoly <- vect(soil_file)
  if(CELLMAP) cell_map <- read.table(cell_file, header=TRUE, stringsAsFactors=FALSE, sep=",")
  if(RDEPTH)  root_depth <- read.table(root_file, header=TRUE, stringsAsFactors=FALSE, sep=",")
  if(CELLMAP & RDEPTH) save <- TRUE

  #Parameters
  rlf=200

  #Any basin polygon pre-processing code goes here
  bsub <- bpoly
  #bsub <- subset(bpoly, bpoly$BASIN==57)

  #Construct HRU table
  result <- tryCatch({
    hru_table <- make.hru.table(rdem, rveg, spoly, bsub, relief=rlf)
    write.table(hru_table, file=file.path(outdir,"hru_data.txt"), row.names=FALSE, sep=",", quote=FALSE)
    if(save) save(hru_table, cell_map, root_depth, file=file.path(outdir,"hru_data.RData"))
    rslt <- TRUE
  }, warning = function(war){
    return(paste("make.hru.table_WARNING: ", war))
  }, error = function(err){
    return(paste("make.hru.table_ERROR: ", err))
  }, finally = {
    #do nothing
  }) #End tryCatch

  return(result)
}