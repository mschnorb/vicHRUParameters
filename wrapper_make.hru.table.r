#!/usr/bin/env Rscript

#####################################################################################################
#USAGE: Rcsript [options] wrapper_make.hru.table.r [ARGUMENTS]

#DESCRIPTION: Write VICGL HRU table

#ARGUMENTS:
# -b, --bfile - filename of basin polygon shapefile [required]
# -c, --cfile - filename of VICGL cell-to-basin map text file [optional]
# -d, --dfile - filename of elevation raster file [required]
# -o, --outdir - output directory [optional; default is working directory]
# -r, --rfile - filename of rooting depth table as comma-delimited text file [optional]
# -s, --sfile - filename of VICGL soil cells polygon shapefile [required]
# -S, --save -  Save output to *.RData file [default = FALSE]
# -v, --vfile - filename of landcover raster file [required]
# -h, --help -  print help message

#DETAILS:
# Script uses function make.hru.table() to generate VICGL HRU table, which is then saved
# as comma-delimited text file. If --cfile and --rfile are supplied , and the -S flag is
# set, the output will also include the combined hru, root_depth and cell_map tables saved
# to an *.RData file. 
# Script uses tryCatch() to print 'result', which will either be TRUE (if successful),
# or an error/warning (if not successful).
#####################################################################################################

#Parse arguments
library('optparse')
library('terra')

#Parse arguments
CELLMAP <- TRUE
RDEPTH  <- TRUE
option_list <- list(
  make_option(c("-b", "--bfile"), action="store", type="character", help="Filename of basin polygon shapefile [required]"),
  make_option(c("-c", "--cfile"), action="store", type="character", help="Filename of VICGL cell-to-basin map text file with 
    fields CELL_ID and NAME [optional]"),
  make_option(c("-d", "--dfile"), action="store", type="character", help="Filename of elevation raster file [required]"),
  make_option(c("-o", "--outdir"), action="store", type="character", help="Output directory [default is working directory]"),
  make_option(c("-r", "--rfile"), action="store", type="character", help="Filename of CSV text file with vegetation rooting
    depths with fields CLASS, RTHICK1, RTHICK2, RTHICK3, RFRAC1, RFRAC2 and RFRAC3 [optional]"),
  make_option(c("-s", "--sfile"), action="store", type="character", help="filename of VICGL soil cells polygon shapefile [required]"),
  make_option(c("-S", "--save"),  action="store_true", default=FALSE, help="Save results to *.RData file"),
  make_option(c("-v", "--vfile"), action="store", type="character", help="Filename of landcover raster file [required]")
)
opt <- parse_args(OptionParser(option_list=option_list))
if(is.null(opt$bfile))   stop("Missing argument for 'bfile'. Use -h or --help flag for usage.")
if(is.null(opt$dfile))   stop("Missing argument for 'dfile'. Use -h or --help flag for usage.")
if(is.null(opt$vfile))   stop("Missing argument for 'vfile'. Use -h or --help flag for usage.")
if(is.null(opt$sfile))   stop("Missing argument for 'sfile'. Use -h or --help flag for usage.")
if(is.null(opt$outdir))  opt$outdir <- getwd()
if(is.null(opt$cfile)) {
  CELLMAP <- FALSE
  cell_map <- NULL
  warning("No argument for 'cfile'. Cell mapping will not be included in *.Rdata output.")
}
if(is.null(opt$rfile)) {
  RDEPTH <- FALSE
  root_depth <- NULL
  warning("No argument for 'rfile'. Rooting depth will not be included in *.RData output.")
}

#Load/source file(s)
source("make.hru.table.R")

#Load data
rdem <- rast(opt$dfile)
rveg <- rast(opt$vfile)
bpoly <- vect(opt$bfile)
spoly <- vect(opt$sfile)
if(CELLMAP) cell_map <- read.table(opt$cfile, header=TRUE, stringsAsFactors=FALSE, sep=",")
if(RDEPTH)  root_depth <- read.table(opt$rfile, header=TRUE, stringsAsFactors=FALSE, sep=",")
if(CELLMAP & RDEPTH) opt$save <- TRUE

#Parameters
rlf=200

#Any basin polygon pre-processing code goes here
#bsub <- bpoly
bsub <- subset(bpoly, bpoly$BASIN=="DEANA")

#Construct HRU table
result <- tryCatch({
  hru_table <- make.hru.table(rdem, rveg, spoly, bsub, relief=rlf)
  write.table(hru_table, file=file.path(opt$outdir,"hru_data.txt"), row.names=FALSE, sep=",", quote=FALSE)
  if(opt$save) save(hru_table, cell_map, root_depth, file=file.path(opt$outdir,"hru_data.RData"))
  rslt <- TRUE
}, warning = function(war){
  return(paste("make.hru.table_WARNING: ", war))
}, error = function(err){
  return(paste("make.hru.table_ERROR: ", err))
}, finally = {
  #do nothing
}) #End tryCatch

#Print 'result' - potentially used by calling scripts to test for successful completion.
cat(result, "\n")
