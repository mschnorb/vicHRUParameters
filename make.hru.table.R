make.hru.table <- function(rdem,
                           rcvr,
                           spoly,
                           bpoly,
                           relief=200,
                           cvr.ref=TRUE){

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #DESCRIPTION: Create HRU table for VICGL parameterization

  #ARGUMENTS:
  #rdem     - elevation SpatRaster object
  #rcvr     - landcover SpatRaster object
  #spoly    - VICGL computational grid as SpatVector object; attribute must have CELLID field
  #bpoly    - basin outline as SpatVector object
  #relief   - band relief in metres (default is 200)
  #cvr.ref  - if TRUE use rcvr as reference raster, else use rdem

  #VALUE: Data frame object with fields CELL_ID, BAND_ID, CLASS, AREA, AREA_FRAC and ELEVATION
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #Load libraries
  library(terra)
  library(tidyverse)

  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... pre-processing input data \n")
  
  #Spatially subset/clip soil polygons
  ssub <- terra::intersect(bpoly, spoly)
  
  #Crop raster data to extent of sub-basin polygon. Ensure that CRS
  # of the polygon matches that of each raster prior to cropping
  ssub_pr1 <- project(ssub, crs(rdem))
  rdem_c <- crop(rdem, ssub_pr1)
  ssub_pr2 <- project(ssub, crs(rcvr))
  rcvr_c <- crop(rcvr, ssub_pr2)
  
  #Align/reproject data sets to common grid/CRS
  if(cvr.ref){
    rref <- rcvr_c
    rdem_p <- project(rdem_c, rref)
    rcvr_p <- rcvr_c
  } else {
    rref <- rdem_c
    rcvr_p <- project(rcvr_c, rref)
    rdem_p <- rdem_c
  }
  soil_poly <- project(ssub, crs(rref))
  rsln <- res(rref)[1]
  
  #Create soil raster from polygon. This creates a raster with a
  # value taken from the CELLID field
  rsoil <- rasterize(soil_poly, rref, field="CELLID")
  
  #Re-classify elevation bands
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... classifying elevation bands \n")
  mmx <- minmax(rdem_p)
  minr <- plyr::round_any(mmx[1], relief, f = floor)
  maxr <- plyr::round_any(mmx[2], relief, f = ceiling)
  rclmat <- matrix(c(seq(minr, maxr-relief, relief), seq(minr+relief, maxr, relief), seq(minr+relief/2, maxr-relief/2, relief)), ncol=3)
  rdem_rcl <- classify(rdem_p, rclmat, right=FALSE)
  
  #Create HRU table
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... creating table from matched pixels \n")
  # ... collect raster data for matching pixels; filter out NA or NaN pixels
  id_table <- tibble(CELL=as.vector(values(rsoil)),
                     BAND=as.vector(values(rdem_rcl)),
                     CLASS=as.vector(values(rcvr_p)),
                     ELEV=as.vector(values(rdem_p)))
  id_table <- id_table |> filter(!is.na(CELL)) |> filter(!is.na(BAND)) |> filter(!is.na(CLASS))
  # ... calculate area for each HRU (CELL-BAND-CLASS)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... creating HRU-AREA table \n")
  hru_area_table <- id_table |> group_by(CELL, BAND, CLASS) |> summarise(AREA=n()*rsln*rsln)
  # ... calculate median elevation for each band (CELL-BAND)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... creating BAND-ELEV table \n")
  band_elev_table <- id_table |> group_by(CELL,BAND) |> summarise(ELEV=median(ELEV))
  # ... calculate area of each VICGL cell (CELL)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... creating CELL-AREA table \n")
  cell_area_table <- id_table |> group_by(CELL) |> summarise(AREA=n()*rsln*rsln)
  # ... and recursively join tables together
  work_table <- left_join(left_join(hru_area_table, band_elev_table, by = c("CELL","BAND")), cell_area_table, by = c("CELL"))
  # Create HRU table
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... creating HRU table \n")
  hru_table <- tibble(CELL_ID   = work_table$CELL,
                      BAND_ID   = work_table$BAND,
                      CLASS     = work_table$CLASS,
                      AREA      = work_table$AREA.x,
                      AREA_FRAC = work_table$AREA.x/work_table$AREA.y,
                      ELEVATION = work_table$ELEV,
                      POLY_ID   = paste(work_table$CELL, work_table$BAND, sep="_")) |>
    arrange(CELL_ID, BAND_ID, CLASS)
  
  return(hru_table)
}
