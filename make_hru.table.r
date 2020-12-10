make_hru.table <- function(rdem,
                           rcvr,
                           spoly,
                           bpoly,
                           relief=200,
                           cvr.ref=TRUE){

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #DESCRIPTION: Create HRU table for VICGL parameterization

  #ARGUMENTS:
  #rdem     - elevation RasterLayer object
  #rcvr     - landcover RasterLayer object
  #spoly    - VICGL computational grid as SimpleFeature object; attribute must have CELLID field
  #bpoly    - basin outline as SimpleFeature object
  #relief   - band relief in metres (default is 200)
  #cvr.ref  - if TRUE use rcvr as reference raster, else use rdem

  #VALUE: Data frame object with fields CELL_ID, BAND_ID, CLASS, AREA, AREA_FRAC and ELEVATION
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #Load libraries
  library('plyr')
  library('raster')
  library('rgdal')
  library('sf')

  #Internal function to match class pairs to corresponding
  #entry in classification table
  row.from.class_table <- function(r1,r2,ctbl){
    a <- cbind(values(r1),values(r2))
    apply(a,1,function(x,y){
      if(any(is.na(x))){
        return(NA)
      } else {
        m1 <- which(y[[1]]==x[1])
        m2 <- which(y[[2]]==x[2])
        m1[which(match(m1,m2,nomatch=-99) > 0)]
      }},
      y = ctbl)
  }
  
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... pre-processing input data \n")
  
  #Spatially subset/clip soil polygons
  #Use dplyr::filter function, but without loading dplyr package, as
  #loading dplyr masks 'extract' (from raster) and 'summarise' (from plyr)
  ssub <- dplyr::filter(spoly, st_contains(bpoly, spoly, sparse=FALSE))
  #Convert to SpatialPolygon* objects
  bsub_sp <- as_Spatial(bpoly)
  ssub_sp <- as_Spatial(ssub)

  #Crop raster data to extent of sub-basin polygon. Ensure that CRS
  #of the polygon matches that of each raster prior to cropping
  subpoly_pr1 <- spTransform(bsub_sp, crs(rdem))
  rdem_c <- crop(rdem,subpoly_pr1)
  subpoly_pr2 <- spTransform(bsub_sp, crs(rcvr))
  rcvr_c <- crop(rcvr,subpoly_pr2)

  #Align/reproject data sets to common grid/CRS
  if(cvr.ref){
    rref <- rcvr_c
    rdem_p <- projectRaster(rdem_c, rref)
    rcvr_p <- rcvr_c
  } else {
    rref <- rdem_c
    rcvr_p <- projectRaster(rcvr_c, rref)
    rdem_p <- rdem_c
  }
  soil_poly  <- spTransform(ssub_sp, crs(rref))
  #Create soil raster from polygon. This creates a raster with a raster
  #attribute table with an ID field. The 'value' of the raster is the ID
  #field - other values must be accessed through the RAT
  rsoil <- rasterize(soil_poly,rref)
  
  #Re-classify elevation bands
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... classifying elevation bands \n")
  zmin <- 0
  zmax <- round_any(cellStats(rdem_p, max),relief)+relief
  zlwr <- seq(zmin,zmax-relief,relief)
  zupr <- seq(relief,zmax,relief)
  zmid <- (zlwr+zupr)/2
  rdem_rcl <- reclassify(rdem_p, cbind(zlwr,zupr,zmid))

  #Create band-class-area table - calculate area of each HRU grouped by VICGL cell ...
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... creating band-class table \n")
  # ... create table of all possible combinations of BAND-CLASS, remove rows with
  # NA values, order table, add column names, and add ID field. The resulting
  # id_table has fields ID, BAND and CLASS.
  band_tmp <- expand.grid(unique(values(rdem_rcl)),unique(values(rcvr_p)))
  band_tmp <- band_tmp[-which(is.na(band_tmp$Var1) | is.na(band_tmp$Var2)),]
  id_table <- band_tmp[order(band_tmp$Var1,band_tmp$Var2),]
  names(id_table) <- c("BAND","CLASS")
  id_table$ID <- seq(1:length(id_table$BAND))
  # ... for all matching pixels in rdem_rcl and rcvr_p, find corresponding row
  # from the id_table - the row number becomes the hru ID. Convert to raster and
  # find intersecting VICGL grid cells from soil_poly
  hru <- row.from.class_table(rdem_rcl,rcvr_p,id_table)
  rhru <- raster(as.matrix(hru), template=rdem_rcl)
  tmp_list  <- raster::extract(rhru, soil_poly)
  # ...dissolve all identical band-class pairs by grid cell and calculate area. The
  # process is 1) count the occurrence of unique hru IDs in each overlapping cell,
  # 2) merge the resulting dataframe object to the id_table by CELLID, 3) get the
  # total number of pixels in each overlapping cell, 4) merge resulting dataframe
  # object to previously merged dataframe by CELLID, 5) calculate total area in each
  # cell, and then 6) calculate the area and cell area fraction of each hru - result
  # is the band-class table with fields ID, CELLID, BAND, CLASS, COUNT, TOT, CAREA,
  # AREAF, and AREA.
  hru_frame <- 
    do.call(rbind,
      lapply(1:length(tmp_list), function(x,y,z){
        rslt <- as.data.frame(count(y[[x]]))
        names(rslt) <- c("ID", "COUNT")
        rslt$CELLID <- z$CELLID[x]
        return(rslt)},
        y=tmp_list, z=soil_poly@data))
  merge1 <- merge(hru_frame,id_table,by="ID")
  area <- ddply(merge1,.(CELLID), summarise, TOT=sum(COUNT))
  merge2 <- merge(merge1, area, by="CELLID")
  merge2$CAREA <- merge2$TOT * res(rref)[1]^2
  merge2$AREAF <- merge2$COUNT/merge2$TOT
  merge2$AREA  <- merge2$AREAF * merge2$CAREA
  band_class_table <- merge2[order(merge2$CELLID,merge2$BAND,merge2$CLASS),]

  #Create band-cell-elevation table - calculate elevation of each band grouped by VICGL
  # cell ...
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... creating band-cell table \n")
  # ... create table of all possible combinations of BAND-CELLID, remove rows with
  # NA values, order table, add column names, and add CID field. The resulting
  # id_table2 has fileds ID, CID, and BAND
  cell_tmp <- expand.grid(unique(values(rdem_rcl)),unique(values(rsoil)))
  cell_tmp <- cell_tmp[-which(is.na(cell_tmp$Var1) | is.na(cell_tmp$Var2)),]
  id_table2 <- cell_tmp[order(cell_tmp$Var2,cell_tmp$Var1),]
  names(id_table2) <- c("BAND","CID")
  id_table2$ID <- seq(1:length(id_table2$BAND))
  # ... for all matching pixels in rdem_rcl and rsoil, find corresponding row
  # from the id_table2 - the row number becomes the ID. Convert to raster and
  # calculate the zonal median elevation for each band-cell zone. Merge resulting
  # dataframe with id_table2 based on ID field - result is band-cell table with
  # fields ID, CID, BAND and ELEV.
  band <- row.from.class_table(rdem_rcl,rsoil,id_table2)
  rband <- raster(as.matrix(band), template=rdem_rcl)
  zelev <- as.data.frame(zonal(rdem_p, rband, fun='median'))
  names(zelev) <- c("ID","ELEV")
  band_cell_table <- merge(zelev,id_table2, by="ID")

  #Create soil table
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... creating soil table \n")
  # ... create table with field CID; one entry per VICGL grid cell
  soil_table <- rsoil@data@attributes[[1]]
  names(soil_table)[which(names(soil_table)=="ID")] <- "CID"

  # Merge to create HRU table
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ... creating HRU table \n")
  # ... add CELLIDs to band-cell table by merging with the soil table on the CELLID
  # field, merge with band-class table, filter for relevant fields and return the
  # resulting hru table with fields CELL_ID, BAND_ID, CLASS, AREA, AREA_FRAC, and
  # ELEVATION.
  merge3 <- merge(band_cell_table,soil_table,by="CID")
  merge4 <- merge(band_class_table, merge3, by=c("CELLID","BAND"))
  hru_table <- merge4[match(c("CELLID","BAND","CLASS","AREA", "AREAF","ELEV"), names(merge4))]
  names(hru_table) <- c("CELL_ID", "BAND_ID", "CLASS", "AREA", "AREA_FRAC", "ELEVATION")
  return(hru_table[order(hru_table$CELL_ID,hru_table$BAND_ID,hru_table$CLASS),])
}
