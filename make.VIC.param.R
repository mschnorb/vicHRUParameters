make.VIC.param <- function(hru_df,
                           root_df,
                           null_glaciers=FALSE, 
                           glacierID=22, 
                           vpf_filename=NULL,
                           snb_filename=NULL,
                           max_bands=20,
                           min_band_id=100){
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #DESCRIPTION
  # Creates formatted output of vegetation and band parameters for VIC model version
  # VICGL; vegetation and band parameters based on the use of Hydrologic Response
  # Units (HRUs).
  
  #USAGE
  # make.VIC.param(hru_df, root_df, [null_glaciers=FALSE], [glacierID=22], 
  #     [vpf_filename="text_"], [snb_filename="text_"], [max_bands=n], [min_band_id=m])
  
  #ARGUMENTS:
  # hru_df        - HRU attribute table as data frame; must contain following fields: CELL_ID, 
  #                 BAND_ID, CLASS, AREA or AREA_FRAC, and ELEVATION
  # root_df       - Vegetation rooting parameters as data frame; must contain following fields:
  #                 CLASS, RTHICK1, RTHICK2, RTHICK3, RFRAC1, RFRAC2 and RFRAC3
  # null_glaciers - if TRUE, add NULL glaciers to elevation bands missing glacier HRUs; also add a
  #                 blank bottom elevation band for each cell in snb_filename
  # glacierID     - define vegetation class id for glacier cover; only required if null_glaciers=TRUE
  # vpf_filename  - Name of output vegetation parameter file (default is NULL)
  # snb_filename  - Name of output snowband file (default is NULL)
  # max_bands     - maximum number of bands for band file. Note that if null_glaciers = TRUE, then
  #                 max_bands should leave room for two extra bands (one below and one above the
  #                 existing bands)
  # min_band_id   - BAND_ID of lowest possible elevation band; used to identify cells where lowest
  #                 elevation band is at sea level
  #
  
  #DETAILS:
  # 
  #OUTPUT:
  # Function returns 6-element list of VIC cell metadata, vegetation and band parameters.
  # List elements are as follows:
  #   $NO_CELLS:  number of VIC cells in computational domain
  #   $CELL_ID:   vector of cell IDs for every cell in computational domain
  #   $NO_HRU:    vector giving number of HRUs per VIC cell
  #   $NO_BANDS:  vector giving number of bands per VIC cell
  #   $VPARAM:    List, where each item (one per cell) is a NO_HRUx9 size matrix of HRU
  #                 parameters, with one record per HRU
  #   $BAND:      List, where each item (one per cell) is a NO_BANDSx2 size matrix of
  #                 elevation band parameters, with one record per cell band
  #
  # If selected by user, function side-effect is to write default vegetation parameter file
  # and/or band parameter file to the current working directory. 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Load dependencies
  require("tidyverse")
  require("progress")
  
  ## Initialize variables
  out_list <- NULL     #Function return value as list output
  cell_vec <- NULL     #Track cell IDs as vector
  no_recs_vec <- NULL  #Track number of HRUs by cell as vector
  no_bands_vec <- NULL #Track number of bands by cell as vector
  hru <- NULL          #Dynamic matrix of HRU parameter records
  bnd <- NULL          #Dynamic matrix of band parameter records
  vparams <- NULL      #Matrix of HRU parameter records for a given cell
  bparams <- NULL      #Matrix of band parameter records for a given cell
  slevel <- NULL       #Track sea-level flag for each cell
  
  ## Pre-process hru data ###
  hru_df <- arrange(hru_df, CELL_ID, BAND_ID, CLASS)  #Ensure data frame is sorted
  cells <- unique(hru_df$CELL_ID) #vector of unique cell IDs
  no_cells <- length(cells)

  # summary by cell and elevation band
  band_df <- hru_df |> group_by(CELL_ID, BAND_ID) |>
    summarise(AREA_FRAC=sum(AREA_FRAC), ELEVATION=mean(ELEVATION))
  
  ## Loop through individual cells
  pb <- progress_bar$new(
    format = "Summarising cells [:bar] :percent eta: :eta",
    total=length(cells))
  
  for (cell in cells){

    #Initialize sea-level flag
    sea_level = FALSE
    
    #Sub-set input data frame by CELL_ID and determine number of records (i.e. HRUs) by cell
    recs <- hru_df[which(hru_df$CELL_ID==cell),]
    no_recs <- length(recs[[1]])
    
    #Sub-set data by band and get band ids for current cell
    bands <- band_df[which(band_df$CELL_ID==cell),]
    band_ids <- bands$BAND_ID
    if(min(band_ids)==min_band_id) sea_level = TRUE
    no_bands <- length(band_ids)
    
    #Ensure that area fractions sum to 1; correct for rounding errors if necessary
    #Determine area fraction correction factor (will equal one if area fractions sum to one)
    area_corr <- 1 + (1 - sum(recs$AREA_FRAC))/sum(recs$AREA_FRAC)
    HRU_AF <- recs$AREA_FRAC * area_corr
    BAND_AF <- bands$AREA_FRAC * area_corr
    
    #Set flag to TRUE if a glacier HRU (CLASS=glacierID) exists in current band
    GlacierInBand <- FALSE  
    #Track band index of previous record; initialize as zero or one for first record
    if(null_glaciers & (!sea_level)){
      prevBandIndex <- 1
    } else {
      prevBandIndex <- 0       
    }
    numNullGlaciers <- 0    #Number of null glacier HRUs in cell - add to record length
    
    #Loop through individual hru records
    for (r in 1:no_recs){
      
      #Determine band index - currently indexed from minimum BAND_ID per cell
      band_index <- which(band_ids == recs$BAND_ID[r])-1
      #Shift band index to account for extra bottom band, but only if lowest band not
      # already at sea level
      if(null_glaciers & (!sea_level)) band_index <- band_index+1
      
      #Add NULL glacier HRUs if option selected by user
      if(null_glaciers) {
        if (prevBandIndex!=band_index){
          if (!GlacierInBand){
            hru <- rbind(hru, c(glacierID, 0.0, 0.1, 1.00, 0.1, 0.0, 0.1, 0.0, prevBandIndex))
            numNullGlaciers <- numNullGlaciers + 1
          }
          GlacierInBand <- FALSE
        }
        prevBandIndex <- band_index
        if (recs$CLASS[r]==glacierID) GlacierInBand <- TRUE
      }
      
      #Look up appropriate rooting parameters from root_df based on vegetation class (CLASS)
      rt_index <- which(root_df$CLASS==recs$CLASS[r])
      thick1 = root_df$RTHICK1[rt_index]   #Thickness of top root zone [m]
      thick2 = root_df$RTHICK2[rt_index]   #Thickness of middle root zone [m]
      thick3 = root_df$RTHICK3[rt_index]   #Thickness of bottom root zone [m]
      rfrac1 = root_df$RFRAC1[rt_index]    #Fraction of roots in top root zone [m]
      rfrac2 = root_df$RFRAC2[rt_index]    #Fraction of roots in middle root zone [m]
      rfrac3 = root_df$RFRAC3[rt_index]    #Fraction of roots in bottom root zone [m]
      
      hru <- rbind(hru, c(recs$CLASS[r], HRU_AF[r], thick1, rfrac1, thick2, rfrac2, thick3,
                   rfrac3, band_index))
    } #HRU Loop
    
    #Check for glacier HRU in topmost band
    if(null_glaciers & !GlacierInBand) {
      hru <- rbind(hru, c(glacierID, 0.0, 0.1, 1.00, 0.1, 0.0, 0.1, 0.0, band_index))
      numNullGlaciers <- numNullGlaciers + 1
    }
    
    #Loop through individual bands
    for (b in 1:no_bands){
      bnd <- rbind(bnd, c(bands$ELEVATION[b], BAND_AF[b]))
    }
    
    #Update variables
    cell_vec <- c(cell_vec, cell)
    no_recs_vec <- c(no_recs_vec, no_recs+numNullGlaciers)
    colnames(hru) <- c("CLASS", "AFRAC", "THICK1", "RFRAC1", "THICK2", "RFRAC2", "THICK3",
                       "RFRAC3", "BAND")
    vparams <- append(vparams, list(hru))
    no_bands_vec <- c(no_bands_vec, no_bands)
    colnames(bnd) <- c("ELEV", "AFRAC")
    bparams <- append(bparams, list(bnd))
    hru <- NULL
    bnd <- NULL
    slevel <- c(slevel, sea_level)
    
    pb$tick()
    Sys.sleep(1/1000)
    
  } #Cell Loop
  
  ## Generate output list
  out_list <- list(NO_CELLS=no_cells, CELL_ID=cell_vec, NO_HRU=no_recs_vec,
                   NO_BANDS=no_bands_vec, VPARAM=vparams, BAND=bparams, SEA_LEVEL=slevel)
  
  ## Write HRU parameters to VIC-formatted file
  if (!is.null(vpf_filename)) {
    vpf_file <- file(description = vpf_filename, open="w")
    
    pb2 <- progress_bar$new(
      format = "Writing VPF [:bar] :percent eta: :eta",
      total=no_cells)
    
    for (x in 1:no_cells){
      txt_hdr <- sprintf(c("%.0f","%5.0f"), c(out_list$CELL_ID[x], out_list$NO_HRU[x]))
      write(txt_hdr, file=vpf_file, ncolumns=2)
      for (y in 1:out_list$NO_HRU[x]){
        txt_par <- sprintf(c("%8.0f","%15.12f","%5.2f","%5.2f","%5.2f","%5.2f","%5.2f","%5.2f","%4.0f"),
                      out_list$VPARAM[[x]][y,])
        write(txt_par, file=vpf_file, ncolumns=9)
      }
      pb2$tick()
      Sys.sleep(1/1000)
    }
    close(vpf_file)
  }
  
  ## Write band parameters to VIC-formatted file; extra band added below existing bands if
  ## null_glaciers = TRUE and lowest elevation band not already at sea level (SEA_LEVEL=FALSE)
  if (!is.null(snb_filename)) {
    band_file <- file(description = snb_filename, open="w")
    
    pb3 <- progress_bar$new(
      format = "Writing SPF [:bar] :percent eta: :eta",
      total=no_cells)
    
    for (x in 1:no_cells){
      #Formatted text for cell ID, band area fraction, elevation; pad with zeros to max_bands
      n_padded_bands <- max_bands - out_list$NO_BANDS[x]
      if(null_glaciers & (!out_list$SEA_LEVEL[x])){
        #Write additional blank elevation band 'below' existing bands
        n_padded_bands <- n_padded_bands - 1
        txt_af <- sprintf("%14.12f", c(0.0, out_list$BAND[[x]][,2], rep(0, n_padded_bands)))
        txt_elev <- sprintf("%5.0f", c(0.0, out_list$BAND[[x]][,1], rep(0, n_padded_bands)))
      } else {
        #Write elevation bands normally
        txt_af <- sprintf("%14.12f", c(out_list$BAND[[x]][,2], rep(0, n_padded_bands)))
        txt_elev <- sprintf("%5.0f", c(out_list$BAND[[x]][,1], rep(0, n_padded_bands)))
      }
      write(c(sprintf("%5.0f", out_list$CELL_ID[x]), txt_af, txt_elev), file=band_file, ncolumns=2*max_bands+1)
      pb3$tick()
      Sys.sleep(1/1000)
    }
    close(band_file)
  }
  return(out_list)
}
