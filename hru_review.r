###########################################################################################################
summarizeHRU <- function(x){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #DESCRIPTION: Summarize an hru table by CLASS and by BAND_ID
  
  #ARGUMENT(S):
  # x - hru table as data frame
  
  #VALUE:
  # Function returns a list containing two data frames:
  #   d1 returns summary of HRUs by CLASS, including total area for each class, median elevation of class,
  #     number of HRUs in each class, and area fraction of each class
  #   d2 returns summary of HRUs by BAND_ID, including total area of each elevation band, overall mean
  #     elevation of given band, number of HRUs in each band, area fraction of each band and cumulative
  #     area fraction of each band
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  require("tidyverse")
  
  d1 <- x |> group_by(CLASS) |> summarise(AREA=sum(AREA)/1000/1000, ELEV=median(ELEVATION*AREA)/sum(AREA), NO_HRU=length(BAND_ID))
  d1$AREA_FRAC <- d1$AREA/sum(d1$AREA)
  d2 <- x |> group_by(BAND_ID) |> summarise(AREA=sum(AREA)/1000/1000, ELEV=median(ELEVATION*AREA)/sum(AREA), NO_HRU=length(CLASS))
  d2$AREA_FRAC <- d2$AREA/sum(d2$AREA)
  d2$CUM_AREA_FRAC <- cumsum(d2$AREA_FRAC)
  
  return(list(VEG=d1,ELEV=d2))
}


###########################################################################################################
summarizeHRUbyCell <- function(x){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #DESCRIPTION: Summarize an hru table by CELL_ID
  
  #ARGUMENT(S):
  # x - hru table as data frame
  
  #VALUE:
  # Function returns a data frame containing the following fields:
  # CELL_ID -   Cell id number
  # AREA -      Total area of cell
  # ELEV -      mean elevation of cell (mean of band elevations)
  # No_CLASS -  Number of vegetation classes in each cell
  # NO_BAND -   Number of elevation bands in each cell
  # NO_HRU -    Number of HRUs in each cell
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  require("tidyverse")
  
  d1 <- x |> group_by(CELL_ID) |> summarise(
              AREA=sum(AREA),
              ELEV=mean(ELEVATION),
              NO_CLASS = length(unique(CLASS)),
              NO_BAND=length(unique(BAND_ID)),
              NO_HRU=length(CLASS))
  
  return(d1)
}

###########################################################################################################
statsHRU <- function(hru_df,
                     qc = FALSE, 
                     minBand = 50){
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #DESCRIPTION: Check HRU data frame for errors/discrepancies and fix; provide some hru statistics
  
  #ARGUMENT(S):
  # hru_df -  data frame of HRU table
  # qc -      quality control flag; default is FALSE 
  # minBand - minimum band ID
  
  #VALUE:
  # If qc = TRUE then return data frame with corrected ELEVATION and BAND_ID values, otherwise return NULL
  # Function always prints descriptive statistics to stdout
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  require("tidyverse")
  
  if (qc) {
    #Check for elevation errors
    ind <- which(hru_df$BAND_ID < minBand)
    hru_df$BAND_ID[ind] <- minBand
    hru_df$ELEVATION[ind] <- 0
    hru_df$POLY_ID[ind] <- paste(hru_df$CELL_ID[ind], minBand, sep="-")
  }
  
  #Calculate and print statistics
  cat("             Minimum elevation (m):", round(min(hru_df$ELEVATION)), "\n",
      "            Maximum elevation (m):", round(max(hru_df$ELEVATION)), "\n",
      "                  Minimum band ID:", min(hru_df$BAND_ID), "\n",
      "                  Maximum band ID:", max(hru_df$BAND_ID), "\n",
      "      Maximum cell band range (m):", max((hru_df |> group_by(CELL_ID) |> summarise(range=max(BAND_ID)-min(BAND_ID)))$range), "\n",
      "                Total area (km^2):", round(sum(hru_df$AREA)/1000/1000,2), "\n",
      "                  Number of cells:", length(unique(hru_df$CELL_ID)), "\n",
      "         Average cell area (km^2):", round(sum(hru_df$AREA)/length(unique(hru_df$CELL_ID))/1000/1000,2), "\n",
      "                Number of classes:", length(unique(hru_df$CLASS)), "\n",
      "                   Number of HRUs:", length(hru_df$CELL_ID), "\n",
      "     Mean number of HRUs per cell:", round(length(hru_df$CELL_ID)/length(unique(hru_df$CELL_ID)),1), "\n",
      "      Min number of HRUs per cell:", min((hru_df |> group_by(CELL_ID) |> summarise(count=length(CLASS)))$count), "\n",
      "      Max number of HRUs per cell:", max((hru_df |> group_by(CELL_ID) |> summarise(count=length(CLASS)))$count), "\n", 
      "Effective model resolution (km^2):", round(sum(hru_df$AREA)/1000/1000,2) / length(hru_df$CELL_ID), "\n", sep=" ")
  
  if (qc) return(hru_df) else return(NULL)
}