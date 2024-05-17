plot_hypsometry <- function(hru_df, by_cell=FALSE){
  
  #Plot study area hypsometry (cumulative basin area by elevation)
  
  require("tidyverse")
  
  # Internal function(s) ###
  find.cell.area <- function(cellid,
                             area_df){
    ii <- which(area_df$CELL_ID == cellid)
    return(area_df$CELL_AREA[ii])
  }
  #######################
  
  #Calculate HRU area fractions if not already included in hru_df
  if(is.na(match("AREA_FRAC", names(hru_df)))){
    cell_area_df <- hru_df |> group_by(CELL_ID) |> summarise(CELL_AREA=sum(AREA))
    area_vector <- sapply(hru_df$CELL_ID, find.cell.area, cell_area_df)
    hru_df$AREA_FRAC <- hru_df$AREA/area_vector
  }
  
  #Order data and calculate cumulative area fraction
  no_cells <- length(unique(hru_df$CELL_ID))
  if (!by_cell){
    hru_df$AREA_FRAC <- hru_df$AREA_FRAC/no_cells
    temp <- hru_df |> arrange(ELEVATION, AREA_FRAC)
    temp$CAREA <- cumsum(temp$AREA_FRAC)
  } else {
    temp <- hru_df |> arrange(CELL_ID, ELEVATION, AREA_FRAC)
    temp$CAREA <- (temp |> group_by(CELL_ID) |> reframe(CAREA=cumsum(AREA_FRAC)))$CAREA
  }
  
  #Create plot
  if (by_cell){
    gplot <- 
      ggplot(temp, aes(x=CAREA, y=ELEVATION)) +
      geom_line() +
      facet_wrap(~CELL_ID) +
      labs(x="Area Fraction", y="Elevation", title="Study Area Hypsometry", color="Cell ID") +
      theme_bw()
  } else {
    gplot <- 
      ggplot(temp, aes(x=CAREA, y=ELEVATION)) +
      geom_line() +
      labs(x="Area Fraction", y="Elevation", title="Study Area Hypsometry") +
      theme_bw()
  }
  return(gplot)
}