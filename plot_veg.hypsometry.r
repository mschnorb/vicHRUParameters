plot_veg.hypsometry <- function(hru_df,
                                by_basin=FALSE){

  #Plot area of each vegetation class by elevation band. Plot either for entire
  #study area, by grid cell or by basin (if hru_df contains almalgamated data)
  
  #require("ggplot2")
  #require("plyr")
  require(tidyverse)
  source("./set_veg_legend.r")
  
  # Internal function(s) ###
  find.cell.area <- function(cellid,
                             area_df){
    ii <- which(area_df$CELL_ID == cellid)
    return(area_df$CELL_AREA[ii])
  }
  #######################
  
  #Set legend properties
  veg_class <- sort(unique(hru_df$CLASS))
  leg <- set_veg_legend(veg_class)
  
  #Calculate HRU area fractions if not already included in hru_df
  if(is.na(match("AREA_FRAC", names(hru_df)))){
    cell_area_df <- ddply(hru_df, .(CELL_ID), summarise, CELL_AREA=sum(AREA))
    area_vector <- sapply(hru_df$CELL_ID, find.cell.area, cell_area_df)
    hru_df$AREA_FRAC <- hru_df$AREA/area_vector
    #hru_df <- hru_df %>% group_by(CELL_ID) %>% mutate(AREA_FRAC=AREA/sum(AREA))
  }
  
  #Normalize cell area fractions
  if (by_basin){
    basins <- unique(hru_df$basin)
    for (b in basins){
      i <- which(hru_df$basin==b)
      no_cells_b <- length(unique(hru_df$CELL_ID[i]))
      hru_df$AREA_FRAC[i] <- hru_df$AREA_FRAC[i]/no_cells_b
    }
  } else {
    no_cells <- length(unique(hru_df$CELL_ID))
    hru_df$AREA_FRAC <- hru_df$AREA_FRAC/no_cells
  }
  
  ii <- order(hru_df$CLASS, hru_df$ELEVATION, hru_df$CELL_ID, decreasing=c(FALSE,FALSE,FALSE), method="radix")
  temp <- hru_df[ii,]
  
  if(by_basin){
      gplot <- 
        ggplot(temp, aes(x=BAND_ID, y=AREA_FRAC)) + 
        geom_bar(aes(fill=factor(CLASS)), stat="identity", position = position_stack(reverse = TRUE)) +
        scale_fill_manual(labels = leg$lbl, values=leg$clr) +
        theme_bw() + 
        labs(x="Elevation (m)", y="Area Fraction", fill="Land Cover") +
        facet_wrap(~basin) +
        coord_flip()
  } else {
    gplot <- 
      ggplot(temp %>% group_by(BAND_ID, CLASS) %>% summarise(AREA_FRAC=sum(AREA_FRAC)),
             aes(x=BAND_ID, y=AREA_FRAC)) + 
      geom_bar(aes(fill=factor(CLASS)), stat="identity", position = position_stack(reverse = TRUE)) +
      scale_fill_manual(labels = leg$lbl, values=leg$clr) +
      theme_bw() + 
      scale_x_continuous(limits=c(NA,NA)) + 
      #labs(x="Elevation (m)", y="Area Fraction", fill="Land Cover", 
      #     title="Vegetation Class Hypsometry") +
      labs(x="Elevation (m)", y="Area Fraction", fill="Land Cover") +
      coord_flip()    
  }
  
  return(gplot)
}
