plot_cum.veg.hypsometry <- function(hru_df){
  
  #Plot cumulative area of each vegetation class by elevation band for entire file
  
  pckg <- list("ggplot2", "plyr")
  do.call(require, pckg)
  source("./set_veg_legend.r")
  
  #Set legend properties
  veg_class <- sort(unique(hru_df$CLASS))
  leg <- set_veg_legend(veg_class)
  
  #Normalize area fractions
  hru_df$AREA <- hru_df$AREA/sum(hru_df$AREA)
  
  #Calculate cumulative area by vegetation class
  #Order df, remove POLY_ID, and take cumsum
  ii <- order(hru_df$CLASS, hru_df$ELEVATION)
  if(any(names(hrudf_bccoast)=="POLY_ID")){
    j <- which(names(hru_df)=="POLY_ID")
    hru_df <- hru_df[ii,-j]
  } else {
    hru_df <- hru_df[ii,]
  }
  temp_df <- ddply(hru_df, .(CLASS), cumsum)
  hru_df$CAREA <- temp_df$AREA

  #Build ggplot by layers
  gplot <- ggplot(data=hru_df, aes(x=ELEVATION, y=CAREA)) + 
    geom_line(aes(color=factor(CLASS)), size=1) + 
    scale_color_manual("Land Cover", labels = leg$lbl, values=leg$clr) + 
    labs(x="Elevation (m)", y="Cumulative Area Fraction", title="Cumulative Vegetation Hypsometry") +
    coord_flip() +
    theme_bw()
  
  return(gplot)
  
}
