plot_hru.hypsometry <- function(hru_df){
  
  #...
  
  require("tidyverse")
  
  #Set legend properties
  veg_class <- sort(unique(hru_df$CLASS))
  leg <- set_veg_legend(veg_class)
  
  #Order HRU data frame
  tmp <- hru_df |> arrange(BAND_ID, CLASS, AREA)

  gplot <- 
    ggplot(tmp, aes(factor(CLASS), BAND_ID)) + 
    #geom_count(aes(colour=factor(CLASS)), alpha=1/2) +
    geom_violin(aes(fill=factor(CLASS)), alpha=1/2, scale="count", width=6) +
    scale_fill_manual(labels = leg$lbl, values=leg$clr) +
    #scale_size(breaks=seq(0,5000,1000), range = c(2,15)) +
    theme_bw() +
    labs(x="Vegetation Class", y="Elevation (m)", fill="Class", title="HRU Hypsometry")

  return(gplot)
}
