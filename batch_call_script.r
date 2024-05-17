lapply(sort(unique(cell_map_bccoast_all$BASIN)), function(x){
  cat("Processing basin ", x, ".\n", sep="")
  call_make.VIC.param(hru_bccoast_200,
                      root_depth,
                      file.path("../parameters", paste("vpf_",x,"_200_glac.txt", sep="")),
                      file.path("../parameters", paste("snb_",x,"_200_glac.txt", sep="")),
                      basin=x,
                      celldf=cell_map_bccoast_all,
                      minb=100,
                      nullg=TRUE)
})
