#!/bin/bash -u

RSCRIPT=/Users/mschnorbus/Work/vegetation/Rhru/wrapper_make.hru.table.r
BASINFILE=/Users/mschnorbus/Work/vegetation/data/bc_basins_poly.shp
CELLMAP=/Users/mschnorbus/Work/vegetation/data/lat_lon_names_bccoast.csv
DEMFILE=/Users/mschnorbus/Work/vegetation/data/gmted_mea075_4072N_169101W.tif
FNCFILE=/Users/mschnorbus/Work/vegetation/Rhru/make.hru.table.r
OUTDIR=/Users/mschnorbus/Work/vegetation/data/
ROOTDEPTH=/Users/mschnorbus/Work/vegetation/data/root_depths.csv
SOILFILE=/Users/mschnorbus/Work/vegetation/data/gen2_soil_polygon_land.shp
VEGFILE=/Users/mschnorbus/Work/vegetation/data/NA_LandCover_2005_v2_vic_gen2_final.tif

Rscript --vanilla --quiet $RSCRIPT -b $BASINFILE -s $SOILFILE -d $DEMFILE -v $VEGFILE -o $OUTDIR -f $FNCFILE -S -c $CELLMAP -r $ROOTDEPTH
