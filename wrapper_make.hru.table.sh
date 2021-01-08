#!/bin/bash -u

RSCRIPT=/storage/data/projects/hydrology/vic_gen2/code/wrapper_make.hru.table.r
BASINFILE=__PLACEHOLDER__
CELLMAP=/storage/data/projects/hydrology/vic_gen2/input/routing/bccoast/lat_lon_names_bccoast.csv
DEMFILE=/storage/data/gis/basedata/DEMS/GMTED2010/7_5_arcsecond/gmted_mea075_4072N_169101W.tif
FNCFILE=/storage/data/projects/hydrology/vic_gen2/code/vegetation/make_hru.table.r
OUTDIR=/storage/data/projects/hydrology/vic_gen2/input/vegetation/hru/processing/hru_tables
ROOTDEPTH=/storage/data/projects/hydrology/vic_gen2/input/vegetation/parameters/root_depths.csv
SOILFILE=/storage/data/projects/hydrology/vic_gen2/input/soils/gis/gen2_soil_polygon_land.shp
VEGFILE=/storage/data/projects/hydrology/vic_gen2/input/vegetation/gis/NA_LandCover_2005_v2_vic_gen2_final.tif

module load R
Rscript --vanilla --quiet $RSCRIPT -b $BASINFILE -s $SOILFILE -d $DEMFILE -v $VEGFILE \
  -c $CELLMAP -r $ROOTDEPTH -o $OUTDIR -f $FNCFILE -S
