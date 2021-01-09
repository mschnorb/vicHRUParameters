# vicHRUParameters
Code to generate vegetation and snowband parameter files for VICGL model.

This set of code and wrapper scripts is designed for two main tasks:
  1) Produce an HRU table from the interesection of the VICGL computational grid, the outline of the study domain, elevation data and landcover data
  2) Using the derived HRU table, produce vegetation and snowband parameter files.

Additional utilities are included to provide satistical and plot summaries of the HRU table.

Each task is achieved by running one of the two scripts: wrapper_make.hru.table.r and wrapper_make.VIC.param.r, which are intended to be run from the command line using the Rscript frontend.
Each wrapper script respectively sources and calls functions from make.hru.table.r and make.VIC.param.r
