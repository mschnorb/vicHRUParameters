library(tidyverse)
library(readr)
library(plyr)

# Read snowband parameter file
read.snb <- function(snb.file){
  snb <- read_table(snb.file, col_names = FALSE)
  tmp <- as_tibble(cbind(snb %>% select(X1:X21) %>% pivot_longer(X2:X21, names_to="BAND", values_to="AFRAC"),
                         snb %>% select(X1,X22:X41) %>% pivot_longer(X22:X41, names_to="BAND", values_to="ELEV") %>% select(ELEV)))
  names(tmp) <- c("CELLID", "XID", "AFRAC", "ELEV")
  key <- tibble(XID = paste("X", 2:21, sep=""), BAND=0:19)
  return(left_join(tmp,key) %>% select(CELLID, BAND, AFRAC, ELEV))
}
#snb_file <- read.snb("~/Work/tmp/snb_HOMAM_200_glac.txt")
snb_file <- read.snb("~/Work/vicgl/vegetation/Rhru/snb_HOMAM_200_LCC_glac.txt")

# Read vegetation parameter file
read.vpf <- function(vpf.file){
  # Read vegetation parameter file
  tmp <- readLines(vpf.file)
  n <- 1
  m <- 0
  out_list <- NULL
  while(n < length(tmp)){
    inSplit <- strsplit(tmp[n], " +")
    cellid <- as.numeric(inSplit[[1]][1])
    nhru <- as.numeric(inSplit[[1]][2])
    for(x in 1:nhru){
      m <- n+x
      inSplit <- strsplit(tmp[m], " +")
      vegc <- as.numeric(inSplit[[1]][2])
      afrc <- as.numeric(inSplit[[1]][3])
      band <- as.numeric(inSplit[[1]][10])
      out_list[[m-1]] <- c(cellid, vegc, afrc, band)
    }
    n <- n + nhru + 1
  }
  vpf <- as_tibble(do.call(rbind,out_list))
  names(vpf) <- c("CELLID", "VEG_CLASS", "AFRAC", "BAND")
  return(vpf)
}

vpf_file <- read.vpf("~/Work/vicgl/vegetation/Rhru/vpf_HOMAM_200_LCC_glac.txt")

# Read pixel map
pixel_map <- read_table("~/Work/tmp/pixel_map_HOMAM_SRTM200.txt", skip = 2,
                        col_types = cols(PIXEL_ID = col_double(),
                                         ROW = col_double(),
                                         COL = col_double(),
                                         BAND = col_double(),
                                         ELEV = col_double(),
                                         CELL_ID = col_integer()))

#Check for matching CELLIDs
check.cell.ids <- function(vpf, snb, pxmap){
  s <- sort(unlist(snb_file %>% distinct(CELLID)), na.last = TRUE) #CELLID should not have NA values
  v <- sort(unlist(vpf_file %>% distinct(CELLID)), na.last = TRUE) #CELLID should not have NA values 
  p <- sort(unlist(pixel_map %>% distinct(CELL_ID)))  #CELL_ID can have NA values
  nx <- max(length(s),length(v),length(p))
  n1 <- nx - length(v)
  n2 <- nx - length(s)
  n3 <- nx - length(p)
  cell.check <- 
    tibble(VPF  = c(v, rep(NA,n1)),
           SNB  = c(s, rep(NA,n2)),
           PMAP = c(p, rep(NA,n3))) %>%
    mutate(PASS=if_all(SNB:PMAP, function(x,y){x==y},y=VPF))
}

#Check cell areas
check.cell.areas <- function(vpf,snb){
  v <- vpf %>% group_by(CELLID) %>% summarise(AREA=sum(AFRAC))
  s <- snb %>% group_by(CELLID) %>% summarise(AREA=sum(AFRAC))
  tibble(CELLID = v$CELLID, AREAV=v$AREA, AREAB=s$AREA) %>%
    mutate(TEST1 = abs(AREAV-1) < 0.000001,
           TEST2 = abs(AREAB-1) < 0.000001) %>%
    mutate(PASS = if_all(TEST1:TEST2, function(x){x}))
}

check.band.ids <- function(snb_file) {
  as_tibble(cbind(snb_file,BAND_ID=0)) %>%
    filter(AFRAC > 0) %>%
    mutate(BAND_ID = round_any(ELEV, 200, floor)+100)
}

check.band.areas <- function(vpf,snb){
  v <- vpf_file %>% group_by(CELLID, BAND) %>% summarise(AREA=sum(AFRAC))
  s <- snb_file %>% filter(AFRAC>0) %>% group_by(CELLID, BAND) %>% summarise(AREA=sum(AFRAC))
  tibble(CELLID=v$CELLID, BANDV=v$BAND, AREAV=v$AREA, BANDS=s$BAND, AREAB=s$AREA) %>%
    mutate(TEST1 = BANDV==BANDS,
           TEST2 = abs(AREAV-AREAB) < 0.000001) %>%
    mutate(PASS = if_all(TEST1:TEST2, function(x){x}))
}

check.band.ranges <- function(snb_file) {
  snb_file %>%
    filter(AFRAC>0) %>%
    group_by(CELLID) %>%
    summarise(FLOOR = min(ELEV)-min(ELEV)%%200,
              CEILING = max(ELEV)-max(ELEV)%%200+200,
              N1=n()) %>%
    mutate(N2=(CEILING-FLOOR)/200) %>%
    mutate(PASS = N1==N2)
}

check_cell_ids <- check.cell.ids(vpf_file, snb_file, pixel_map)
check_cell_area <- check.cell.areas(vpf_file, snb_file)
check_band_ranges <- check.band.ranges(snb_file)
check_band_area <- check.band.areas(vpf_file, snb_file)
check_band_ids <- check.band.ids(snb_file)


subset.hru.frame <- function(basin, cellmap, hrudf){
  #Subset main data frame if required; be flexible with cellmap column names
  nind <- min(match(c("name","names","NAME","NAMES","basin","BASIN","basins","BASINS"), names(cellmap)), na.rm = TRUE)
  if(!any(cellmap[[nind]]==basin, na.rm = TRUE)){
    warning(paste("Sub-basin '", basin, "' could not be found in supplied data frame.", sep=""))
    return(paste("make_VIC_param_ERROR: Sub-basin '", basin, "' could not be found in supplied data frame.", sep=""))
  }
  names(cellmap)[nind] <- "BASIN"   # Update name of Basin column
  cind <- min(match(c("cellid","cell_id","CELLID","CELL_ID"), names(cellmap)), na.rm = TRUE)
  names(cellmap)[cind] <- "CELL_ID" # Update name of Cell ID column
  
  left_join(hrudf, cellmap, by="CELL_ID", relationship = "many-to-many") |>
    filter(BASIN==basin) |> select(CELL_ID,BAND_ID,CLASS,AREA,AREA_FRAC,ELEVATION)
}

## BC Coast Basin CLASS area summary
bccoast_basin_summary <- lapply(sort(unique(cell_map_bccoast_all$BASIN)), function(x){
  subset.hru.frame(x, cell_map_bccoast_all, hru_bccoast_200) |>
    group_by(CLASS) |>
    summarise(AREA=sum(AREA)) |>
    mutate(AREA_FRAC = AREA/sum(AREA))
})
names(bccoast_basin_summary) <- sort(unique(cell_map_bccoast_all$BASIN))

## Summarise glacier area
bccoast_basin_summary_glacier <- 
  do.call(rbind, lapply(names(bccoast_basin_summary), function(x,y){
    cind <- which(names(y)==x)
    rind <- which(y[[cind]]$CLASS == 22)
    tibble(BASIN=x,
           AREA = y[[cind]]$AREA[rind]/1000/1000,
           AREA_FRAC=y[[cind]]$AREA_FRAC[rind] * 100)
    }, y=bccoast_basin_summary))

bccoast_basin_summary_glacier |>
  ggplot(aes(x=factor(BASIN, BASIN[order(AREA)]))) +
  geom_col(aes(y=AREA)) +
  geom_point(aes(y=AREA_FRAC*25), color="blue") +
  labs(x = "Basin", y= "Glacier Area") +
  theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~.* 100/2500, name = "Area Fraction (%)", breaks = seq(0,100,20))) +
  theme(axis.text.x = element_text(angle = 60))
