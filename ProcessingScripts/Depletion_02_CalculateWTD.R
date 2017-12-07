## Depletion_02_CalculateWTD.R
#' This script is intended to calculate WTD.

rm(list=ls())

# directory to repository on local computer
dir.git <- "C:/Users/Sam/WorkGits/NanaimoAttributionMethods/"

# load paths + packages
source(paste0(dir.git, "ProcessingScripts/paths+packages.R"))

# define path to MODFLOW output
dir.shps <- "Z:/2.active_projects/TomDallemagne/PROJECTS/MAIN_PROJECT/MAP_LD/MAP_LD_10LAY_ELEVATION/ED_MAP_LD_10LAY_ELEVATION.data"

# water table exported from Visual MODFLOW
df.WTE <- read.csv(paste0(dir.GSAS, "GIS/LD_ELEV_WaterTableElevation_NoPumpingWells.csv"), stringsAsFactors=F)

# number of layers
n.layer <- 10

# cycle through layers and load
for (layer in seq(1,n.layer)){
  # load files
  shp.horizon <- readOGR(dsn=dir.shps, layer=paste0("Results_Export_NumericalGrid1_Layer", layer), stringsAsFactors=F)
  shp.heads <- readOGR(dsn=dir.shps, layer=paste0("Results_Export_Heads_Layer", layer), stringsAsFactors=F)
  shp.drawdown <- readOGR(dsn=dir.shps, layer=paste0("Results_Export_Drawdown_Layer", layer), stringsAsFactors=F)
  
  # combine
  df.layer <- left_join(as.data.frame(shp.horizon), shp.heads@data)
  colnames(df.layer)[colnames(df.layer)=="T1"] <- "head.m"
  
  df.layer <- left_join(df.layer, shp.drawdown@data)
  colnames(df.layer)[colnames(df.layer)=="T1"] <- "drawdown.m"
  
  # add to output
  if (layer==1){
    df.all <- df.layer
  } else {
    df.all <- rbind(df.all, df.layer)
  }
  
}

# set NaNs
df.all$head.m[df.all$head.m>=1e10] <- NaN
df.all$head.m[df.all$head.m<=-1e10] <- NaN

df.all$drawdown.m[df.all$drawdown.m>=1e10] <- NaN
df.all$drawdown.m[df.all$drawdown.m<=-1e10] <- NaN

# extract shallowest layer with data
df.wet <- subset(df.all, complete.cases(df.all))
df.shallowest <- dplyr::summarize(group_by(df.wet, ROW, COLUMN),
                                  layer.shallowest = min(LAYER))

# subset to top wet layer only
df.wet <- left_join(df.wet, df.shallowest)
df.wet <- df.wet[df.wet$LAYER==df.wet$layer.shallowest, c("ROW", "COLUMN", "head.m", "drawdown.m")]

# make output data frame
df.wet <- left_join(subset(df.all, LAYER==1, select=c("ROW", "COLUMN", "TOP", "coords.x1", "coords.x2")), df.wet, by=c("ROW", "COLUMN"))
df.wet$TOP[is.na(df.wet$head.m)] <- NA

# set column names
colnames(df.wet) <- c("ROW", "COLUMN", "elev.ground.m", "long", "lat", "head.m", "WTD.m")

# save
write.csv(df.wet, paste0(dir.git, "data/Depletion_02_CalculateWTD.csv"), row.names=F)