## Depletion_01_AggregateAllResults.R
#' This script is intended to aggregate streamflow depletion estimates for
#' all drainage densities, topographies, recharge values, and models.
#' 
#' This script should only have to be run once, unless it is necessary
#' to update Tom D's raw model results on the GSAS directory for some reason.

rm(list=ls())

# directory to repository on local computer
dir.git <- "C:/Users/Sam/WorkGits/NanaimoAttributionMethods/"

# load paths + packages
source(paste0(dir.git, "ProcessingScripts/paths+packages.R"))

# list of all CSV files
files.all <- list.files(paste0(dir.GSAS, "Results_Depletion/"))

# load files
start.flag <- T
for (file in files.all){
  # parse name
  traits <- str_split(tools::file_path_sans_ext(file), pattern="_", n=4, simplify=T)
  
  # load file
  df.in <- read.csv(paste0(dir.GSAS, "Results_Depletion/", file))
  
  # replace well string with numeric
  df.in$Well <- as.numeric(substring(df.in$Well, first=5))
  
  # melt to longform
  df.in.melt <- melt(df.in, id=c("Well"), value.name="depletion.prc", variable.name="reach")
  
  # replace zone string with numeric
  df.in.melt$reach <- as.numeric(substring(df.in.melt$reach, first=5))
  
  # add relevant output data
  df.in.melt$drainage.density <- traits[1]
  df.in.melt$topography <- traits[2]
  df.in.melt$recharge <- traits[3]
  df.in.melt$method <- traits[4]
  
  # output data frame
  if (start.flag){
    df.all <- df.in.melt
    start.flag <- F
  } else {
    df.all <- rbind(df.all, df.in.melt)
  }
  
  # status update
  print(paste0(file, " complete"))
}

# rename columns
colnames(df.all)
colnames(df.all)[1] <- "well"

# save output
write.csv(df.all, paste0(dir.git, "data/Depletion_01_AggregateAllResults.csv"), row.names=F)
