## paths+packages.R
#' This script contains relevant paths and packages common to multiple processing scripts.

## packages
require(dplyr)
require(ggplot2)
require(ggtern)
require(hydroGOF)
require(stringr)
require(reshape2)
require(raster)
require(rgdal)
require(rgeos)
require(maptools)
require(gridExtra)
require(tidyr)
require(broom)
require(gstat)
require(extrafont)

## colors
# from http://paletton.com/#uid=7000u0ktSlllysDruqa-qh2KKbE
col.LD <- "#D01D1D"
col.MD <- "#D06F1D"
col.HD <- "#127D7D"
col.elev <- "#18A718"
col.ramp.rech <- colorRampPalette(c("#2D8989", "#003838"))(5)
col.RCH10 <- col.ramp.rech[1]
col.RCH50 <- col.ramp.rech[2]
col.RCH100 <- col.ramp.rech[3]
col.RCH500 <- col.ramp.rech[4]
col.RCH1000  <- col.ramp.rech[5]
pal.density <- c("LD"=col.LD, "MD"=col.MD, "HD"=col.HD)
pal.topo <- c("FLAT"=col.LD, "ELEV"=col.elev)
pal.recharge <- c("NORCH"=col.LD, "RCH10"=col.RCH10, "RCH50"=col.RCH50, "RCH100"=col.RCH100, "RCH500"=col.RCH500, "RCH1000"=col.RCH1000)

# depletion
#col.ramp.depletion <- colorRampPalette(c("blue", col.LD))(5)
col.lt5 <- "blue"
col.5to10 <- "#00D9D9"
col.10to15 <- col.elev
col.15to20 <- col.MD
col.gt20 <- col.LD
pal.depletion <- c("<5%"=col.lt5, "5-10%"=col.5to10, "10-15%"=col.10to15, "15-20%"=col.15to20, ">20%"=col.gt20)
pal.depletion.0to100 <- rev(c("#a50026",  "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
                              "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"))

# from http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=5
col.TP <- "#1b9e77"
col.ID <- "#d95f02"
col.ID2 <- "#7570b3"
col.WID <- "#e7298a"
col.WID2 <- "#66a61e"
pal.method <- c("THIESSEN"=col.TP, "IDLIN"=col.ID, "IDLINSQ"=col.ID2, "WEBLIN"=col.WID, "WEBLINSQ"=col.WID2)

# stream length quartile
col.Q1 <- pal.depletion.0to100[11]
col.Q2 <- pal.depletion.0to100[8]
col.Q3 <- pal.depletion.0to100[4]
col.Q4 <- pal.depletion.0to100[1]
pal.quartile <- c("Q1"=col.Q1, "Q2"=col.Q2, "Q3"=col.Q3, "Q4"=col.Q4)
pal.length.class <- c("<0.1"=pal.depletion.0to100[11], "0.1-0.5"=pal.depletion.0to100[9], 
                      "0.5-1"=pal.depletion.0to100[7], "1-5"=pal.depletion.0to100[3], 
                      ">5"=pal.depletion.0to100[1])

## labels
labels.recharge <- c("NORCH"="0", "RCH10"="10", "RCH50"="50", "RCH100"="100", "RCH500"="500", "RCH1000"="1000")
labels.method <- c("THIESSEN"="TPOLY", "IDLIN"="ID", "IDLINSQ"="IDS", "WEBLIN"="WID", "WEBLINSQ"="WIDS")
labels.quartile <- c("Q1"="0-25%", "Q2"="25-50%", "Q3"="50-75%", "Q4"="75-100%")
labels.density <- c("HD"="HD", "MD"="MD", "LD"="LD")

## paths
# path to directory on GSAS with data
dir.GSAS <- "Z:/2.active_projects/Zipper/2.Model_data/NanaimoAttributionMethods/"

# path to Dropbox
dir.drop <- "C:/Users/Sam/Dropbox/Work/"

# path to plots
dir.plot <- paste0(dir.drop, "StreamflowDepletion/NanaimoAttributionMethods/Plots/")

# path to publication-quality figures
dir.fig <- paste0(dir.git, "Figures+Tables/")

## functions from Gudmundsson et al. (2012) for modified version of KGE
# eq. 5 - units of output from these will be same as input units of sim and obs
#         the ideal value for each of these is 0.0
sd.p <- function(x){sqrt((length(x)-1)/length(x))*sd(x)}  # from https://stackoverflow.com/questions/44339070/calculating-population-standard-deviation-in-r
MSE.bias <- function(sim,obs){(mean(sim)-mean(obs))^2}
MSE.var <- function(sim,obs){(sd.p(sim)-sd.p(obs))^2}
MSE.cor <- function(sim,obs){
  if (sd(obs)==0 | sd(sim)==0){
    0 
  } else {
    2*sd.p(sim)*sd.p(obs)*(1-cor(sim,obs))
  }
}
MSE <- function(sim,obs){MSE.bias(sim,obs)+MSE.var(sim,obs)+MSE.cor(sim,obs)}   # this outputs slightly different results than mse() in the hydroGOF package

# eq. 6 - the ideal value for each is 0.0,but these are normalized and will always sum to 1.0
MSE.bias.norm <- function(sim,obs){
  MSE.bias(sim,obs)/MSE(sim,obs)
}

MSE.var.norm <- function(sim,obs){
  MSE.var(sim,obs)/MSE(sim,obs)
}

MSE.cor.norm <- function(sim,obs){
  MSE.cor(sim,obs)/MSE(sim,obs)
}

# R2
R2 <- function(sim, obs) {
  if (length(sim) != length(obs)) stop("vectors not the same size")
  return((sum((obs-mean(obs))*(sim-mean(sim)))/
            ((sum((obs-mean(obs))^2)^0.5)*(sum((sim-mean(sim))^2)^0.5)))^2)
}

## ggplot theme
windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw() + 
    theme(
      text=element_text(size=8, color="black"),
      axis.title=element_text(face="bold", size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      panel.grid=element_blank())
}

# extract legend - https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
