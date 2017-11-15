## paths+packages.R
#' This script contains relevant paths and packages common to multiple processing scripts.

## packages
require(dplyr)
require(ggplot2)
require(hydroGOF)
require(stringr)
require(reshape2)
require(raster)
require(rgdal)
require(rgeos)
require(maptools)

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


# named vectors defining color strings
pal.density <- c("LD"=col.LD, "MD"=col.MD, "HD"=col.HD)
pal.topo <- c("FLAT"=col.LD, "ELEV"=col.elev)
pal.recharge <- c("NORCH"=col.LD, "RCH10"=col.RCH10, "RCH50"=col.RCH50, "RCH100"=col.RCH100, "RCH500"=col.RCH500, "RCH1000"=col.RCH1000)

## labels
labels.recharge <- c("NORCH"="0", "RCH10"="10", "RCH50"="50", "RCH100"="100", "RCH500"="500", "RCH1000"="1000")

## paths
# path to directory on GSAS with data
dir.GSAS <- "Z:/2.active_projects/Zipper/2.Model_data/NanaimoAttributionMethods/"

# path to Dropbox
dir.drop <- "C:/Users/Sam/Dropbox/Work/"

# path to plots
dir.plot <- paste0(dir.drop, "StreamflowDepletion/Plots/NanaimoAttributionMethods/")

# path to publication-quality figures
dir.fig <- paste0(dir.git, "Figures+Tables/")

## functions from Gudmundsson et al. (2012) for modified version of KGE
# eq. 5 - units of output from these will be same as input units of sim and obs
#         the ideal value for each of these is 0.0
fit.bias <- function(sim,obs){(mean(sim)-mean(obs))^2}
fit.amp <- function(sim,obs){(sd(sim)-sd(obs))^2}
fit.cor <- function(sim,obs){2*sd(sim)*sd(obs)*(1-cor(sim,obs))}
MSE <- function(sim,obs){fit.bias(sim,obs)+fit.amp(sim,obs)+fit.cor(sim,obs)}   # this outputs slightly different results than mse() in the hydroGOF package

# eq. 6 - the ideal value for each is 0.0,but these are normalized and will always sum to 1.0
fit.bias.norm <- function(sim,obs){
  fit.bias(sim,obs)/MSE(sim,obs)
}

fit.amp.norm <- function(sim,obs){
  fit.amp(sim,obs)/MSE(sim,obs)
}

fit.cor.norm <- function(sim,obs){
  fit.cor(sim,obs)/MSE(sim,obs)
}

## ggplot theme
theme_scz <- function(...){
  theme_bw() + 
    theme(panel.grid=element_blank())
}
