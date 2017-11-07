## paths+packages.R
#' This script contains relevant paths and packages common to multiple processing scripts.

## packages
require(dplyr)
require(ggplot2)
require(hydroGOF)
require(stringr)
require(reshape2)

## paths
# path to directory on GSAS with data
dir.GSAS <- "Z:/2.active_projects/Zipper/2.Model_data/NanaimoAttributionMethods/"

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
