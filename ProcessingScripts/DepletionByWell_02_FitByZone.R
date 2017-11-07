## DepletionByWell_02_FitByZone.R
#' This script is intended to plot model fit for streamflow depletion results
#' using the modified KGE metrics suggested by Gudmundsson et al. (2012)
#' 
#' This script requires output from DepletionByWell_01_AggregateAllResults.R

rm(list=ls())

# directory to repository on local computer
dir.git <- "C:/Users/Sam/WorkGits/NanaimoAttributionMethods/"

# load paths + packages
source(paste0(dir.git, "ProcessingScripts/paths+packages.R"))

## load data
df.all <- read.csv(paste0(dir.git, "data/DepletionByWell_01_AggregateAllResults.csv"))

# set factor levels
df.all$drainage.density <- factor(df.all$drainage.density, levels=c("LD", "MD", "HD"))
df.all$topography <- factor(df.all$topography, levels=c("FLAT", "ELEV"))
df.all$recharge <- factor(df.all$recharge, levels=c("NORCH", "RCH10", "RCH50", "RCH100", "RCH500", "RCH1000"))
df.all$method <- factor(df.all$method, levels=c("MODFLOW", "THIESSEN", "IDLIN", "IDLINSQ", "WEBLIN", "WEBLINSQ"))

# extract MODFLOW data and make it a column of its own in 'df' data frame
df.mod <- subset(df.all, method=="MODFLOW")
df <- subset(df.all, method != "MODFLOW")
colnames(df.mod)[colnames(df.mod)=="depletion.prc"] <- "depletion.prc.modflow"
df.mod$method <- NULL
df <- left_join(df, df.mod)

# calculate fit for each well, drainage density, topography, recharge, and method
df.fit.all <- summarize(group_by(df, Zone, drainage.density, topography, recharge, method),
                        bias = fit.bias(depletion.prc, depletion.prc.modflow),
                        amp = fit.amp(depletion.prc, depletion.prc.modflow),
                        cor = fit.cor(depletion.prc, depletion.prc.modflow),
                        mse = MSE(depletion.prc, depletion.prc.modflow))

# recreate figure 4 in Tom D report
df.fig4 <- melt(subset(df.fit.all, topography=="FLAT" & recharge=="NORCH"), id=c("Zone", "drainage.density", "topography", "recharge", "method"),
                value.name = "fit.prc", variable.name="fit.metric")
p.fig4 <- 
  ggplot(df.fig4, aes(x=method, y=log10(fit.prc))) +
  geom_boxplot() +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=2, fill="red", alpha=0.5) +
  facet_grid(drainage.density~fit.metric) +
  theme_scz()