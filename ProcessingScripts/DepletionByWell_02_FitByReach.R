## DepletionBywell_02_FitByReach.R
#' This script is intended to plot model fit for streamflow depletion results
#' using the modified KGE metrics suggested by Gudmundsson et al. (2012)
#' 
#' This script requires output from DepletionBywell_01_AggregateAllResults.R

rm(list=ls())

# directory to repository on local computer
dir.git <- "C:/Users/Sam/WorkGits/NanaimoAttributionMethods/"

# load paths + packages
source(paste0(dir.git, "ProcessingScripts/paths+packages.R"))

## load data
df.all <- read.csv(paste0(dir.git, "data/DepletionBywell_01_AggregateAllResults.csv"))

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
df.fit.all <- summarize(group_by(df, reach, drainage.density, topography, recharge, method),
                        bias = fit.bias(depletion.prc, depletion.prc.modflow),
                        amp = fit.amp(depletion.prc, depletion.prc.modflow),
                        cor = fit.cor(depletion.prc, depletion.prc.modflow),
                        mse = MSE(depletion.prc, depletion.prc.modflow))

# make fit labels
fit.labels <- c("bias"="Bias MSE [%]", "amp"="Amplitude MSE [%]", "cor"="Correlation MSE [%]", "mse"="Total MSE [%]")

## comparing fit across different drainage densities (figure 4 in Tom D report)
df.fit.density <- melt(subset(df.fit.all, topography=="FLAT" & recharge=="NORCH"), id=c("reach", "drainage.density", "topography", "recharge", "method"),
                value.name = "fit.prc", variable.name="fit.metric")
p.fit.density <- 
  ggplot(df.fit.density, aes(x=method, y=log10(fit.prc), fill=drainage.density)) +
  geom_hline(yintercept=c(0,1,2), color="gray65") +
  geom_boxplot(outlier.shape=21) +
#  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=2, fill="red", alpha=0.5) +
  facet_wrap(~fit.metric, labeller=as_labeller(fit.labels)) +
  scale_x_discrete("Method") +
  scale_y_continuous("log(Fit) [%]") +
  scale_fill_manual(name="Drainage Density", values=pal.density) +
  theme_scz() +
  theme(legend.position="bottom")
ggsave(paste0(dir.plot, "DepletionByWell_02_FitByReach_p.fit.density.png"),
       p.fit.density, width=8, height=6, units="in")

## comparing fit across different elevations (figure 5 in Tom D report)
df.fit.recharge <- melt(subset(df.fit.all, drainage.density=="LD" & recharge=="NORCH"), id=c("reach", "drainage.density", "topography", "recharge", "method"),
                    value.name = "fit.prc", variable.name="fit.metric")
p.fit.recharge <- 
  ggplot(df.fit.recharge, aes(x=method, y=log10(fit.prc), fill=topography)) +
  geom_hline(yintercept=c(0,1,2), color="gray65") +
  geom_boxplot(outlier.shape=21) +
  #  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=2, fill="red", alpha=0.5) +
  facet_wrap(~fit.metric, labeller=as_labeller(fit.labels)) +
  scale_x_discrete("Method") +
  scale_y_continuous("log(Fit) [%]") +
  scale_fill_manual(name="Topography", values=pal.recharge) +
  theme_scz() +
  theme(legend.position="bottom")
ggsave(paste0(dir.plot, "DepletionByWell_02_FitByReach_p.fit.recharge.png"),
       p.fit.recharge, width=8, height=6, units="in")

## comparing fit across different recharge values (figure 6 in Tom D report)
df.fit.recharge <- melt(subset(df.fit.all, drainage.density=="LD" & topography=="ELEV"), id=c("reach", "drainage.density", "topography", "recharge", "method"),
                    value.name = "fit.prc", variable.name="fit.metric")
p.fit.recharge <- 
  ggplot(df.fit.recharge, aes(x=method, y=log10(fit.prc), fill=recharge)) +
  geom_hline(yintercept=c(0,1,2), color="gray65") +
  geom_boxplot(outlier.shape=21) +
  #  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=2, fill="red", alpha=0.5) +
  facet_wrap(~fit.metric, labeller=as_labeller(fit.labels)) +
  scale_x_discrete("Method") +
  scale_y_continuous("log(Fit) [%]") +
  scale_fill_manual(name="Recharge", values=pal.recharge, labels=labels.recharge) +
  theme_scz() +
  theme(legend.position="bottom")
ggsave(paste0(dir.plot, "DepletionByWell_02_FitByReach_p.fit.recharge.png"),
       p.fit.recharge, width=8, height=6, units="in")
