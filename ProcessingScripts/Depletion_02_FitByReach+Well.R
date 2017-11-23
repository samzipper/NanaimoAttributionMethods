## Depletion_02_FitByReach+Well.R
#' This script is intended to calculate model fit for streamflow depletion results.
#' 
#' Fit is calculated in two ways:
#'   ByWell = for each well, MODFLOW and analytical depletion compared across all reaches
#'   ByReach = for each reach, MODFLOW and analytical depletion compared across all wells
#' 
#' This script requires output from Depletion_01_AggregateAllResults.R

rm(list=ls())

# directory to repository on local computer
dir.git <- "C:/Users/Sam/WorkGits/NanaimoAttributionMethods/"

# load paths + packages
source(paste0(dir.git, "ProcessingScripts/paths+packages.R"))

## percent threshold for inclusion: a well/reach combo will be eliminated if 
## both MODFLOW and analytical results are below this threshold
prc.thres <- 5  # Reeves et al. (2009) used 5%
                # if you want to use all data, set to -9999

## load data
df.all <- read.csv(paste0(dir.git, "data/Depletion_01_AggregateAllResults.csv"))

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
df$depletion.diff.prc <- df$depletion.prc - df$depletion.prc.modflow  # analytical - MODFLOW; positive means analytical overpreedicts

# remove well/reach combos not meeting percent depletion threshold
df.prc <- subset(df, depletion.prc > prc.thres & depletion.prc.modflow > prc.thres)

## calculate fit
# fit is calculated for each reach across all wells
df.fit.ByReach <- summarize(group_by(df.prc, reach, drainage.density, topography, recharge, method),
                            n.well = sum(is.finite(depletion.prc)),
                            MSE.bias.norm = MSE.bias.norm(depletion.prc, depletion.prc.modflow),
                            MSE.var.norm = MSE.var.norm(depletion.prc, depletion.prc.modflow),
                            MSE.cor.norm = MSE.cor.norm(depletion.prc, depletion.prc.modflow),
                            MSE.overall = MSE(depletion.prc, depletion.prc.modflow),
                            KGE.overall = KGE(depletion.prc, depletion.prc.modflow, method="2012"))

# fit is calcualted for each well across all reaches
df.fit.ByWell <- summarize(group_by(df.prc, well, drainage.density, topography, recharge, method),
                           n.reach = sum(is.finite(depletion.prc)),
                           MSE.bias.norm = MSE.bias.norm(depletion.prc, depletion.prc.modflow),
                           MSE.var.norm = MSE.var.norm(depletion.prc, depletion.prc.modflow),
                           MSE.cor.norm = MSE.cor.norm(depletion.prc, depletion.prc.modflow),
                           MSE.overall = MSE(depletion.prc, depletion.prc.modflow),
                           KGE.overall = KGE(depletion.prc, depletion.prc.modflow, method="2012"))

# fit is calculated for each scenario based on all reach + well combos
df.fit.ByScenario <- summarize(group_by(df.prc, drainage.density, topography, recharge, method),
                               n.reach = sum(is.finite(depletion.prc)),
                               MSE.bias.norm = MSE.bias.norm(depletion.prc, depletion.prc.modflow),
                               MSE.var.norm = MSE.var.norm(depletion.prc, depletion.prc.modflow),
                               MSE.cor.norm = MSE.cor.norm(depletion.prc, depletion.prc.modflow),
                               MSE.overall = MSE(depletion.prc, depletion.prc.modflow),
                               KGE.overall = KGE(depletion.prc, depletion.prc.modflow, method="2012"))

## plots
# overview of data
p.depletion.reach.well <-
  ggplot(subset(df.all, topography=="FLAT" & recharge=="NORCH"), aes(x=reach, y=well, fill=depletion.prc)) +
  geom_raster() +
  facet_grid(drainage.density ~ method, scales="free") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_scz()
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.depletion.reach.well.png"),
       p.depletion.reach.well, width=8, height=4, units="in")

# performance at each individual reach-well combo
p.depletion.diff.reach.well <-
  ggplot(subset(df, topography=="FLAT" & recharge=="NORCH"), aes(x=reach, y=well, fill=depletion.diff.prc)) +
  geom_raster() +
  facet_grid(drainage.density ~ method, scales="free") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_gradient2() +
  theme_scz()
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.depletion.diff.reach.well.png"),
       p.depletion.diff.reach.well, width=8, height=4, units="in")

p.depletion.diff.hist <-
  ggplot(subset(df, topography=="FLAT" & recharge=="NORCH"), aes(x=depletion.diff.prc, fill=method, color=method)) +
  geom_histogram(alpha=0.2, binwidth=5, position="dodge") +
  facet_wrap(~drainage.density, scales="free_y") +
  scale_fill_discrete() +
  theme_scz()
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.depletion.diff.hist.png"),
       p.depletion.diff.hist, width=8, height=4, units="in")

p.depletion.diff.dens.noZeros <-
  ggplot(subset(df.prc, topography=="FLAT" & recharge=="NORCH"), aes(x=depletion.diff.prc, fill=method, color=method)) +
  geom_density(alpha=0.2) +
  geom_vline(xintercept=0) +
  facet_wrap(~drainage.density, scales="free_y") +
  scale_x_continuous(name="Analytical - MODFLOW [% of Total Depletion]") +
  scale_fill_manual(values=pal.method) +
  scale_color_manual(values=pal.method) +
  theme_scz() +
  theme(legend.position="bottom")
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.depletion.diff.dens.noZeros.png"),
       p.depletion.diff.dens.noZeros, width=8, height=4, units="in")

p.fit.ByScenario.tern <-
  ggtern(subset(df.fit.ByScenario, recharge=="NORCH" & topography=="FLAT"), 
         aes(x=MSE.bias.norm, y=MSE.var.norm, z=MSE.cor.norm, size=-KGE.overall, color=method, shape=drainage.density)) +
  geom_point(alpha=0.75) +
  labs(x="% MSE due to Bias", y="% MSE due to Variability", z="% MSE due to Correlation") +
  scale_color_manual(values=pal.method) +
  theme_rgbw() +
  theme(tern.axis.title=element_blank(),
        tern.panel.grid.major=element_blank())
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.fit.ByScenario.tern.png"),
       p.fit.ByScenario.tern, width=6, height=4, units="in")

p.fit.ByScenario.tern.facet <-
  ggtern(subset(df.fit.ByScenario, recharge=="NORCH" & topography=="FLAT"), 
         aes(x=MSE.bias.norm, y=MSE.var.norm, z=MSE.cor.norm, size=-KGE.overall, color=method)) +
  geom_point() +
  facet_wrap(~drainage.density) +
  labs(x="% MSE due to Bias", y="% MSE due to Variability", z="% MSE due to Correlation") +
  scale_color_manual(values=pal.method) +
  theme_rgbw() +
  theme(tern.axis.title=element_blank(),
        tern.panel.grid.major=element_blank(),
        legend.position="bottom")
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.fit.ByScenario.tern.facet.png"),
       p.fit.ByScenario.tern.facet, width=10, height=4, units="in")

# fit by well
p.fit.ByWell.tern <-
  ggtern(subset(df.fit.ByWell, drainage.density=="LD" & recharge=="NORCH" & method=="WEBLINSQ" & n.reach>2), 
         aes(x=MSE.bias.norm, y=MSE.var.norm, z=MSE.cor.norm, size=-KGE.overall, shape=factor(n.reach))) +
  geom_point() +
  labs(x="% MSE due to Bias", y="% MSE due to Variability", z="% MSE due to Correlation") +
  theme_rgbw() +
  theme(tern.axis.title=element_blank())

# fit by scenario
p.ByScenario.scatter <-
  ggplot(subset(df.prc, topography=="FLAT" & recharge=="NORCH"), aes(x=depletion.prc, y=depletion.prc.modflow, color=method)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(shape=21) +
  stat_smooth(method="lm") +
  facet_wrap(~drainage.density) +
  scale_x_continuous(name="Analytical Depletion [% of Total Depletion]", breaks=seq(0,100,25)) +
  scale_y_continuous(name="MODFLOW Depletion [% of Total Depletion]", breaks=seq(0,100,25)) +
  scale_color_manual(values=pal.method) +
  theme_scz() +
  theme(legend.position="bottom")
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.ByScenario.scatter.png"),
       p.ByScenario.scatter, width=8, height=4, units="in")

## comparing fit across different drainage densities (figure 4 in Tom D report)
df.fit.density <- melt(subset(df.fit.ByReach, topography=="FLAT" & recharge=="NORCH"), id=c("reach", "drainage.density", "topography", "recharge", "method"),
                       value.name = "fit.prc", variable.name="fit.metric")
p.fit.density <- 
  ggplot(df.fit.density, aes(x=method, y=log10(fit.prc), fill=drainage.density)) +
  geom_hline(yintercept=c(0,1,2), color="gray65") +
  geom_boxplot(outlier.shape=21) +
  #  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=2, fill="red", alpha=0.5) +
  facet_wrap(~fit.metric) +
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
df.fit.recharge <- melt(subset(df.fit.ByReach, drainage.density=="LD" & topography=="ELEV"), id=c("reach", "drainage.density", "topography", "recharge", "method"),
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

