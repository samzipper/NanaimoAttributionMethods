## Figure_Baseline_Scatter+Dens.R
#' Scatter and density plots comparing MODFLOW and analytical results.
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

# extract MODFLOW data and make it a column of its own in 'df' data frame
df.mod <- subset(df.all, method=="MODFLOW")
df <- subset(df.all, method != "MODFLOW")
colnames(df.mod)[colnames(df.mod)=="depletion.prc"] <- "depletion.prc.modflow"
df.mod$method <- NULL
df <- left_join(df, df.mod)
df$depletion.diff.prc <- df$depletion.prc - df$depletion.prc.modflow  # analytical - MODFLOW; positive means analytical overpreedicts

# set factor levels
df$drainage.density <- factor(df$drainage.density, levels=c("HD", "MD", "LD"))
df$topography <- factor(df$topography, levels=c("FLAT", "ELEV"))
df$recharge <- factor(df$recharge, levels=c("NORCH", "RCH10", "RCH50", "RCH100", "RCH500", "RCH1000"))
df$method <- factor(df$method, levels=c("MODFLOW", "THIESSEN", "IDLIN", "IDLINSQ", "WEBLIN", "WEBLINSQ"))

# remove well/reach combos not meeting percent depletion threshold
df.prc <- subset(df, abs(depletion.prc) > prc.thres | abs(depletion.prc.modflow) > prc.thres)

## calculate fit
# fit is calculated for each scenario based on all reach + well combos
df.fit.ByScenario <- summarize(group_by(df.prc, drainage.density, topography, recharge, method),
                               n.reach = sum(is.finite(depletion.prc)),
                               cor = cor(depletion.prc, depletion.prc.modflow, method="pearson"),
                               R2 = R2(depletion.prc.modflow,depletion.prc),
                               MSE.bias.norm = MSE.bias.norm(depletion.prc, depletion.prc.modflow),
                               MSE.var.norm = MSE.var.norm(depletion.prc, depletion.prc.modflow),
                               MSE.cor.norm = MSE.cor.norm(depletion.prc, depletion.prc.modflow),
                               MSE.overall = MSE(depletion.prc, depletion.prc.modflow),
                               KGE.overall = KGE(depletion.prc, depletion.prc.modflow, method="2012"))

## table
df.fit.table <- dcast(df.fit.ByScenario[,c("drainage.density", "topography", "recharge", "method", "KGE.overall")],
                      drainage.density + topography + recharge ~ method, value.var="KGE.overall")
df.fit.table[,4:8] <- round(df.fit.table[,4:8], 3)

## plots
# scatterplot and density plot comparing all methods for each drainage density
p.ByScenario.scatter <-
  ggplot(subset(df.prc, topography=="FLAT" & recharge=="NORCH"), aes(x=depletion.prc, y=depletion.prc.modflow, color=method)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(shape=21, alpha=0.5) +
  stat_smooth(method="lm", se=F) +
  facet_wrap(~drainage.density, ncol=3, scales="free") +
  scale_x_continuous(name="Analytical Depletion [% of Total Depletion]", 
                     breaks=seq(0,100,25), limits=c(0,100), expand=c(0,0)) +
  scale_y_continuous(name="MODFLOW Depletion [% of Total Depletion]", 
                     breaks=seq(0,100,25), limits=c(0,100), expand=c(0,0)) +
  scale_color_manual(values=pal.method, guide=F) +
  theme_scz() +
  theme(legend.position="bottom",
        panel.border=element_rect(color="black"))

p.ByScenario.dens <-
  ggplot(subset(df.prc, topography=="FLAT" & recharge=="NORCH"), aes(x=depletion.diff.prc, fill=method, color=method)) +
  geom_vline(xintercept=0) +
  geom_density(alpha=0.2) +
  facet_wrap(~drainage.density, scales="free_y", ncol=3) +
  scale_x_continuous(name="Analytical - MODFLOW [% of Total Depletion]") +
  scale_y_continuous(limits=c(0,0.055), breaks=seq(0,0.05,0.01), expand=c(0,0)) +
  scale_fill_manual(values=pal.method, guide=F) +
  scale_color_manual(values=pal.method, guide=F) +
  theme_scz() +
  theme(legend.position="bottom",
        panel.border=element_rect(color="black"))

pdf(paste0(dir.fig, "Figure_Baseline_Scatter+Dens.pdf"), width=(183/25.4), height=(100/25.4))
grid.arrange(p.ByScenario.scatter + theme(text=element_blank(), plot.margin=unit(c(0.5,0.5,0,8), "mm")), 
             p.ByScenario.dens + theme(text=element_blank(), plot.margin=unit(c(7,0.5,0,8), "mm")), 
             ncol=1)
dev.off()

# linear fits in scatterplot
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="THIESSEN" & drainage.density=="HD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="IDLIN" & drainage.density=="HD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="IDLINSQ" & drainage.density=="HD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="WEBLIN" & drainage.density=="HD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="WEBLINSQ" & drainage.density=="HD" & topography=="FLAT" & recharge=="NORCH")))

summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="THIESSEN" & drainage.density=="MD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="IDLIN" & drainage.density=="MD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="IDLINSQ" & drainage.density=="MD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="WEBLIN" & drainage.density=="MD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="WEBLINSQ" & drainage.density=="MD" & topography=="FLAT" & recharge=="NORCH")))

summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="THIESSEN" & drainage.density=="LD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="IDLIN" & drainage.density=="LD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="IDLINSQ" & drainage.density=="LD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="WEBLIN" & drainage.density=="LD" & topography=="FLAT" & recharge=="NORCH")))
summary(lm(depletion.prc.modflow ~ depletion.prc, data=subset(df.prc, method=="WEBLINSQ" & drainage.density=="LD" & topography=="FLAT" & recharge=="NORCH")))
