## Figure_Baseline_Ternary.R
#' Ternary plot showing causes for mismatch between analytical and MODFLOW.
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
# ternary plots
p.fit.ByScenario.tern <-
  ggtern(subset(df.fit.ByScenario, recharge=="NORCH" & topography=="FLAT"), 
         aes(x=MSE.bias.norm, y=MSE.var.norm, z=MSE.cor.norm, size=-KGE.overall, color=method, shape=drainage.density)) +
  geom_point(alpha=0.75) +
  labs(x="% MSE due to Bias", y="% MSE due to Variability", z="% MSE due to Correlation") +
  scale_color_manual(name=NULL, values=pal.method, labels=labels.method) +
  scale_shape_discrete(name="Drainage Density", labels=labels.density) +
  scale_size_continuous(name="KGE", breaks=seq(-0.6, 0, 0.2), labels=c("0.6", "0.4", "0.2", "0.0")) +
  theme_rgbw(base_size=8, base_family="Arial") +
  theme(text=element_text(color="black"),
        axis.title=element_text(face="bold", size=rel(1)),
        axis.text=element_text(size=rel(1)),
        tern.axis.title=element_blank(),
        tern.panel.grid.major=element_blank(),
        legend.position="bottom",
        legend.box="vertical",
        legend.box.margin=margin(-3,0,0,0, "mm"),
        legend.background=element_blank(),
        legend.title=element_text(face="bold", size=rel(1)),
        legend.text=element_text(size=rel(1)),
        legend.spacing.y=unit(-4, "mm"),
        legend.key=element_blank(),
        legend.text.align=0,
        tern.axis.arrow.sep=0.085,
        tern.axis.arrow.start=0.15,
        tern.axis.arrow.finish=0.85,
        tern.axis.arrow.text=element_text(face="bold", size=rel(1)),
        plot.margin=unit(c(-8,-8,0,-8), "mm")) +
  guides(size=guide_legend(reverse=T, order=3),
         color=guide_legend(order=1),
         shape=guide_legend(order=2))

ggsave(paste0(dir.fig, "Figure_Baseline_Ternary.pdf"), 
       p.fit.ByScenario.tern, 
       width=95, height=95, units="mm", device=cairo_pdf)

ggsave(paste0(dir.fig, "Figure_Baseline_Ternary.png"), 
       p.fit.ByScenario.tern, 
       width=95, height=95, units="mm", dpi=300)

subset(df.fit.ByScenario, topography=="FLAT" & method=="WEBLINSQ")
subset(df.fit.ByScenario, topography=="FLAT" & method=="IDLINSQ")
subset(df.fit.ByScenario, topography=="FLAT" & method=="THIESSEN")
