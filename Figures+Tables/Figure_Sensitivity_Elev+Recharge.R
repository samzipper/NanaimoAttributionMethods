## Figure_Sensitivity_Elev+Recharge.R
#' Make figure showing performance as a function of length.
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

## plots
p.recharge.ByScenario.scatter <-
  ggplot(subset(df.prc, drainage.density=="LD" & topography=="ELEV" & recharge %in% c("NORCH", "RCH10", "RCH100", "RCH1000")), aes(x=depletion.prc, y=depletion.prc.modflow, color=method)) +
  geom_abline(slope=1, intercept=0, color="gray65") +
  geom_hline(yintercept=0, color="gray65") +
  geom_point(shape=21, alpha=0.5) +
  stat_smooth(method="lm", se=F) +
  facet_wrap(~recharge, ncol=4, scales="free",
             labeller=as_labeller(c("NORCH"="(a) 0 mm recharge", "RCH10"="(b) 10 mm recharge", 
                                    "RCH100"="(c) 100 mm recharge", "RCH1000"="(d) 1000 mm recharge"))) +
  scale_x_continuous(name="Analytical Depletion [%]", breaks=seq(0,100,25), limits=c(0,100)) +
  scale_y_continuous(name="MODFLOW Depletion [%]", 
                     limits=c(min(subset(df.prc, drainage.density=="LD" & topography=="ELEV")$depletion.prc.modflow), 
                              max(subset(df.prc, drainage.density=="LD" & topography=="ELEV")$depletion.prc.modflow))) +
  scale_color_manual(name="Method", values=pal.method) +
  theme_scz() +
  theme(legend.position="bottom",
        strip.background=element_blank())

p.recharge.depletion.diff.dens.noZeros <-
  ggplot(subset(df.prc, drainage.density=="LD" & topography=="ELEV" & recharge %in% c("NORCH", "RCH10", "RCH100", "RCH1000")), aes(x=depletion.diff.prc, fill=method, color=method)) +
  geom_density(alpha=0.2) +
  geom_vline(xintercept=0) +
  facet_wrap(~recharge, scales="free", ncol=4, labeller=as_labeller(c("NORCH"="(e) 0 mm recharge", "RCH10"="(f) 10 mm recharge", 
                                                                      "RCH100"="(g) 100 mm recharge", "RCH1000"="(h) 1000 mm recharge"))) +
  scale_x_continuous(name="Analytical - MODFLOW Depletion [%]") +
  scale_y_continuous(name="Density") +
  scale_fill_manual(name="Method", values=pal.method) +
  scale_color_manual(name="Method", values=pal.method) +
  theme_scz() +
  theme(legend.position="bottom",
        strip.background=element_blank())

p.recharge.fit.ByScenario.tern.facet <-
  ggtern(subset(df.fit.ByScenario, drainage.density=="LD" & topography=="ELEV" & recharge %in% c("NORCH", "RCH10", "RCH100", "RCH1000")), 
         aes(x=MSE.bias.norm, y=MSE.var.norm, z=MSE.cor.norm, size=KGE.overall, color=method)) +
  geom_point() +
  facet_wrap(~recharge, ncol=4, labeller=as_labeller(c("NORCH"="(i) 0 mm recharge", "RCH10"="(j) 10 mm recharge", 
                                                       "RCH100"="(k) 100 mm recharge", "RCH1000"="(l) 1000 mm recharge"))) +
  labs(x="% Bias", y="% Variability", z="% Correlation") +
  scale_color_manual(name="Method", values=pal.method, labels=labels.method) +
  scale_size_continuous(name="KGE", breaks=seq(0,0.5,0.25), limits=c(0,0.6)) +
#  theme_rgbw() +
  theme_custom(col.T = "darkred", 
               col.L = "darkblue", 
               col.R = "darkgreen") +
  theme(text=element_text(family="Arial", size=8, color="black"),
        axis.title=element_text(face="bold", size=rel(1)),
        axis.text=element_text(size=rel(1)),
        strip.text=element_text(size=rel(1)),
        tern.axis.title=element_blank(),
        tern.axis.arrow.text=element_text(face="bold", size=rel(1)),
        tern.axis.arrow.start=0.15,
        tern.axis.arrow.finish=0.85,
        tern.panel.grid.major=element_blank(),
        tern.panel.background = element_rect(fill = "white"), 
        tern.panel.grid.minor = element_line(color = "gray90"), 
        tern.axis.arrow.show = TRUE,
        legend.title=element_text(face="bold", size=rel(1)),
        legend.background=element_blank(),
        legend.text=element_text(size=rel(1)),
        legend.key=element_blank(),
        legend.position="bottom",
        legend.box="vertical",
        strip.background=element_blank()) +
  guides(size=guide_legend(reverse=F, order=2),
         color=guide_legend(order=1))

recharge.legend <- g_legend(p.recharge.fit.ByScenario.tern.facet + theme(legend.spacing.y=unit(-4, "mm")))

# align scatter and density plots
p1 <- ggplotGrob(p.recharge.ByScenario.scatter + guides(color="none") + 
                   theme(plot.margin=margin(0,1,0,0, "mm")))
p2 <- ggplotGrob(p.recharge.depletion.diff.dens.noZeros + guides(color="none", fill="none") + 
                   theme(plot.margin=margin(0,1,0,0, "mm")))
p <- rbind(p1, p2, size="first")
p$widths <- unit.pmax(p1$widths, p2$widths)

# save output
ggsave(paste0(dir.fig, "Figure_Sensitivity_Elev+Recharge.png"),
       ggtern::arrangeGrob(p, 
                           p.recharge.fit.ByScenario.tern.facet + guides(color="none", size="none") + 
                             theme(plot.margin=margin(-8,0,-5,10, "mm"),
                                   tern.axis.arrow.sep=0.15,
                                   tern.axis.vshift=0.1),
                           recharge.legend,
                           ncol=1, heights=c(3, 1.5, 0.4)),
       width=190, height=156, units="mm", dpi=300)

ggsave(paste0(dir.fig, "Figure_Sensitivity_Elev+Recharge.pdf"),
       ggtern::arrangeGrob(p, 
                           p.recharge.fit.ByScenario.tern.facet + guides(color="none", size="none") + 
                             theme(plot.margin=margin(-8,0,-5,10, "mm"),
                                   tern.axis.arrow.sep=0.15,
                                   tern.axis.vshift=0.1),
                           recharge.legend,
                           ncol=1, heights=c(3, 1.5, 0.4)),
       width=190, height=156, units="mm", device=cairo_pdf)
