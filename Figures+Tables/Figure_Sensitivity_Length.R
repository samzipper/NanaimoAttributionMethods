## Figure_Sensitivity_Length.R
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

# make a wide-format version
df.wide.LD <- dcast(subset(df.all, topography=="FLAT" & drainage.density=="LD"), drainage.density+well+reach+topography+recharge~method, value.var="depletion.prc")
df.wide.MD <- dcast(subset(df.all, topography=="FLAT" & drainage.density=="MD"), drainage.density+well+reach+topography+recharge~method, value.var="depletion.prc")
df.wide.HD <- dcast(subset(df.all, topography=="FLAT" & drainage.density=="HD"), drainage.density+well+reach+topography+recharge~method, value.var="depletion.prc")

# check depletion to make sure things sum to 100
df.depletion.byWell <- dplyr::summarize(group_by(df.all, well, drainage.density, topography, recharge, method),
                                        depletion.prc.sum = sum(depletion.prc))
# wells near edges often don't sum to 100, especially for Thiessen and MODFLOW methods

# load and prep map data
## load and prep data
# load data
shp.LD.boundary <- readOGR(paste0(dir.GSAS, "GIS"), layer="LD_boundary_aquifer")
shp.LD.streams <- readOGR(paste0(dir.GSAS, "GIS"), layer="LD_streams")
df.LD.wells <- read.csv(paste0(dir.GSAS, "GIS/LD_wells_info.csv"), stringsAsFactors=F)

shp.MD.boundary <- readOGR(paste0(dir.GSAS, "GIS"), layer="MD_boundary_aquifer")
shp.MD.streams <- readOGR(paste0(dir.GSAS, "GIS"), layer="MD_streams")
shp.MD.wells <- readOGR(paste0(dir.GSAS, "GIS"), layer="MD_wells")

shp.HD.boundary <- readOGR(paste0(dir.GSAS, "GIS"), layer="HD_boundary_aquifer")
shp.HD.streams <- readOGR(paste0(dir.GSAS, "GIS"), layer="HD_streams")
df.HD.wells <- read.csv(paste0(dir.GSAS, "GIS/HD_wells_info.csv"), stringsAsFactors=F)

# tidy spatial data for ggplot
df.LD.boundary <- tidy(shp.LD.boundary)
df.LD.streams <- tidy(shp.LD.streams)

df.MD.boundary <- tidy(shp.MD.boundary)
df.MD.streams <- tidy(shp.MD.streams)
df.MD.wells <- as.data.frame(shp.MD.wells)

df.HD.boundary <- tidy(shp.HD.boundary)
df.HD.streams <- tidy(shp.HD.streams)

# set up reach and well columns to match df.all
df.LD.wells$well <- as.numeric(substring(df.LD.wells$Well_Name, 8))
df.LD.streams$reach <- as.numeric(df.LD.streams$id)+2

df.MD.wells$well <- as.numeric(substring(df.MD.wells$Well_Name, 8))
df.MD.streams$reach <- as.numeric(df.MD.streams$id)+2

df.HD.wells$well <- as.numeric(substring(df.HD.wells$Well_Name, 8))
df.HD.streams$reach <- as.numeric(df.HD.streams$id)+2

# extract stream length for each reach
df.LD.stream.length <- data.frame(reach = seq(2,63),
                                  length.km = shp.LD.streams@data$Length,
                                  drainage.density="LD")
df.LD.stream.length$length.km.tercile <- cut(df.LD.stream.length$length.km, 
                                             quantile(df.LD.stream.length$length.km, c(0, 0.33, 0.66, 1)),
                                             labels=c("shortest", "middle", "longest"), include.lowest=T)
df.LD.stream.length$length.km.quartile <- cut(df.LD.stream.length$length.km, 
                                              quantile(df.LD.stream.length$length.km, c(0, 0.25, 0.5, 0.75, 1)),
                                              labels=c("Q1", "Q2", "Q3", "Q4"), include.lowest=T)

df.MD.stream.length <- data.frame(reach = seq(2,63),
                                  length.km = shp.MD.streams@data$STE_LE,
                                  drainage.density="MD")
df.MD.stream.length$length.km.tercile <- cut(df.MD.stream.length$length.km, 
                                             quantile(df.MD.stream.length$length.km, c(0, 0.33, 0.66, 1)),
                                             labels=c("shortest", "middle", "longest"), include.lowest=T)
df.MD.stream.length$length.km.quartile <- cut(df.MD.stream.length$length.km, 
                                              quantile(df.MD.stream.length$length.km, c(0, 0.25, 0.5, 0.75, 1)),
                                              labels=c("Q1", "Q2", "Q3", "Q4"), include.lowest=T)

df.HD.stream.length <- data.frame(reach = seq(2,63),
                                  length.km = shp.HD.streams@data$Length,
                                  drainage.density="HD")
df.HD.stream.length$length.km.tercile <- cut(df.HD.stream.length$length.km, 
                                             quantile(df.HD.stream.length$length.km, c(0, 0.33, 0.66, 1)),
                                             labels=c("shortest", "middle", "longest"), include.lowest=T)
df.HD.stream.length$length.km.quartile <- cut(df.HD.stream.length$length.km, 
                                              quantile(df.HD.stream.length$length.km, c(0, 0.25, 0.5, 0.75, 1)),
                                              labels=c("Q1", "Q2", "Q3", "Q4"), include.lowest=T)

# extract MODFLOW data and make it a column of its own in 'df' data frame
df.mod <- subset(df.all, method=="MODFLOW")
df <- subset(df.all, method != "MODFLOW")
colnames(df.mod)[colnames(df.mod)=="depletion.prc"] <- "depletion.prc.modflow"
df.mod$method <- NULL
df <- left_join(df, df.mod)
df$depletion.diff.prc <- df$depletion.prc - df$depletion.prc.modflow  # analytical - MODFLOW; positive means analytical overpreedicts

# add reach length to df
df <- left_join(df, rbind(df.LD.stream.length, df.MD.stream.length, df.HD.stream.length), by=c("reach", "drainage.density"))

# set factor levels
df$drainage.density <- factor(df$drainage.density, levels=c("HD", "MD", "LD"))
df$topography <- factor(df$topography, levels=c("FLAT", "ELEV"))
df$recharge <- factor(df$recharge, levels=c("NORCH", "RCH10", "RCH50", "RCH100", "RCH500", "RCH1000"))
df$method <- factor(df$method, levels=c("MODFLOW", "THIESSEN", "IDLIN", "IDLINSQ", "WEBLIN", "WEBLINSQ"))

# remove well/reach combos not meeting percent depletion threshold
df.prc <- subset(df, abs(depletion.prc) > prc.thres | abs(depletion.prc.modflow) > prc.thres)

## reach length comparison
# fit is calculated for each stream segment length based on all reach + well combos
df.length.quartiles <- rbind(df.LD.stream.length, df.MD.stream.length, df.HD.stream.length)
df.length.quartiles$drainage.density <- factor(df.length.quartiles$drainage.density, levels=c("HD", "MD", "LD"))
df.prc$length.km.class <- cut(df.prc$length.km, c(1e-6, 1e-1, 5e-1, 1, 5, 1e2), labels=c("<0.1", "0.1-0.5", "0.5-1", "1-5", ">5"), include.lowest=T)

df.fit.ByLength.class <- summarize(group_by(subset(df.prc, topography=="FLAT"), method, length.km.class),
                                      n.reach = sum(is.finite(depletion.prc)),
                                      KGE.overall = KGE(depletion.prc, depletion.prc.modflow, method="2012"))
p.fit.length.class <-
  ggplot(df.fit.ByLength.class, aes(x=method, y=KGE.overall, fill=length.km.class)) +
  geom_bar(stat="identity", position="dodge") + 
  geom_vline(xintercept=seq(1.5,4.5,1), color="gray85") +
  geom_hline(yintercept=0, color="gray65") +
  scale_fill_manual(name="Reach\nLength\n[km]", values=pal.length.class) +
  scale_x_discrete(name="Method", labels=labels.method) +
  scale_y_continuous(name="KGE", limits=c(-2,1), breaks=seq(-2,1,1), expand=c(0,0)) +
  theme_scz() +
  theme(plot.margin=unit(c(1, 0.5, 0.5, 0.5), "mm"),
        legend.background=element_blank(),
        legend.box.margin=unit(c(0.5, 0.5, 0.5, -3), "mm"),
        legend.margin=unit(c(0.5, 0.5, 0.5, 0.5), "mm"))

ggsave(paste0(dir.fig, "Figure_Sensitivity_Length_Class.pdf"),
       p.fit.length.class,
       width=95, height=60, units="mm", device=cairo_pdf)

ggsave(paste0(dir.fig, "Figure_Sensitivity_Length_Class.png"),
       p.fit.length.class,
       width=95, height=60, units="mm", dpi=300)

## For SI: plot by quartile
df.fit.ByLength.quartile <- summarize(group_by(subset(df.prc, topography=="FLAT"), drainage.density, topography, recharge, method, length.km.quartile),
                             n.reach = sum(is.finite(depletion.prc)),
                             cor = cor(depletion.prc, depletion.prc.modflow, method="pearson"),
                             R2 = R2(depletion.prc.modflow,depletion.prc),
                             MSE.bias.norm = MSE.bias.norm(depletion.prc, depletion.prc.modflow),
                             MSE.var.norm = MSE.var.norm(depletion.prc, depletion.prc.modflow),
                             MSE.cor.norm = MSE.cor.norm(depletion.prc, depletion.prc.modflow),
                             MSE.overall = MSE(depletion.prc, depletion.prc.modflow),
                             KGE.overall = KGE(depletion.prc, depletion.prc.modflow, method="2012"))
# set up for ecdf
df.length.HD <- subset(df.length.quartiles, drainage.density=="HD")
df.length.HD$ecdf <- ecdf(df.length.HD$length.km)(df.length.HD$length.km)

df.length.MD <- subset(df.length.quartiles, drainage.density=="MD")
df.length.MD$ecdf <- ecdf(df.length.MD$length.km)(df.length.MD$length.km)

df.length.LD <- subset(df.length.quartiles, drainage.density=="LD")
df.length.LD$ecdf <- ecdf(df.length.LD$length.km)(df.length.LD$length.km)

df.length.ecdf <- rbind(df.length.HD, df.length.MD, df.length.LD)
df.length.ecdf$drainage.density <- factor(df.length.ecdf$drainage.density, levels=c("HD", "MD", "LD"))

p.length.fit.box <-
  ggplot(subset(df.fit.ByLength.quartile, !(method %in% c("IDLIN", "WEBLIN"))), aes(x=method, fill=length.km.quartile, y=KGE.overall)) +
  geom_bar(stat="identity", position="dodge") +
  geom_vline(xintercept=c(1.5, 2.5), color="gray85") +
  geom_hline(yintercept=0, color="gray65") +
  facet_wrap(~drainage.density, scales="free", ncol=1) +
  scale_y_continuous(name="KGE", limits=c(min(subset(df.fit.ByLength.quartile, !(method %in% c("IDLIN", "WEBLIN")))$KGE.overall), 
                                          max(subset(df.fit.ByLength.quartile, !(method %in% c("IDLIN", "WEBLIN")))$KGE.overall))) +
  scale_x_discrete(name="Method", labels=labels.method.oneline) +
  scale_fill_manual(name="Length Quartile", labels=labels.quartile, values=pal.quartile) +
  theme_scz() +
  theme(legend.position="bottom",
        strip.background=element_blank(),
        strip.text=element_blank())

p.length.ecdf <-
  ggplot() +
  #geom_hline(yintercept=0, color="gray65") +
  geom_ribbon(data=df.length.ecdf, aes(x=length.km, ymin=0, ymax=ecdf, color=length.km.quartile, fill=length.km.quartile)) +
  geom_line(data=df.length.ecdf, aes(x=length.km, y=ecdf)) +
  facet_wrap(~drainage.density, scales="free", ncol=1) +
  scale_x_log10(name="Reach Length [km]", expand=c(0,0), breaks=c(1e-4, 1e-2, 1e0),
                limits=c(min(df.length.ecdf$length.km), max(df.length.ecdf$length.km))) +
  scale_y_continuous(name="ECDF", expand=c(0,0), limits=c(0,1), breaks=seq(0,1,0.25), labels=c("0.0", "", "0.5", "", "1.0")) +
  scale_fill_manual(name="Length Quartile", labels=labels.quartile, values=pal.quartile) +
  scale_color_manual(name="Length Quartile", labels=labels.quartile, values=pal.quartile) +
  theme_scz() +
  theme(legend.position="bottom",
        strip.background=element_blank(),
        strip.text=element_blank())

length.legend <- g_legend(p.length.fit.box + guides(fill=guide_legend(keywidth=1, keyheight=1)))

ggsave(paste0(dir.fig, "SI_Figure_Sensitivity_Length_Quartile_NoLabels.pdf"),
       grid.arrange(
         arrangeGrob(p.length.fit.box + guides(fill="none") + theme(plot.margin=unit(c(1,2,0,0), "mm")), 
                     p.length.ecdf + guides(fill="none", color="none") + theme(plot.margin=unit(c(1,0.5,0,0), "mm")), 
                     ncol=2), 
         length.legend, nrow=2, heights=c(12,1)),
       width=95, height=126, units="mm", device=cairo_pdf)

## For SI: ternary plot
p.length.fit.tern <-
  ggtern(subset(df.fit.ByLength.quartile, !(method %in% c("IDLIN", "WEBLIN"))), 
         aes(x=MSE.bias.norm, y=MSE.var.norm, z=MSE.cor.norm, size=-KGE.overall, color=length.km.quartile)) +
  geom_point(alpha=0.9) +
  facet_grid(method~drainage.density, switch="y", labeller=as_labeller(c(labels.method, labels.density.long))) +
  labs(x="% Bias", y="% Variability", z="% Correlation") +
  scale_color_manual(name="Length Quartile", labels=labels.quartile, values=pal.quartile) +
  scale_size_continuous(name="KGE", breaks=seq(-0.5, 0.5, 0.5), labels=c("0.5", "0", "-0.5")) +
  theme_rgbw(base_size=8, base_family="Arial") +
  theme(text=element_text(family="Arial", size=8, color="black"),
        axis.title=element_text(face="bold", size=rel(1)),
        axis.text=element_text(size=rel(1)),
        strip.text=element_text(size=rel(1)),
        tern.axis.title=element_blank(),
        tern.panel.grid.major=element_blank(),
        legend.position="bottom",
        legend.background=element_blank(),
        legend.title=element_text(face="bold", size=rel(1)),
        legend.text=element_text(size=rel(1)),
        legend.key=element_blank(),
        #tern.axis.arrow.sep=0.075,
        tern.axis.arrow.start=0.2,
        tern.axis.arrow.finish=0.85,
        tern.axis.arrow.text=element_text(face="bold", size=rel(1))) +
  guides(size=guide_legend(reverse=T, order=2),
         color=guide_legend(order=1))

ggsave(paste0(dir.fig, "SI_Figure_Sensitivity_Length_Ternary.png"), p.length.fit.tern, width=190, height=170, units="mm")
ggsave(paste0(dir.fig, "SI_Figure_Sensitivity_Length_Ternary.pdf"), p.length.fit.tern, width=190, height=170, units="mm", device=cairo_pdf)
