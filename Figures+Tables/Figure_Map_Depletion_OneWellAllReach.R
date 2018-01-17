## Figure_Map_Depletino_OneWellAllReach.R
#' Figure showing depletion across all reaches for a single pumping well.
#' 
#' Must be connected to GSAS server.

rm(list=ls())

# directory to repository on local computer
dir.git <- "C:/Users/Sam/WorkGits/NanaimoAttributionMethods/"

# load paths + packages
source(paste0(dir.git, "ProcessingScripts/paths+packages.R"))

## load data
df.all <- read.csv(paste0(dir.git, "data/Depletion_01_AggregateAllResults.csv"))

## load and prep data
# load data
shp.LD.boundary <- readOGR(paste0(dir.GSAS, "GIS"), layer="LD_boundary_aquifer")
shp.LD.inset <- readOGR(paste0(dir.GSAS, "GIS"), layer="LD_boundary_inset")
shp.LD.streams <- readOGR(paste0(dir.GSAS, "GIS"), layer="LD_streams")
df.LD.wells <- read.csv(paste0(dir.GSAS, "GIS/LD_wells_info.csv"), stringsAsFactors=F)

shp.MD.boundary <- readOGR(paste0(dir.GSAS, "GIS"), layer="MD_boundary_aquifer")
shp.MD.inset <- readOGR(paste0(dir.GSAS, "GIS"), layer="MD_boundary_inset")
shp.MD.streams <- readOGR(paste0(dir.GSAS, "GIS"), layer="MD_streams")
shp.MD.wells <- readOGR(paste0(dir.GSAS, "GIS"), layer="MD_wells")

shp.HD.boundary <- readOGR(paste0(dir.GSAS, "GIS"), layer="HD_boundary_aquifer")
shp.HD.inset <- readOGR(paste0(dir.GSAS, "GIS"), layer="HD_boundary_inset")
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

##### make maps #####
### make maps of reach depletion for a given well
# low density
# choose a well
LD.well.n <- 20

# subset depletion data for that well
df.LD.depletion.well <- subset(df.all, topography=="FLAT" & recharge=="NORCH" & drainage.density=="LD" & well==LD.well.n)
df.LD.streams.all <- inner_join(df.LD.streams, df.LD.depletion.well, by="reach")
df.LD.streams.all$depletion.prc.class <- cut(df.LD.streams.all$depletion.prc,
                                             breaks=c(c(0,5,10,15,20,100)),
                                             labels=c(" 0-5% ", " 5-10%", "10-15%", "15-20%", "> 20%"),
                                             include.lowest=T)
df.LD.streams.all$method <- factor(df.LD.streams.all$method, levels=c("MODFLOW", "THIESSEN", "IDLIN", "IDLINSQ", "WEBLIN", "WEBLINSQ"))

## medium density
# choose a well
MD.well.n <- 23

# subset depletion data for that well
df.MD.depletion.well <- subset(df.all, topography=="FLAT" & recharge=="NORCH" & drainage.density=="MD" & well==MD.well.n)
df.MD.streams.all <- inner_join(df.MD.streams, df.MD.depletion.well, by="reach")
df.MD.streams.all$depletion.prc.class <- cut(df.MD.streams.all$depletion.prc,
                                             breaks=c(c(0,5,10,15,20,100)),
                                             labels=c(" 0-5% ", " 5-10%", "10-15%", "15-20%", "> 20%"),
                                             include.lowest=T)
df.MD.streams.all$method <- factor(df.MD.streams.all$method, levels=c("MODFLOW", "THIESSEN", "IDLIN", "IDLINSQ", "WEBLIN", "WEBLINSQ"))

## low density
# choose a well
HD.well.n <- 23

# subset depletion data for that well
df.HD.depletion.well <- subset(df.all, topography=="FLAT" & recharge=="NORCH" & drainage.density=="HD" & well==HD.well.n)
df.HD.streams.all <- inner_join(df.HD.streams, df.HD.depletion.well, by="reach")
df.HD.streams.all$depletion.prc.class <- cut(df.HD.streams.all$depletion.prc,
                                             breaks=c(c(0,5,10,15,20,100)),
                                             labels=c(" 0-5% ", " 5-10%", "10-15%", "15-20%", "> 20%"),
                                             include.lowest=T)
df.HD.streams.all$method <- factor(df.HD.streams.all$method, levels=c("MODFLOW", "THIESSEN", "IDLIN", "IDLINSQ", "WEBLIN", "WEBLINSQ"))

## make plots
p.HD.byWell <-
  ggplot(data=df.HD.streams.all, aes(x=long, y=lat)) +
  facet_wrap(~method, ncol=6, 
             labeller=as_labeller(c("MODFLOW"="MODFLOW", labels.method))) +
  geom_polygon(data=df.HD.boundary, aes(x=long, y=lat), color="black", fill=NA) +
  #  geom_point(data=df.HD.wells, aes(x=X.world_coord.m., y=Y.world_coord.m.), color="black", shape=21) +
  geom_path(aes(group=group, color=depletion.prc.class), size=1) +
  geom_point(data=subset(df.HD.wells, well==HD.well.n), aes(x=X.world_coord.m., y=Y.world_coord.m.), shape=21, fill="red", size=2) +
  scale_color_manual(name="Depletion [%]", values=pal.depletion, drop=F) +
  coord_equal() +
  theme_scz() +
  theme(legend.position="bottom")

p.MD.byWell <-
  ggplot(data=df.MD.streams.all, aes(x=long, y=lat)) +
  facet_wrap(~method, ncol=6) +
  geom_polygon(data=df.MD.boundary, aes(x=long, y=lat), color="black", fill=NA) +
  #  geom_point(data=df.MD.wells, aes(x=X_world_co, y=Y_world_co), color="black", shape=21) +
  geom_path(aes(group=group, color=depletion.prc.class), size=1) +
  geom_point(data=subset(df.MD.wells, well==MD.well.n), aes(x=X_world_co, y=Y_world_co), shape=21, fill="red", size=2) +
  scale_color_manual(name="Depletion [%]", values=pal.depletion, drop=F) +
  coord_equal() +
  theme_scz() +
  theme(legend.position="bottom")

p.LD.byWell <-
  ggplot(data=df.LD.streams.all, aes(x=long, y=lat)) +
  facet_wrap(~method, ncol=6) +
  geom_polygon(data=df.LD.boundary, aes(x=long, y=lat), color="black", fill=NA) +
  #  geom_point(data=df.LD.wells, aes(x=X.world_coord.m., y=Y.world_coord.m.), color="black", shape=21) +
  geom_path(aes(group=group, color=depletion.prc.class), size=1) +
  geom_point(data=subset(df.LD.wells, well==LD.well.n), aes(x=X.world_coord.m., y=Y.world_coord.m.), shape=21, fill="red", size=2) +
  scale_color_manual(name="Depletion [%]", values=pal.depletion, drop=F) +
  coord_equal() +
  theme_scz() +
  theme(legend.position="bottom")

# align plots and fine-tune themes
p1 <- ggplotGrob(p.HD.byWell + guides(color="none") + 
                   theme(plot.margin=margin(0,0,0,0, "mm"),
                         strip.background=element_blank(),
                         strip.text=element_text(face="bold", size=8),
                         axis.title=element_blank(),
                         axis.text=element_blank(),
                         axis.ticks=element_blank(),
                         panel.border=element_blank()))
p2 <- ggplotGrob(p.MD.byWell + guides(color="none") + 
                   theme(plot.margin=margin(0,0,0,0, "mm"),
                         strip.background=element_blank(),
                         strip.text=element_blank(),
                         axis.title=element_blank(),
                         axis.text=element_blank(),
                         axis.ticks=element_blank(),
                         panel.border=element_blank()))
p3 <- ggplotGrob(
  p.LD.byWell + guides(color="none") + 
    theme(plot.margin=margin(0,0,0,0, "mm"),
          strip.background=element_blank(),
          strip.text=element_blank(),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.border=element_blank()))
p <- rbind(p1, p2, p3, size="first")
p$widths <- unit.pmax(p1$widths, p2$widths, p3$widths)

depletion.legend <- g_legend(p.LD.byWell + 
                               theme(legend.position="bottom", legend.text=element_text(size=8)) + 
                               guides(color=guide_legend(label.position="bottom",
                                                        nrow=1)))

# save output
ggsave(paste0(dir.fig, "Figure_Map_Depletion_OneWellAllReach_NoLabels.pdf"),
       grid.arrange(p, depletion.legend, heights=c(8,1)), 
       width=190, height=135, units="mm", device=cairo_pdf)
