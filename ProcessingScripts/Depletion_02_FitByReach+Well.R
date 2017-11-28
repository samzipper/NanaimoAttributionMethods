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
df.all$drainage.density <- factor(df.all$drainage.density, levels=c("HD", "MD", "LD"))
df.all$topography <- factor(df.all$topography, levels=c("FLAT", "ELEV"))
df.all$recharge <- factor(df.all$recharge, levels=c("NORCH", "RCH10", "RCH50", "RCH100", "RCH500", "RCH1000"))
df.all$method <- factor(df.all$method, levels=c("MODFLOW", "THIESSEN", "IDLIN", "IDLINSQ", "WEBLIN", "WEBLINSQ"))

# cut depletion data into categories
df.all$depletion.prc.class <- cut(df.all$depletion.prc, 
                                  breaks=c(min(df.all$depletion.prc),5,10,15,20,max(df.all$depletion.prc)),
                                  labels=c("<5%", "5-10%", "10-15%", "15-20%",">20%"))

# check depletion to make sure things sum to 100
df.depletion.byWell <- dplyr::summarize(group_by(df.all, well, drainage.density, topography, recharge, method),
                                        depletion.prc.sum = sum(depletion.prc))

# wells near edges often don't sum to 100, especially for Thiessen and MODFLOW methods

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

p.ByScenario.sensitivity.scatter <-
  ggplot(subset(df.prc, topography=="ELEV" & drainage.density=="LD"), aes(x=depletion.prc, y=depletion.prc.modflow, color=method)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(shape=21) +
  stat_smooth(method="lm") +
  facet_wrap(~recharge) +
  scale_x_continuous(name="Analytical Depletion [% of Total Depletion]") +
  scale_y_continuous(name="MODFLOW Depletion [% of Total Depletion]") +
  scale_color_manual(values=pal.method) +
  theme_scz() +
  theme(legend.position="bottom")
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.ByScenario.sensitivity.scatter.png"),
       p.ByScenario.sensitivity.scatter, width=8, height=6, units="in")

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

##### make maps #####
## load and prep data
# load data
shp.LD.boundary <- readOGR(paste0(dir.GSAS, "GIS"), layer="LD_boundary_aquifer")
shp.LD.streams <- readOGR(paste0(dir.GSAS, "GIS"), layer="LD_streams")
df.LD.wells <- read.csv(paste0(dir.GSAS, "GIS/LD_wells_info.csv"), stringsAsFactors=F)

shp.MD.boundary <- readOGR(paste0(dir.GSAS, "GIS"), layer="MD_boundary_aquifer")
shp.MD.streams <- readOGR(paste0(dir.GSAS, "GIS"), layer="MD_streams")
shp.MD.wells <- readOGR(paste0(dir.GSAS, "GIS"), layer="MD_wells")
#df.MD.wells <- read.csv(paste0(dir.GSAS, "GIS/MD_wells_info.csv"), stringsAsFactors=F)

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

### make maps of reach depletion for a given well
# low density
# choose a well
LD.well.n <- 20

# subset depletion data for that well
df.LD.depletion.well <- subset(df.all, topography=="FLAT" & recharge=="NORCH" & drainage.density=="LD" & well==LD.well.n)
df.LD.streams.all <- inner_join(df.LD.streams, df.LD.depletion.well, by="reach")

# make ggplot: streamlines color-coded by depletion for one well
p.LD.byWell <-
  ggplot(data=df.LD.streams.all, aes(x=long, y=lat)) +
  facet_wrap(~method, ncol=6) +
  geom_polygon(data=df.LD.boundary, aes(x=long, y=lat), color="gray65", fill=NA) +
#  geom_point(data=df.LD.wells, aes(x=X.world_coord.m., y=Y.world_coord.m.), color="black", shape=21) +
  geom_point(data=subset(df.LD.wells, well==LD.well.n), aes(x=X.world_coord.m., y=Y.world_coord.m.), color="red", size=2) +
  geom_path(aes(group=group, color=depletion.prc.class), size=1) +
  scale_color_manual(name="Depletion [%]", values=pal.depletion, drop=F, guide=F) +
  coord_equal() +
  theme_scz() +
  theme(legend.position="bottom")

# medium density
# choose a well
MD.well.n <- 23

# subset depletion data for that well
df.MD.depletion.well <- subset(df.all, topography=="FLAT" & recharge=="NORCH" & drainage.density=="MD" & well==MD.well.n)
df.MD.streams.all <- inner_join(df.MD.streams, df.MD.depletion.well, by="reach")

# make ggplot: streamlines color-coded by depletion for one well
p.MD.byWell <-
  ggplot(data=df.MD.streams.all, aes(x=long, y=lat)) +
  facet_wrap(~method, ncol=6) +
  geom_polygon(data=df.MD.boundary, aes(x=long, y=lat), color="gray65", fill=NA) +
#  geom_point(data=df.MD.wells, aes(x=X_world_co, y=Y_world_co), color="black", shape=21) +
  geom_point(data=subset(df.MD.wells, well==MD.well.n), aes(x=X_world_co, y=Y_world_co), color="red", size=2) +
  geom_path(aes(group=group, color=depletion.prc.class), size=1) +
  scale_color_manual(name="Depletion [%]", values=pal.depletion, drop=F, guide=F) +
  coord_equal() +
  theme_scz() +
  theme(legend.position="bottom")

# low density
# choose a well
HD.well.n <- 23

# subset depletion data for that well
df.HD.depletion.well <- subset(df.all, topography=="FLAT" & recharge=="NORCH" & drainage.density=="HD" & well==HD.well.n)
df.HD.streams.all <- inner_join(df.HD.streams, df.HD.depletion.well, by="reach")

# make ggplot: streamlines color-coded by depletion for one well
p.HD.byWell <-
  ggplot(data=df.HD.streams.all, aes(x=long, y=lat)) +
  facet_wrap(~method, ncol=6) +
  geom_polygon(data=df.HD.boundary, aes(x=long, y=lat), color="gray65", fill=NA) +
#  geom_point(data=df.HD.wells, aes(x=X.world_coord.m., y=Y.world_coord.m.), color="black", shape=21) +
  geom_point(data=subset(df.HD.wells, well==HD.well.n), aes(x=X.world_coord.m., y=Y.world_coord.m.), color="red", size=2) +
  geom_path(aes(group=group, color=depletion.prc.class), size=1) +
  scale_color_manual(name="Depletion [%]", values=pal.depletion, drop=F) +
  coord_equal() +
  theme_scz() +
  theme(legend.position="bottom")

# save output
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.depletion.byWell.png"),
       arrangeGrob(p.LD.byWell + theme(axis.text = element_blank(), axis.title=element_blank(), panel.border=element_blank(), 
                                       axis.ticks=element_blank(), strip.background=element_blank()),
                   p.MD.byWell + theme(axis.text = element_blank(), axis.title=element_blank(), panel.border=element_blank(), 
                                       axis.ticks=element_blank(), strip.background=element_blank()), 
                   p.HD.byWell + theme(axis.text = element_blank(), axis.title=element_blank(), panel.border=element_blank(), 
                                       axis.ticks=element_blank(), strip.background=element_blank()),
                   ncol=1, heights=c(1, 0.7, 0.8)),
       width=12, height=8)

### make rasters of depletion for a given reach
# low density
# choose a reach
df.prc.LD <- subset(df.prc, drainage.density=="LD" & recharge=="NORCH" & topography=="FLAT")
df.prc.LD.nwell <- dplyr::summarize(group_by(df.prc.LD, reach), n.well=sum(is.finite(depletion.prc)))
LD.reach.n <- 51

# subset depletion data for that reach
df.LD.depletion.reach <- subset(df.all, topography=="FLAT" & recharge=="NORCH" & drainage.density=="LD" & reach==LD.reach.n)
df.LD.depletion.reach.wide <- dcast(subset(df.LD.depletion.reach, select=c("well", "depletion.prc", "method")), well~method, value.var="depletion.prc")
df.LD.depletion <- left_join(df.LD.wells, df.LD.depletion.reach.wide, by="well")

# empty LD raster
r.LD.empty <- raster(extent(shp.LD.boundary), crs=crs(shp.LD.boundary))
res(r.LD.empty) <- c(50,50)

# rasterize for each method - interpolate using IDW (package: gstat) https://mgimond.github.io/Spatial/interpolation-in-r.html
sp.LD <- SpatialPoints(df.LD.depletion[,c("X.world_coord.m.", "Y.world_coord.m.")], proj4string=crs(shp.LD.boundary))
sp.LD <- SpatialPointsDataFrame(sp.LD, df.LD.depletion)
start.flag <- T
for (method in c("MODFLOW", "THIESSEN", "IDLIN", "IDLINSQ", "WEBLIN", "WEBLINSQ")){
  # build IDW model
  gs <- gstat(formula=as.formula(paste0(method, "~1")), locations=sp.LD)
  
  # interpolate to raster
  r.LD.method <- interpolate(r.LD.empty, gs)
  
  # mask with shapefile
  r.LD.method <- mask(r.LD.method, shp.LD.boundary)
  
  # convert to data frame
  df.LD.method <- as.data.frame(rasterToPoints(r.LD.method))
  colnames(df.LD.method) <- c("long", "lat", "depletion.prc")
  df.LD.method$method <- method
  
  # output dataframe
  if (start.flag){
    df.LD.depletion.all <- df.LD.method
    start.flag <- F
  } else {
    df.LD.depletion.all <- rbind(df.LD.depletion.all, df.LD.method)
  }
  
}

# make breaks
df.LD.depletion.all$depletion.prc.class <- cut(df.LD.depletion.all$depletion.prc,
                                               breaks=c(c(0,5,10), seq(20,100,10)),
                                               labels=c("0-5%", "5-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
                                                        "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
                                               include.lowest=T)

# set factor level
df.LD.depletion.all$method <- factor(df.LD.depletion.all$method, levels=levels(df.all$method))

# make ggplot: streamlines color-coded by depletion for one well
p.LD.byReach <-
  ggplot(data=df.LD.depletion.all, aes(x=long, y=lat)) +
  geom_raster(aes(fill=depletion.prc.class)) +
  facet_wrap(~method, ncol=6) +
#  geom_polygon(data=df.LD.boundary, aes(x=long, y=lat), color="gray65", fill=NA) +
  geom_point(data=df.LD.wells, aes(x=X.world_coord.m., y=Y.world_coord.m.), color="black", shape=21, alpha=0.5) +
  geom_path(data=subset(df.LD.streams, reach==LD.reach.n), aes(x=long, y=lat, group=group), color="red", size=1) +
  scale_fill_manual(name="Depletion [%]", values=pal.depletion.0to100, drop=F) +
  coord_equal() +
  theme_scz() 

# medium density
# choose a reach
df.prc.MD <- subset(df.prc, drainage.density=="MD" & recharge=="NORCH" & topography=="FLAT")
df.prc.MD.nwell <- dplyr::summarize(group_by(df.prc.MD, reach), n.well=sum(is.finite(depletion.prc)))
MD.reach.n <- 11

# subset depletion data for that reach
df.MD.depletion.reach <- subset(df.all, topography=="FLAT" & recharge=="NORCH" & drainage.density=="MD" & reach==MD.reach.n)
df.MD.depletion.reach.wide <- dcast(subset(df.MD.depletion.reach, select=c("well", "depletion.prc", "method")), well~method, value.var="depletion.prc")
df.MD.depletion <- left_join(df.MD.wells, df.MD.depletion.reach.wide, by="well")

# empty MD raster
r.MD.empty <- raster(extent(shp.MD.boundary), crs=crs(shp.MD.boundary))
res(r.MD.empty) <- c(50,50)

# rasterize for each method - interpolate using IDW (package: gstat) https://mgimond.github.io/Spatial/interpolation-in-r.html
sp.MD <- SpatialPoints(df.MD.depletion[,c("X_world_co", "Y_world_co")], proj4string=crs(shp.MD.boundary))
sp.MD <- SpatialPointsDataFrame(sp.MD, df.MD.depletion)
start.flag <- T
for (method in c("MODFLOW", "THIESSEN", "IDLIN", "IDLINSQ", "WEBLIN", "WEBLINSQ")){
  # build IDW model
  gs <- gstat(formula=as.formula(paste0(method, "~1")), locations=sp.MD)
  
  # interpolate to raster
  r.MD.method <- interpolate(r.MD.empty, gs)
  
  # mask with shapefile
  r.MD.method <- mask(r.MD.method, shp.MD.boundary)
  
  # convert to data frame
  df.MD.method <- as.data.frame(rasterToPoints(r.MD.method))
  colnames(df.MD.method) <- c("long", "lat", "depletion.prc")
  df.MD.method$method <- method
  
  # output dataframe
  if (start.flag){
    df.MD.depletion.all <- df.MD.method
    start.flag <- F
  } else {
    df.MD.depletion.all <- rbind(df.MD.depletion.all, df.MD.method)
  }
  
}

# make breaks
df.MD.depletion.all$depletion.prc.class <- cut(df.MD.depletion.all$depletion.prc,
                                               breaks=c(c(0,5,10), seq(20,100,10)),
                                               labels=c("0-5%", "5-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
                                                        "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
                                               include.lowest=T)

# set factor level
df.MD.depletion.all$method <- factor(df.MD.depletion.all$method, levels=levels(df.all$method))

# make ggplot: streamlines color-coded by depletion for one well
p.MD.byReach <-
  ggplot(data=df.MD.depletion.all, aes(x=long, y=lat)) +
  geom_raster(aes(fill=depletion.prc.class)) +
  facet_wrap(~method, ncol=6) +
  #  geom_polygon(data=df.MD.boundary, aes(x=long, y=lat), color="gray65", fill=NA) +
  geom_point(data=df.MD.wells, aes(x=X_world_co, y=Y_world_co), color="black", shape=21, alpha=0.5) +
  geom_path(data=subset(df.MD.streams, reach==MD.reach.n), aes(x=long, y=lat, group=group), color="red", size=1) +
  scale_fill_manual(name="Depletion [%]", values=pal.depletion.0to100, drop=F, guide=F) +
  coord_equal() +
  theme_scz()

# low density
# choose a reach
df.prc.HD <- subset(df.prc, drainage.density=="HD" & recharge=="NORCH" & topography=="FLAT")
df.prc.HD.nwell <- dplyr::summarize(group_by(df.prc.HD, reach), n.well=sum(is.finite(depletion.prc)))
HD.reach.n <- 30

# subset depletion data for that reach
df.HD.depletion.reach <- subset(df.all, topography=="FLAT" & recharge=="NORCH" & drainage.density=="HD" & reach==HD.reach.n)
df.HD.depletion.reach.wide <- dcast(subset(df.HD.depletion.reach, select=c("well", "depletion.prc", "method")), well~method, value.var="depletion.prc")
df.HD.depletion <- left_join(df.HD.wells, df.HD.depletion.reach.wide, by="well")

# empty HD raster
r.HD.empty <- raster(extent(shp.HD.boundary), crs=crs(shp.HD.boundary))
res(r.HD.empty) <- c(50,50)

# rasterize for each method - interpolate using IDW (package: gstat) https://mgimond.github.io/Spatial/interpolation-in-r.html
sp.HD <- SpatialPoints(df.HD.depletion[,c("X.world_coord.m.", "Y.world_coord.m.")], proj4string=crs(shp.HD.boundary))
sp.HD <- SpatialPointsDataFrame(sp.HD, df.HD.depletion)
start.flag <- T
for (method in c("MODFLOW", "THIESSEN", "IDLIN", "IDLINSQ", "WEBLIN", "WEBLINSQ")){
  # build IDW model
  gs <- gstat(formula=as.formula(paste0(method, "~1")), locations=sp.HD)
  
  # interpolate to raster
  r.HD.method <- interpolate(r.HD.empty, gs)
  
  # mask with shapefile
  r.HD.method <- mask(r.HD.method, shp.HD.boundary)
  
  # convert to data frame
  df.HD.method <- as.data.frame(rasterToPoints(r.HD.method))
  colnames(df.HD.method) <- c("long", "lat", "depletion.prc")
  df.HD.method$method <- method
  
  # output dataframe
  if (start.flag){
    df.HD.depletion.all <- df.HD.method
    start.flag <- F
  } else {
    df.HD.depletion.all <- rbind(df.HD.depletion.all, df.HD.method)
  }
  
}

# make breaks
df.HD.depletion.all$depletion.prc.class <- cut(df.HD.depletion.all$depletion.prc,
                                               breaks=c(c(0,5,10), seq(20,100,10)),
                                               labels=c("0-5%", "5-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
                                                        "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
                                               include.lowest=T)

# set factor level
df.HD.depletion.all$method <- factor(df.HD.depletion.all$method, levels=levels(df.all$method))

# make ggplot: streamlines color-coded by depletion for one well
p.HD.byReach <-
  ggplot(data=df.HD.depletion.all, aes(x=long, y=lat)) +
  geom_raster(aes(fill=depletion.prc.class)) +
  facet_wrap(~method, ncol=6) +
  #  geom_polygon(data=df.HD.boundary, aes(x=long, y=lat), color="gray65", fill=NA) +
  geom_point(data=df.HD.wells, aes(x=X.world_coord.m., y=Y.world_coord.m.), color="black", shape=21, alpha=0.5) +
  geom_path(data=subset(df.HD.streams, reach==HD.reach.n), aes(x=long, y=lat, group=group), color="red", size=1) +
  scale_fill_manual(name="Depletion [%]", values=pal.depletion.0to100, drop=F, guide=F) +
  coord_equal() +
  theme_scz()

# save output
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.depletion.byReach.png"),
       arrangeGrob(p.LD.byReach + theme(axis.text = element_blank(), axis.title=element_blank(), panel.border=element_blank(), 
                                       axis.ticks=element_blank(), strip.background=element_blank()),
                   p.MD.byReach + theme(axis.text = element_blank(), axis.title=element_blank(), panel.border=element_blank(), 
                                       axis.ticks=element_blank(), strip.background=element_blank()), 
                   p.HD.byReach + theme(axis.text = element_blank(), axis.title=element_blank(), panel.border=element_blank(), 
                                       axis.ticks=element_blank(), strip.background=element_blank()),
                   ncol=1, heights=c(1, 0.7, 0.7)),
       width=12, height=8)

#### Elevation sensitivity analysis ####

p.elev.ByScenario.scatter <-
  ggplot(subset(df.prc, drainage.density=="LD" & recharge=="NORCH"), aes(x=depletion.prc, y=depletion.prc.modflow, color=method)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(shape=21) +
  stat_smooth(method="lm") +
  facet_wrap(~topography) +
  scale_x_continuous(name="Analytical Depletion [% of Total Depletion]", breaks=seq(0,100,25)) +
  scale_y_continuous(name="MODFLOW Depletion [% of Total Depletion]", breaks=seq(0,100,25)) +
  scale_color_manual(values=pal.method) +
  theme_scz()

p.elev.depletion.diff.dens.noZeros <-
  ggplot(subset(df.prc, drainage.density=="LD" & recharge=="NORCH"), aes(x=depletion.diff.prc, fill=method, color=method)) +
  geom_density(alpha=0.2) +
  geom_vline(xintercept=0) +
  facet_wrap(~topography, scales="free_y") +
  scale_x_continuous(name="Analytical - MODFLOW [% of Total Depletion]") +
  scale_fill_manual(values=pal.method) +
  scale_color_manual(values=pal.method) +
  theme_scz()

p.elev.fit.ByScenario.tern.facet <-
  ggtern(subset(df.fit.ByScenario, drainage.density=="LD" & recharge=="NORCH"), 
         aes(x=MSE.bias.norm, y=MSE.var.norm, z=MSE.cor.norm, size=-KGE.overall, color=method)) +
  geom_point() +
  facet_wrap(~topography) +
  labs(x="% MSE due to Bias", y="% MSE due to Variability", z="% MSE due to Correlation") +
  scale_color_manual(values=pal.method) +
  theme_rgbw() +
  theme(tern.axis.title=element_blank(),
        tern.panel.grid.major=element_blank(),
        legend.position="bottom")

# save output
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.elev.fit.png"),
       ggtern::arrangeGrob(p.elev.ByScenario.scatter,
                           p.elev.depletion.diff.dens.noZeros, 
                           p.elev.fit.ByScenario.tern.facet,
                           ncol=1, heights=c(0.75, 0.75, 1)),
       width=6, height=12)

# plot elev diff vs flat diff
df.elev.diff <- subset(df.prc, drainage.density=="LD" & recharge=="NORCH")[,c("well", "reach", "topography", "method", "depletion.diff.prc")]
df.elev.diff <- dcast(df.elev.diff, well+reach+method ~ topography, value.var="depletion.diff.prc")
p.elev.diff.ByScenario.scatter <-
  ggplot(df.elev.diff, aes(x=FLAT, y=ELEV, color=method)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(shape=21) +
  stat_smooth(method="lm") +
  scale_x_continuous(name="Analyical-MODFLOW [%], FLAT") +
  scale_y_continuous(name="Analyical-MODFLOW [%], ELEV") +
  scale_color_manual(values=pal.method) +
  theme_scz()
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.elev.diff.ByScenario.scatter.png"),
       p.elev.diff.ByScenario.scatter,
       width=6, height=6)

# stats
summary(lm(FLAT ~ ELEV, data=subset(df.elev.diff, method=="THIESSEN")))
summary(lm(FLAT ~ ELEV, data=subset(df.elev.diff, method=="IDLIN")))
summary(lm(FLAT ~ ELEV, data=subset(df.elev.diff, method=="IDLINSQ")))
summary(lm(FLAT ~ ELEV, data=subset(df.elev.diff, method=="WEBLIN")))
summary(lm(FLAT ~ ELEV, data=subset(df.elev.diff, method=="WEBLINSQ")))

#### Recharge sensitivity analysis ####

p.recharge.ByScenario.scatter <-
  ggplot(subset(df.prc, drainage.density=="LD" & topography=="ELEV"), aes(x=depletion.prc, y=depletion.prc.modflow, color=method)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(shape=21) +
  stat_smooth(method="lm") +
  facet_wrap(~recharge, ncol=6) +
  scale_x_continuous(name="Analytical Depletion [% of Total Depletion]", breaks=seq(0,100,25)) +
  scale_y_continuous(name="MODFLOW Depletion [% of Total Depletion]", breaks=seq(0,100,25)) +
  scale_color_manual(values=pal.method) +
  theme_scz()

p.recharge.depletion.diff.dens.noZeros <-
  ggplot(subset(df.prc, drainage.density=="LD" & topography=="ELEV"), aes(x=depletion.diff.prc, fill=method, color=method)) +
  geom_density(alpha=0.2) +
  geom_vline(xintercept=0) +
  facet_wrap(~recharge, scales="free_y", ncol=6) +
  scale_x_continuous(name="Analytical - MODFLOW [% of Total Depletion]") +
  scale_fill_manual(values=pal.method) +
  scale_color_manual(values=pal.method) +
  theme_scz()

p.recharge.fit.ByScenario.tern.facet <-
  ggtern(subset(df.fit.ByScenario, drainage.density=="LD" & topography=="ELEV"), 
         aes(x=MSE.bias.norm, y=MSE.var.norm, z=MSE.cor.norm, size=-KGE.overall, color=method)) +
  geom_point() +
  facet_wrap(~recharge, ncol=6) +
  labs(x="% MSE due to Bias", y="% MSE due to Variability", z="% MSE due to Correlation") +
  scale_color_manual(values=pal.method) +
  theme_rgbw() +
  theme(tern.axis.title=element_blank(),
        tern.panel.grid.major=element_blank(),
        legend.position="bottom")

# save output
ggsave(paste0(dir.plot, "Depletion_02_FitByReach+Well_p.recharge.fit.png"),
       ggtern::arrangeGrob(p.recharge.ByScenario.scatter + theme(legend.position="bottom"),
                           p.recharge.depletion.diff.dens.noZeros + theme(legend.position="bottom"), 
                           p.recharge.fit.ByScenario.tern.facet + theme(legend.position="bottom"),
                           ncol=1, heights=c(0.75, 0.6, 1)),
       width=14, height=10)
