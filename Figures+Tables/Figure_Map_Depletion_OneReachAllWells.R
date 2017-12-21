## Figure_Map_Depletion_OneReachAllWells.R
#' Figure showing, for a single stream reach, how depletion varies for each well.
#' 
#' This script requires output from Depletion_01_AggregateAllResults.R
#' and must be connected to GSAS server.

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

##### make maps #####
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
                                               labels=c(" 0-5% ", " 5-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
                                                        "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
                                               include.lowest=T)

# set factor level
df.LD.depletion.all$method <- factor(df.LD.depletion.all$method, levels=levels(df.prc$method))

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
                                               labels=c(" 0-5% ", " 5-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
                                                        "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
                                               include.lowest=T)

# set factor level
df.MD.depletion.all$method <- factor(df.MD.depletion.all$method, levels=levels(df.prc$method))

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
                                               labels=c(" 0-5% ", " 5-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
                                                        "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
                                               include.lowest=T)

# set factor level
df.HD.depletion.all$method <- factor(df.HD.depletion.all$method, levels=levels(df.prc$method))

### make plots
p.HD.byReach <-
  ggplot(data=df.HD.depletion.all, aes(x=long, y=lat)) +
  geom_raster(aes(fill=depletion.prc.class)) +
  facet_wrap(~method, ncol=6, 
             labeller=as_labeller(c("MODFLOW"="MODFLOW", labels.method))) +
#  labeller=as_labeller(c("MODFLOW"="(a) MODFLOW", "THIESSEN"="(b) TPOLY", "IDLIN"="(c) ID", 
#                         "IDLINSQ"="(d) IDS", "WEBLIN"="(e) WID", "WEBLINSQ"="(f) WIDS"))) +
  #geom_polygon(data=df.HD.boundary, aes(x=long, y=lat), color="gray65", fill=NA) +
  #geom_point(data=df.HD.wells, aes(x=X.world_coord.m., y=Y.world_coord.m.), color="black", shape=21, alpha=0.5) +
  geom_path(data=subset(df.HD.streams, reach==HD.reach.n), aes(x=long, y=lat, group=group), color="red", size=1) +
  scale_fill_manual(name="Depletion [%]", values=pal.depletion.0to100) +
  coord_equal() +
  theme_scz()

p.MD.byReach <-
  ggplot(data=df.MD.depletion.all, aes(x=long, y=lat)) +
  geom_raster(aes(fill=depletion.prc.class)) +
  facet_wrap(~method, ncol=6, 
             labeller=as_labeller(c("MODFLOW"="(g) MODFLOW", "THIESSEN"="(h) TPOLY", "IDLIN"="(i) ID", 
                                    "IDLINSQ"="(j) IDS", "WEBLIN"="(k) WID", "WEBLINSQ"="(l) WIDS"))) +
  #geom_polygon(data=df.MD.boundary, aes(x=long, y=lat), color="gray65", fill=NA) +
  #geom_point(data=df.MD.wells, aes(x=X_world_co, y=Y_world_co), color="black", shape=21, alpha=0.5) +
  geom_path(data=subset(df.MD.streams, reach==MD.reach.n), aes(x=long, y=lat, group=group), color="red", size=1) +
  scale_fill_manual(name="Depletion [%]", values=pal.depletion.0to100, drop=T) +
  coord_equal() +
  theme_scz()

p.LD.byReach <-
  ggplot(data=df.LD.depletion.all, aes(x=long, y=lat)) +
  geom_raster(aes(fill=depletion.prc.class)) +
  facet_wrap(~method, ncol=6, 
             labeller=as_labeller(c("MODFLOW"="(m) MODFLOW", "THIESSEN"="(n) TPOLY", "IDLIN"="(o) ID", 
                                    "IDLINSQ"="(p) IDS", "WEBLIN"="(q) WID", "WEBLINSQ"="(r) WIDS"))) +
  #geom_polygon(data=df.LD.boundary, aes(x=long, y=lat), color="gray65", fill=NA) +
  #geom_point(data=df.LD.wells, aes(x=X.world_coord.m., y=Y.world_coord.m.), color="black", shape=21, alpha=0.5) +
  geom_path(data=subset(df.LD.streams, reach==LD.reach.n), aes(x=long, y=lat, group=group), color="red", size=1) +
  scale_fill_manual(name="Depletion [%]", values=pal.depletion.0to100, drop=T) +
  coord_equal() +
  theme_scz()

# align plots and fine-tune themes
p1 <- ggplotGrob(p.HD.byReach + guides(fill="none") + 
                   theme(plot.margin=margin(0,0,0,0, "mm"),
                         strip.background=element_blank(),
                         strip.text=element_text(face="bold", size=8),
                         axis.title=element_blank(),
                         axis.text=element_blank(),
                         axis.ticks=element_blank(),
                         panel.border=element_blank()))
p2 <- ggplotGrob(p.MD.byReach + guides(fill="none") + 
                   theme(plot.margin=margin(0,0,0,0, "mm"),
                         strip.background=element_blank(),
                         strip.text=element_blank(),
                         axis.title=element_blank(),
                         axis.text=element_blank(),
                         axis.ticks=element_blank(),
                         panel.border=element_blank()))
p3 <- ggplotGrob(
  p.LD.byReach + guides(fill="none") + 
                   theme(plot.margin=margin(0,0,0,0, "mm"),
                         strip.background=element_blank(),
                         strip.text=element_blank(),
                         axis.title=element_blank(),
                         axis.text=element_blank(),
                         axis.ticks=element_blank(),
                         panel.border=element_blank())
                 )
p <- rbind(p1, p2, p3, size="first")
p$widths <- unit.pmax(p1$widths, p2$widths, p3$widths)


depletion.legend <- g_legend(p.LD.byReach + 
                               theme(legend.position="bottom", legend.text=element_text(size=8)) + 
                               guides(fill=guide_legend(label.position="bottom",
                                                        nrow=1)))

# save output
ggsave(paste0(dir.fig, "Figure_Map_Depletion_OneReachAllWells_NoLabels.pdf"),
       grid.arrange(p, depletion.legend, heights=c(8,1)), 
       width=190, height=135, units="mm", device=cairo_pdf)

# # make all plots via facet_grid
# df.HD.depletion.all$drainage.density <- "HD"
# df.MD.depletion.all$drainage.density <- "MD"
# df.LD.depletion.all$drainage.density <- "LD"
# df.depletion.all <- rbind(df.HD.depletion.all, df.MD.depletion.all, df.LD.depletion.all)
# df.depletion.all$drainage.density <- factor(df.depletion.all$drainage.density, levels=c("HD", "MD", "LD"))
# 
# df.HD.streams$drainage.density <- "HD"
# df.MD.streams$drainage.density <- "MD"
# df.LD.streams$drainage.density <- "LD"
# df.streams <- rbind(subset(df.HD.streams, reach==HD.reach.n), 
#                     subset(df.MD.streams, reach==MD.reach.n),
#                     subset(df.LD.streams, reach==LD.reach.n))
# df.streams$drainage.density <- factor(df.streams$drainage.density, levels=c("HD", "MD", "LD"))
# 
# # add a bunch of stream data frames together
# start.flag <- T
# for (method in c("MODFLOW", "THIESSEN", "IDLIN", "IDLINSQ", "WEBLIN", "WEBLINSQ")){
#   df.streams$method <- method
#   if (start.flag){
#     df.streams.all <- df.streams
#     start.flag <- F
#   } else {
#     df.streams.all <- rbind(df.streams.all, df.streams)
#   }
# }
# 
# p.all.byReach <-
#   ggplot() +
#   geom_raster(data=df.depletion.all, aes(x=long, y=lat, fill=depletion.prc.class)) +
#   geom_path(data=df.streams.all, aes(x=long, y=lat, group=group), color="red", size=1) +
#   facet_wrap(drainage.density~method, ncol=6, scales="free",
#              labeller=as_labeller(c("MODFLOW"="MODFLOW", labels.method, labels.density))) +
#   #geom_polygon(data=df.LD.boundary, aes(x=long, y=lat), color="gray65", fill=NA) +
#   #geom_point(data=df.LD.wells, aes(x=X.world_coord.m., y=Y.world_coord.m.), color="black", shape=21, alpha=0.5) +
#   scale_fill_manual(name="Depletion [%]", values=pal.depletion.0to100, drop=T) +
#   #coord_equal() +
#   theme_scz() +
#   theme(legend.position="bottom",
#         plot.margin=margin(0,0,0,0, "mm"),
#         strip.background=element_blank(),
#         strip.text=element_blank(),
#         axis.title=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         panel.border=element_blank()) +
#   guides(fill=guide_legend(label.position="bottom", nrow=1))