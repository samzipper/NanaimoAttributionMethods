## Figure_Map_Depletino_OneMethodAllWells.R
#' Figure showing depletion across all wells for a single method (web squared).
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

# tidy spatial data for ggplot
df.LD.boundary <- tidy(shp.LD.boundary)
df.LD.streams <- tidy(shp.LD.streams)

# set up reach and well columns to match df.all
df.LD.wells$well <- as.numeric(substring(df.LD.wells$Well_Name, 8))
df.LD.streams$reach <- as.numeric(df.LD.streams$id)+2

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

# extract MODFLOW data and make it a column of its own in 'df' data frame
df.mod <- subset(df.all, method=="MODFLOW")
df <- subset(df.all, method != "MODFLOW")
colnames(df.mod)[colnames(df.mod)=="depletion.prc"] <- "depletion.prc.modflow"
df.mod$method <- NULL
df <- left_join(df, df.mod)
df$depletion.diff.prc <- df$depletion.prc - df$depletion.prc.modflow  # analytical - MODFLOW; positive means analytical overpreedicts

# subset to only method and LD
df <- subset(df, method %in% c("THIESSEN", "IDLINSQ", "WEBLINSQ") & drainage.density=="LD" & topography=="FLAT")
df$method <- factor(df$method, levels=c("THIESSEN", "IDLINSQ", "WEBLINSQ"))

# add reach length to df
df <- left_join(df, df.LD.stream.length, by=c("reach", "drainage.density"))

# summarize by well: number of differences > 5% between modflow and weblinsq
df.diff <- dplyr::summarize(group_by(df, method, well),
                            n.diff.gt.5 = sum(abs(depletion.diff.prc)>5),
                            n.diff.gt.10 = sum(abs(depletion.diff.prc)>10),
                            diff.abs.mean = mean(abs(depletion.diff.prc)))
df.LD.wells.diff <- left_join(df.diff, df.LD.wells, by="well")

## make plots
p.LD.byWell <-
  ggplot(data=df.LD.wells.diff) +
  facet_wrap(~method, labeller=as_labeller(labels.method), scales="free") +
  geom_polygon(data=df.LD.boundary, aes(x=long, y=lat), color="black", fill=NA) +
  geom_path(data=df.LD.streams, aes(x=long, y=lat, group=group), color="blue") +
  geom_point(aes(x=X.world_coord.m., y=Y.world_coord.m., color=factor(n.diff.gt.10))) +
  scale_x_continuous(name="Easting [m]", breaks=pretty(df.LD.wells.diff$X.world_coord.m., 2)) + 
  scale_y_continuous(name="Northing [m]") +
  scale_color_manual(name="Number of Reaches with abs(Difference) > 10%", 
                    values=c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026")) +
  coord_equal() +
  theme_scz() +
  theme(strip.background=element_blank(), legend.position="bottom") + 
  guides(color = guide_legend(nrow = 1))

# save output
ggsave(paste0(dir.fig, "Figure_Map_Depletion_OneMethodAllWells.pdf"),
       p.LD.byWell, 
       width=190, height=135, units="mm", device=cairo_pdf)

ggsave(paste0(dir.fig, "Figure_Map_Depletion_OneMethodAllWells.png"),
       p.LD.byWell, 
       width=190, height=135, units="mm", dpi=300)
