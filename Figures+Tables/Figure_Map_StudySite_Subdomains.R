## Figure_Map_StudySite_Subdomains.R
#' Figure showing HD, MD, and LD subdomains.
#' 
#' Must be connected to GSAS server.

rm(list=ls())

# directory to repository on local computer
dir.git <- "C:/Users/Sam/WorkGits/NanaimoAttributionMethods/"

# load paths + packages
source(paste0(dir.git, "ProcessingScripts/paths+packages.R"))

## load and prep data
# load data
shp.LD.boundary <- readOGR(paste0(dir.GSAS, "GIS"), layer="LD_boundary_aquifer")
shp.LD.inset <- readOGR(paste0(dir.GSAS, "GIS"), layer="LD_boundary_inset_clipToAquifer")
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
df.LD.inset <- tidy(shp.LD.inset)
df.LD.streams <- tidy(shp.LD.streams)

df.MD.boundary <- tidy(shp.MD.boundary)
df.MD.inset <- tidy(shp.MD.inset)
df.MD.streams <- tidy(shp.MD.streams)
df.MD.wells <- as.data.frame(shp.MD.wells)

df.HD.boundary <- tidy(shp.HD.boundary)
df.HD.inset <- tidy(shp.HD.inset)
df.HD.streams <- tidy(shp.HD.streams)

# set up reach and well columns to match df.all
df.LD.wells$well <- as.numeric(substring(df.LD.wells$Well_Name, 8))
df.LD.streams$reach <- as.numeric(df.LD.streams$id)+2

df.MD.wells$well <- as.numeric(substring(df.MD.wells$Well_Name, 8))
df.MD.streams$reach <- as.numeric(df.MD.streams$id)+2

df.HD.wells$well <- as.numeric(substring(df.HD.wells$Well_Name, 8))
df.HD.streams$reach <- as.numeric(df.HD.streams$id)+2

# normalize columns to 0,0
LD.x.min <- min(df.LD.boundary$long)
LD.y.min <- min(df.LD.boundary$lat)
df.LD.boundary$long.0 <- df.LD.boundary$long - LD.x.min
df.LD.boundary$lat.0 <- df.LD.boundary$lat - LD.y.min
df.LD.boundary$drainage.density <- "LD"
df.LD.inset$long.0 <- df.LD.inset$long - LD.x.min
df.LD.inset$lat.0 <- df.LD.inset$lat - LD.y.min
df.LD.inset$drainage.density <- "LD"
df.LD.streams$long.0 <- df.LD.streams$long - LD.x.min
df.LD.streams$lat.0 <- df.LD.streams$lat - LD.y.min
df.LD.streams$drainage.density <- "LD"
df.LD.wells$long.0 <- df.LD.wells$X.world_coord.m. - LD.x.min
df.LD.wells$lat.0 <- df.LD.wells$Y.world_coord.m. - LD.y.min
df.LD.wells$drainage.density <- "LD"

MD.x.min <- min(df.MD.boundary$long)
MD.y.min <- min(df.MD.boundary$lat)
df.MD.boundary$long.0 <- df.MD.boundary$long - MD.x.min
df.MD.boundary$lat.0 <- df.MD.boundary$lat - MD.y.min
df.MD.boundary$drainage.density <- "MD"
df.MD.inset$long.0 <- df.MD.inset$long - MD.x.min
df.MD.inset$lat.0 <- df.MD.inset$lat - MD.y.min
df.MD.inset$drainage.density <- "MD"
df.MD.streams$long.0 <- df.MD.streams$long - MD.x.min
df.MD.streams$lat.0 <- df.MD.streams$lat - MD.y.min
df.MD.streams$drainage.density <- "MD"
df.MD.wells$long.0 <- df.MD.wells$X_world_co - MD.x.min
df.MD.wells$lat.0 <- df.MD.wells$Y_world_co - MD.y.min
df.MD.wells$drainage.density <- "MD"

HD.x.min <- min(df.HD.boundary$long)
HD.y.min <- min(df.HD.boundary$lat)
df.HD.boundary$long.0 <- df.HD.boundary$long - HD.x.min
df.HD.boundary$lat.0 <- df.HD.boundary$lat - HD.y.min
df.HD.boundary$drainage.density <- "HD"
df.HD.inset$long.0 <- df.HD.inset$long - HD.x.min
df.HD.inset$lat.0 <- df.HD.inset$lat - HD.y.min
df.HD.inset$drainage.density <- "HD"
df.HD.streams$long.0 <- df.HD.streams$long - HD.x.min
df.HD.streams$lat.0 <- df.HD.streams$lat - HD.y.min
df.HD.streams$drainage.density <- "HD"
df.HD.wells$long.0 <- df.HD.wells$X.world_coord.m. - HD.x.min
df.HD.wells$lat.0 <- df.HD.wells$Y.world_coord.m. - HD.y.min
df.HD.wells$drainage.density <- "HD"

# combine into one
df.boundary <- rbind(df.HD.boundary, df.MD.boundary, df.LD.boundary)
df.inset <- rbind(df.HD.inset, df.MD.inset, df.LD.inset)
df.streams <- rbind(df.HD.streams, df.MD.streams, df.LD.streams)
df.wells <- rbind(df.HD.wells[,c("well", "long.0", "lat.0", "drainage.density")], 
                  df.MD.wells[,c("well", "long.0", "lat.0", "drainage.density")], 
                  df.LD.wells[,c("well", "long.0", "lat.0", "drainage.density")])

# set factor level
df.boundary$drainage.density <- factor(df.boundary$drainage.density, levels=c("HD", "MD", "LD"))
df.inset$drainage.density <- factor(df.inset$drainage.density, levels=c("HD", "MD", "LD"))
df.streams$drainage.density <- factor(df.streams$drainage.density, levels=c("HD", "MD", "LD"))
df.wells$drainage.density <- factor(df.wells$drainage.density, levels=c("HD", "MD", "LD"))

##### make maps #####
p.map <- 
  ggplot() +
  geom_polygon(data=df.inset, aes(x=long.0, y=lat.0, group=group), fill="gray65", color=NA) +
  geom_polygon(data=df.boundary, aes(x=long.0, y=lat.0, group=group, color=drainage.density), fill=NA) +
  geom_path(data=df.streams, aes(x=long.0, y=lat.0, group=group), color="#313695") +
  geom_point(data=df.wells, aes(x=long.0, y=lat.0), shape=1) +
  facet_wrap(~drainage.density, ncol=3) +
  scale_color_manual(values=pal.density, guide=F) +
  coord_equal() +
  theme_scz() +
  theme(panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        strip.text=element_blank(),
        strip.background=element_blank())

ggsave(paste0(dir.fig, "Figure_Map_StudySite_Subdomains_NoLabels.pdf"),
       p.map, 
       width=176, height=120, units="mm", device=cairo_pdf)

# 
# p.HD.map <- 
#   ggplot() +
#   geom_polygon(data=df.HD.inset, aes(x=long.0, y=lat.0, group=group), fill="gray65", color=NA) +
#   geom_polygon(data=df.HD.boundary, aes(x=long.0, y=lat.0, group=group), fill=NA, color=col.HD) +
#   geom_path(data=df.HD.streams, aes(x=long.0, y=lat.0, group=group), color="#313695") +
#   geom_point(data=df.HD.wells, aes(x=long.0, y=lat.0), shape=1) +
#   coord_equal() +
#   theme_scz()
# 
# p.MD.map <- 
#   ggplot() +
#   geom_polygon(data=df.MD.inset, aes(x=long.0, y=lat.0, group=group), fill="gray65", color=NA) +
#   geom_polygon(data=df.MD.boundary, aes(x=long.0, y=lat.0, group=group), fill=NA, color=col.MD) +
#   geom_path(data=df.MD.streams, aes(x=long.0, y=lat.0, group=group), color="#313695") +
#   geom_point(data=df.MD.wells, aes(x=long.0, y=lat.0), shape=1) +
#   coord_equal() +
#   theme_scz()
# 
# p.LD.map <- 
#   ggplot() +
#   geom_polygon(data=df.LD.inset, aes(x=long.0, y=lat.0, group=group), fill="gray65", color=NA) +
#   geom_polygon(data=df.LD.boundary, aes(x=long.0, y=lat.0, group=group), fill=NA, color=col.LD) +
#   geom_path(data=df.LD.streams, aes(x=long.0, y=lat.0, group=group), color="#313695") +
#   geom_point(data=df.LD.wells, aes(x=long.0, y=lat.0), shape=1) +
#   coord_equal() +
#   theme_scz()
