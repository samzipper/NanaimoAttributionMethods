## Figure_Map_Elevation.R
#' Make figure showing maps of elevation, head, and WTD for the LD ELEV NORCH scenario.
#' 
#' This script requires output from Depletion_01_AggregateAllResults.R

rm(list=ls())

# directory to repository on local computer
dir.git <- "C:/Users/Sam/WorkGits/NanaimoAttributionMethods/"

# load paths + packages
source(paste0(dir.git, "ProcessingScripts/paths+packages.R"))

## load data
df.WTD <- read.csv(paste0(dir.git, "data/Depletion_02_CalculateWTD.csv"))
shp.LD.streams <- readOGR(paste0(dir.GSAS, "GIS"), layer="LD_streams")

# conversions
df.LD.streams <- tidy(shp.LD.streams)

## plots
p.elev <- 
  ggplot() +
  geom_raster(data=df.WTD, aes(x=long, y=lat, fill=elev.ground.m)) +
  geom_path(data=df.LD.streams, aes(x=long, y=lat, group=group), color="cyan") +
  scale_fill_distiller(name="(a) Elevation [m]", palette="Spectral", na.value=NA,
                       limits=c(min(df.WTD$elev.ground.m, na.rm=T), max(df.WTD$elev.ground.m, na.rm=T))) +
  scale_x_continuous(name="Easting [m]", expand=c(0,0)) +
  scale_y_continuous(name="Northing [m]", expand=c(0,0)) +
  coord_equal() +
  theme_scz() +
  theme(axis.text.y=element_text(angle=90, hjust=0.5),
        legend.position="top") +
  guides(fill=guide_colorbar(title.position="top", title.hjust=0.5))

p.head <- 
  ggplot() +
  geom_raster(data=df.WTD, aes(x=long, y=lat, fill=head.m)) +
  geom_path(data=df.LD.streams, aes(x=long, y=lat, group=group), color="cyan") +
  scale_fill_distiller(name="(b) Head [m]", palette="Spectral", na.value=NA,
                       limits=c(min(df.WTD$elev.ground.m, na.rm=T), max(df.WTD$elev.ground.m, na.rm=T))) +
  scale_x_continuous(name="Easting [m]", expand=c(0,0)) +
  scale_y_continuous(name="Northing [m]", expand=c(0,0)) +
  coord_equal() +
  theme_scz() +
  theme(axis.text.y=element_text(angle=90, hjust=0.5),
        legend.position="top") +
  guides(fill=guide_colorbar(title.position="top", title.hjust=0.5))

p.WTD <- 
  ggplot() +
  geom_raster(data=df.WTD, aes(x=long, y=lat, fill=WTD.m)) +
  geom_path(data=df.LD.streams, aes(x=long, y=lat, group=group), color="cyan") +
  scale_fill_gradient2(name="(c) Water Table Depth [m]", low=pal.depletion.0to100[1], 
                       mid=pal.depletion.0to100[6], high=pal.depletion.0to100[11], na.value=NA) +
  scale_x_continuous(name="Easting [m]", expand=c(0,0)) +
  scale_y_continuous(name="Northing [m]", expand=c(0,0)) +
  coord_equal() +
  theme_scz() +
  theme(axis.text.y=element_text(angle=90, hjust=0.5),
        legend.position="top") +
  guides(fill=guide_colorbar(title.position="top", title.hjust=0.5))


# align scatter and density plots
p1 <- ggplotGrob(p.elev)
p2 <- ggplotGrob(p.head)
p3 <- ggplotGrob(p.WTD)
p <- cbind(p1, p2, p3, size="first")
p$heights <- unit.pmax(p1$heights, p2$heights, p3$heights)

# save output
ggsave(paste0(dir.fig, "Figure_Map_Elevation.png"),
       p, width=176, height=120, units="mm", dpi=300)

ggsave(paste0(dir.fig, "Figure_Map_Elevation.pdf"),
       p, width=176, height=120, units="mm", device=cairo_pdf)
