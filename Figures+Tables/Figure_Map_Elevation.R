## Figure_Map_Elevation.R
#' Make figure showing maps of elevation, head, and WTD for the LD ELEV NORCH scenario.
#' 
#' This script requires output from Depletion_02_CalculateWTD.R and Nanaimo_RunModels_RechargeSensitivity.py

rm(list=ls())

# directory to repository on local computer
dir.git <- "C:/Users/Sam/WorkGits/NanaimoAttributionMethods/"

# load paths + packages
source(paste0(dir.git, "ProcessingScripts/paths+packages.R"))

## load data
# WTD data manually exported from Visual MODFLOW
df.WTD <- read.csv(paste0(dir.git, "data/Depletion_02_CalculateWTD.csv"))

# WTD data calculated with FloPy in Nanaimo_RunModels_RechargeSensitivity.py
WTD.0 <- read.csv(paste0(dir.GSAS, "models/LD/ELEV/NORCH/wtd.txt"), header=F)
WTD.10 <- read.csv(paste0(dir.GSAS, "models/LD/ELEV/RCH10/wtd.txt"), header=F)
WTD.100 <- read.csv(paste0(dir.GSAS, "models/LD/ELEV/RCH100/wtd.txt"), header=F)
WTD.1000 <- read.csv(paste0(dir.GSAS, "models/LD/ELEV/RCH1000/wtd.txt"), header=F)

df.WTD$NORCH <- c(t(WTD.0))
df.WTD$RCH10 <- c(t(WTD.10))
df.WTD$RCH100 <- c(t(WTD.100))
df.WTD$RCH1000 <- c(t(WTD.1000))

df.WTD.melt <- melt(df.WTD[,c("long", "lat", "NORCH", "RCH10", "RCH100", "RCH1000")], 
                    id=c("long", "lat"), value.name="WTD", variable.name="recharge")

# prep streamlines
shp.LD.streams <- readOGR(paste0(dir.GSAS, "GIS"), layer="LD_streams")
df.LD.streams <- tidy(shp.LD.streams)

## plots
p.elev <- 
  ggplot() +
  geom_raster(data=df.WTD, aes(x=long, y=lat, fill=elev.ground.m)) +
  geom_path(data=df.LD.streams, aes(x=long, y=lat, group=group), color="black") +
  scale_fill_distiller(name="(a) Elevation [m]", palette="Spectral", na.value=NA,
                       limits=c(min(df.WTD$elev.ground.m, na.rm=T), max(df.WTD$elev.ground.m, na.rm=T))) +
  scale_x_continuous(name="Easting [m]", expand=c(0,0)) +
  scale_y_continuous(name="Northing [m]", expand=c(0,0)) +
  coord_equal() +
  theme_scz() +
  theme(legend.position="top") +
  guides(fill=guide_colorbar(title.position="top", title.hjust=0.5))

p.WTD <- 
  ggplot() +
  geom_raster(data=df.WTD.melt, aes(x=long, y=lat, fill=WTD)) +
  geom_path(data=df.LD.streams, aes(x=long, y=lat, group=group), color="black") +
  facet_wrap(~recharge, ncol=4, 
             labeller=as_labeller(c("NORCH"="(b) 0 mm", "RCH10"="(c) 10 mm", "RCH100"="(d) 100 mm", "RCH1000"="(e) 1000 mm"))) +
  scale_fill_gradient2(name="(b-e) Water Table Depth [m]", low=pal.depletion.0to100[1], 
                       mid=pal.depletion.0to100[6], high=pal.depletion.0to100[11], na.value=NA) +
  scale_x_continuous(name="Easting [m]", expand=c(0,0)) +
  scale_y_continuous(name="Northing [m]", expand=c(0,0)) +
  coord_equal() +
  theme_scz() +
  theme(legend.position="top") +
  guides(fill=guide_colorbar(title.position="top", title.hjust=0.5))

# save output
ggsave(paste0(dir.fig, "Figure_Map_Elevation.png"),
       grid.arrange(p.elev+theme(axis.title=element_blank(), axis.text=element_blank(), 
                                 plot.margin=unit(c(0,0,2.5,0), "mm"),
                                 legend.box.margin=unit(c(2,0,4.5,0), "mm")), 
                    p.WTD+theme(axis.title=element_blank(), axis.text=element_blank()), 
                    ncol=2, widths=c(0.21, 0.8)), 
       width=190, height=100, units="mm", dpi=300)

ggsave(paste0(dir.fig, "Figure_Map_Elevation.pdf"),
       grid.arrange(p.elev+theme(axis.title=element_blank(), axis.text=element_blank(), 
                                 plot.margin=unit(c(0,0,2.5,0), "mm"),
                                 legend.box.margin=unit(c(2,0,4.5,0), "mm")), 
                    p.WTD+theme(axis.title=element_blank(), axis.text=element_blank()), 
                    ncol=2, widths=c(0.21, 0.8)), 
       width=190, height=100, units="mm", device=cairo_pdf)
