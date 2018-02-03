## Figure_Baseline_Scatter+Dens.R
#' Scatter and density plots comparing MODFLOW and analytical results.
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
                               bias = pbias(depletion.prc, depletion.prc.modflow),
                               R2 = R2(depletion.prc, depletion.prc.modflow),
                               MSE.bias = MSE.bias(depletion.prc, depletion.prc.modflow),
                               MSE.var = MSE.var(depletion.prc, depletion.prc.modflow),
                               MSE.cor = MSE.cor(depletion.prc, depletion.prc.modflow),
                               MSE.bias.norm = MSE.bias.norm(depletion.prc, depletion.prc.modflow),
                               MSE.var.norm = MSE.var.norm(depletion.prc, depletion.prc.modflow),
                               MSE.cor.norm = MSE.cor.norm(depletion.prc, depletion.prc.modflow),
                               MSE.overall = MSE(depletion.prc, depletion.prc.modflow),
                               KGE.overall = KGE(depletion.prc, depletion.prc.modflow, method="2012"))

## table of KGE
df.fit.table <- dcast(df.fit.ByScenario[,c("drainage.density", "topography", "recharge", "method", "KGE.overall")],
                      drainage.density + topography + recharge ~ method, value.var="KGE.overall")
df.fit.table[,4:8] <- round(df.fit.table[,4:8], 3)

## table of MSE
df.MSE.table <- dcast(df.fit.ByScenario[,c("drainage.density", "topography", "recharge", "method", "MSE.overall")],
                      drainage.density + topography + recharge ~ method, value.var="MSE.overall")
df.MSE.table[,4:8] <- round(df.MSE.table[,4:8], 1)

## table of bias
df.bias.table <- dcast(df.fit.ByScenario[,c("drainage.density", "topography", "recharge", "method", "bias")],
                      drainage.density + topography + recharge ~ method, value.var="bias")
df.bias.table[,4:8] <- round(df.bias.table[,4:8], 3)

## test...
df.test <- subset(df.prc, drainage.density=="LD" & topography=="ELEV" & recharge=="RCH1000")
df.fit.test <- summarize(group_by(df.test, method),
                         n.reach = sum(is.finite(depletion.prc)),
                         cor = cor(depletion.prc, depletion.prc.modflow, method="pearson"),
                         bias = pbias(depletion.prc, depletion.prc.modflow),
                         R2 = R2(depletion.prc, depletion.prc.modflow),
                         MSE.bias = MSE.bias(depletion.prc, depletion.prc.modflow),
                         MSE.var = MSE.var(depletion.prc, depletion.prc.modflow),
                         MSE.cor = MSE.cor(depletion.prc, depletion.prc.modflow),
                         MSE.bias.norm = MSE.bias.norm(depletion.prc, depletion.prc.modflow),
                         MSE.var.norm = MSE.var.norm(depletion.prc, depletion.prc.modflow),
                         MSE.cor.norm = MSE.cor.norm(depletion.prc, depletion.prc.modflow),
                         MSE.overall = MSE(depletion.prc, depletion.prc.modflow),
                         KGE.overall = KGE(depletion.prc, depletion.prc.modflow, method="2012"))
