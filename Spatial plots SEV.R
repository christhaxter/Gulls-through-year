###########################################################################
# LBBG Annual vulnerability paper 18/03/2019
#
# Plotting sensitivity and vulnerability surfaces from 200.5 model A
#
###########################################################################

library(data.table)
library(lattice); library(foreign)
library(shapefiles); library(sp)
library(maptools); library(maps)
library(RColorBrewer); library(proj4)
library(grid);
library(PBSmapping); library(maptools) 
library(raster)
library(rgeos)
library(rgdal)


# read in a world shapefile used for plotting

### SET PROJECTION FOR EACH SHAPEFILE READ IN
setwd("Y:/Gull Tracking/Data/GIS/")
world <- readOGR(getwd(),layer="country")
crop_lim = extent(c(-12,20), c(10,70))
world <- crop(world,crop_lim)  
world <- spTransform(world,CRS("+proj=utm +zone=31N +ellps=GRS80 +units=m +no_defs"))
x11(); plot(world,col="grey75",border="grey85",xpd=T,lwd=0.4)


#######################################
# Base raster
#######################################
# extent
xx1<--2200000; xx2<-700000	
yy1<-1500000; yy2<-6400000

proj4stringA <- CRS("+proj=utm +zone=31N +ellps=GRS80 +units=m +no_defs")
r <- raster(nrow=40, ncol=80, xmn=xx1, xmx=xx2, ymn=yy1, ymx = yy2, crs=proj4stringA) ### create a raster at the extent you want
r[] <- rep(0,ncell(r)) ### add ficticious data
res(r) <- 20000	# resolution, 20 km
values(r) <- 1:ncell(r)
pol <- rasterToPolygons(r)	### create a polygon from the raster



#######################################
# Set colony
#######################################
#colony <- "Skokholm"
#colony <- "Walney"
colony <- "Orford Ness"


#######################################
# Prediction grid
#######################################

setwd("A:/")
path.in <- "predgrid_CCC_firstyear.csv"
path.in <- gsub("CCC",colony,path.in)
pred.grid <- read.csv(path.in,sep=",",header=T)
grid.out <- data.frame(dum = rep(0,ncell(r)))
grid.out$square <- 1:length(rep(0,ncell(r)))

######### QUANTILE FUNCTION FOR PLOTTING GRIDS
plotquant <- function(r.obs0,whatplot,Colony,plotlocal = TRUE,adjust.zero=TRUE){
  
  if(plotlocal == TRUE) {x11()}
  par(mar= (c(0,0,0,0)))
  plot(world,col="grey75",border="grey85",xpd=TRUE,lwd=0.4,xlim=c(xx1,xx2),ylim=c(yy1,yy2))
  
  r.obs <- r.obs0
  r.obs[] <- ifelse(r.obs[] == -999,NA,r.obs[])
  b <- as.numeric(quantile(r.obs[], probs=c(0, 0.3, 0.5, 0.7, 0.80, 0.85, 0.90, 0.95, 0.98, 0.99, 0.999, 1),na.rm=T))
  
  fr <- rainbow(length(b))[(length(b)-3):1]; fr <- c("blue3",fr,"darkred")
  
  ppol1 <- ppol2 <- ppol3 <- ppol4 <- ppol5 <- ppol6 <- ppol7 <- ppol8 <- ppol9 <- ppol10 <- ppol11 <-  ppol12 <- NULL
  try(ppol1 <- rasterToPolygons(r.obs, fun=function(x){x >= b[1] & x < b[2]}),silent=T)
  try(ppol2 <- rasterToPolygons(r.obs, fun=function(x){x >= b[2] & x <  b[3]}),silent=T)
  try(ppol3 <- rasterToPolygons(r.obs, fun=function(x){x >= b[3] & x <  b[4]}),silent=T)
  try(ppol4 <- rasterToPolygons(r.obs, fun=function(x){x >= b[4] & x <  b[5]}),silent=T)
  try(ppol5 <- rasterToPolygons(r.obs, fun=function(x){x >= b[5] & x <  b[6]}),silent=T)
  try(ppol6 <- rasterToPolygons(r.obs, fun=function(x){x >= b[6] & x <  b[7]}),silent=T)
  try(ppol7 <- rasterToPolygons(r.obs, fun=function(x){x >= b[7] & x <  b[8]}),silent=T)
  try(ppol8 <- rasterToPolygons(r.obs, fun=function(x){x >= b[8] & x <= b[9]}),silent=T)
  try(ppol9 <- rasterToPolygons(r.obs, fun=function(x){x >= b[9] & x <= b[10]}),silent=T)
  try(ppol10 <- rasterToPolygons(r.obs, fun=function(x){x >= b[10] & x <= b[11]}),silent=T)
  try(ppol11 <- rasterToPolygons(r.obs, fun=function(x){x >= b[11] & x <= b[12]}),silent=T) 
  #try(ppol12 <- rasterToPolygons(r.obs, fun=function(x){x >= b[12] & x <= b[13]}),silent=T) 
  
  if(!is.null(ppol1)){plot(ppol1,add=T,col=fr[1],xpd=T,border="grey60",lwd=0.45)};
  if(!is.null(ppol2)){plot(ppol2,add=T,col=fr[2],xpd=T,border="grey60",lwd=0.45)}; 
  if(!is.null(ppol3)){plot(ppol3,add=T,col=fr[3],xpd=T,border="grey60",lwd=0.45)}; 
  if(!is.null(ppol4)){plot(ppol4,add=T,col=fr[4],xpd=T,border="grey60",lwd=0.45)}; 
  if(!is.null(ppol5)){plot(ppol5,add=T,col=fr[5],xpd=T,border="grey60",lwd=0.45)}; 
  if(!is.null(ppol6)){plot(ppol6,add=T,col=fr[6],xpd=T,border="grey60",lwd=0.45)}; 
  if(!is.null(ppol7)){plot(ppol7,add=T,col=fr[7],xpd=T,border="grey60",lwd=0.45)}; 
  if(!is.null(ppol8)){plot(ppol8,add=T,col=fr[8],xpd=T,border="grey60",lwd=0.45)}; 
  if(!is.null(ppol9)){plot(ppol9,add=T,col=fr[9],xpd=T,border="grey60",lwd=0.45)}; 
  if(!is.null(ppol10)){plot(ppol10,add=T,col=fr[10],xpd=T,border="grey60",lwd=0.45)}; 
  if(!is.null(ppol11)){plot(ppol11,add=T,col=fr[11],xpd=T,border="grey60",lwd=0.45)}
  #if(!is.null(ppol12)){plot(ppol12,add=T,col=fr[12],xpd=T,border="grey60",lwd=0.45)}
  
  # if NA data within the swathe (vulnerability) plot as white
  ppoll13 <- NULL
  try(ppol13 <- rasterToPolygons(r.obs0, fun=function(x){x == -999}),silent=T) 
  if(!is.null(ppol13)){plot(ppol13,add=T,col="white",xpd=T,border="grey60",lwd=0.45)}
  
  plot(world,border="grey85",xpd=TRUE,add=TRUE,lwd=0.5)
  
  ## LEGEND FOR THE VALUES PLOTTED AND COLOUR RAMP
  
  if(min(r.obs0[],na.rm=TRUE) == -999){
    bn <- c("0%",">0%+", "30%", "50%", "70%", "80%", "85%", "90%", "95%", "98%", "99%","99.9%")
    legend(xx2-180000,yy1+1150000,bn,bty="n",fill=c("white",fr),xpd=TRUE,cex=0.8,border=c("white",fr))
  }
  if(min(r.obs0[],na.rm=TRUE) != -999){
    bn <- c("0%+", "30%", "50%", "70%", "80%", "85%", "90%", "95%", "98%", "99%","99.9%")
    legend(xx2-180000,yy1+1000000,bn,bty="n",fill=fr,xpd=TRUE,cex=0.8,border=fr)
  }
  # labels at the top - what are we plotting and for what colony
  legend("topright", inset=c(0.005,-0.01), whatplot, bty="n",cex=0.8,xpd=TRUE)  
  legend("topleft", inset=c(-0.045,-0.01), Colony, bty="n",cex=0.8,xpd=TRUE)  
  
}

# sensible plotting extent based on projected UTM 31N
xx1<<--700000; xx2<<-500000		
yy1<<-3600000; yy2<<-6200000


##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
# SENSITIVITY
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################

###########################################################
# MODEL OUTPUT - sensitivity
###########################################################
# code to sum up the raw predictions
setwd("A:/")
mod.data.in <- "CCC.200.5.csv"
mod.data.in <- gsub("CCC",colony,mod.data.in)
data <- read.csv(mod.data.in,sep=",",header=T)
data$RSE <- data$pred.se / data$pred
data2 <- with(data,aggregate(pred,by = list(julian,square), 'sum'))
names(data2)[c(1,2,3)] <- c("julian","square","pred") # NOTE THIS WAY - issues of how to combine SEs!!
data3 <- with(data2,aggregate(pred,by = list(square), 'sum'))
names(data3)[c(1,2)] <- c("square","pred")

data3 <- data3[data3$square %in% pred.grid$square,] # If Orford Ness total grid surface was run

###########################################################
# Probability of collision
###########################################################

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
## Based on windpower data
## ADDED IN MISSING INFORMATION FOR TURBINE DATA NEEDED BY BAND MODEL
## ROTOR SPEEDS - USED POWER RELATIONSHIP
## BETWEEN CAPACITY AND RPM TO FILL IN GAPS FOR SOME SITES, AND REGRESSED
## MAX AND MIN RPM SPEED TO GET MIN RPM WHERE MISSING, AND THEN AVERAGED
## MAX AND MIN TO USE AVERAGE RPM
## ALSO WHERE HUB HEIGHT WAS MISSING, AVERAGED MAX AND MIN RANGE FOR THAT
## TURBINE SPEC, OR USED WHAT WAS ALREADY IN SPREADSHEET. MISSING DATA
## FOR SITES, USED AVERAGES FOR OFFSHORE AND ONSHORE FOR ALL ABOVE VARS.
## I.E. SIMILAR PROCESS TO THE CRW WHERE INFO MISSING.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

## FILE HERE NOT SUPPLIED AS SUPPLEMENTARY DUE TO DATA SHARING RESTRICTION
## CODE HERE TO SHOW WORKING
#
## NOTE THIS IS ONLY FOR THE COUNTRIES IN WHICH BIRD TRACKS OVERLAPPED
#wp <- read.csv("Turbines4probcoll.csv",sep=",",na.strings = "#ND")
#coll.dat <- wp
#p_coll = 0
#names(coll.dat)[which(names(coll.dat) == "Average.speed.rd.min.CT")] <- "Rotor.Speed"
#names(coll.dat)[which(names(coll.dat) == "Diameter.m")] <- "Rotor.Diameter"
#coll.dat$speed <- 9.95
#coll.dat$flapglide <- "flap"
#coll.dat$length <- 0.58
#coll.dat$wingspan <- 1.42
#coll3 <- coll2 <- NULL
#for (z in 1:nrow(coll.dat)) { # Code credit: Aonghais Cook
#  
#  print(paste(z,"out of",nrow(coll.dat)))
#  
#  ##### TURBINE DATA
#  k <- 1             # 0 = 1-dimension, 1 = 3-dimensions
#  no.blades <- coll.dat$N.blades
#  max.chord <- 3
#  pitch <- coll.dat$Pitch[z]
#  rotor.rad <- coll.dat$Rotor.Diameter[z]/2
#  speed <- coll.dat$Rotor.Speed[z]
#  rotor.period = 60 / speed
#  
#  ###### BIRD DATA
#  bird.length <- coll.dat$length[z]    # metres
#  wingspan <- coll.dat$wingspan[z]       # metres
#  flap.glide <- 0
#  bird.speed <- coll.dat$speed[z]  # m/s
#  aspect <- bird.length/wingspan
#  
#  ##### p(collision) data
#  radius = seq(0.05,1, 0.05)
#  chord = c(0.73, 0.79, 0.88, 0.96,1, 0.98, 0.92, 0.85, 0.8, 0.75, 0.7, 0.64, 0.58, 0.52, 0.47, 0.41, 0.37, 0.3, 0.24, 0)
#  alpha = bird.speed * rotor.period / (radius * 2 * rotor.rad * pi)
#  
#  ### upwind p(collision)
#  up_col_length = abs(max.chord * chord * sin(pitch*pi/180) + (alpha * max.chord * chord * cos(pitch*pi/180)))
#  up_col_length2 = vector()
#  up_p_collision = vector()
#  
#  for (i in 1:20){
#    ifelse(alpha[i] < aspect, bird.length + up_col_length[i] -> up_col_length2[i], (wingspan * alpha[i]) + up_col_length[i] -> up_col_length2[i])
#    up_p_collision[i] = min(1, (no.blades/rotor.period)*up_col_length2[i]/bird.speed)
#  }
# 
#  overall_up_p_collision = 2 * (sum(radius[1:19] * up_p_collision[1:19]) + up_p_collision[20]/2) * 0.05
#  
#  ### downwind p(collision)
#  down_col_length = abs(-max.chord * chord * sin(pitch*pi/180) + (alpha * max.chord * chord * cos(pitch*pi/180)))
#  down_col_length2 = vector()
#  down_p_collision = vector()
#  
#  for (i in 1:20){
#    ifelse(alpha[i] < aspect, bird.length + down_col_length[i] -> down_col_length2[i], (wingspan * alpha[i]) + down_col_length[i] -> down_col_length2[i])
#    down_p_collision[i] = min(1, (no.blades/rotor.period)*down_col_length2[i]/bird.speed)
#  }
#  
#  overall_down_p_collision = 2 * (sum(radius[1:19] * down_p_collision[1:19]) + down_p_collision[20]/2) * 0.05
#  
#  p_coll = (0.5 * overall_down_p_collision) + (0.5 * overall_up_p_collision)
#  
#  coll3 <- rbind(coll2,p_coll)
#  coll2 <- coll3
#  
#} # end loop through rows for this bird
#
#### identify the cell number for each point; "overlap" is a dataframe for each point with the square each point overlaps
#Point <- data.frame(X=coll.dat$X, Y=coll.dat$Y)
#sp2   <- SpatialPoints(Point)
#Overlap <- data.frame(square = cellFromXY(r,sp2),Point)
#coll.dat1 <- cbind(coll.dat,Overlap[1])
#
## take out wind farms lying outside the grid
#coll.dat2 <- coll.dat1[!is.na(coll.dat1$square),]
#
## subset for only pcoll
#coll.dat3 <- subset(coll.dat2,select=c(square,pcoll))
#coll.dat3 <- coll.dat3[order(coll.dat3$square),]
#
## take mean pcoll per square - obviously some sqs have more than one wind farm, or turbine type per site
#coll.dat3a <- with(coll.dat3,aggregate(pcoll,by=list(square),FUN='mean'))
#names(coll.dat3a)[c(1,2)] <- c("square","pcoll")
#
## now merge in the blank squares, for where there was NO wind farm
## add in the mean pcoll for those squares
#coll.dat4 <- data.frame(square = 1:length(unique(pol2$SID)))
#coll.dat5 <- merge(coll.dat4,coll.dat3a,by="square",all=TRUE)
#
## add in means for pcoll for squares with no turbines
## Note - this is obviously an assumption - ca. 0.061
#coll.dat5$pcoll <- ifelse(is.na(coll.dat5$pcoll),mean(coll.dat3$pcoll,na.rm=TRUE),coll.dat5$pcoll)
#
## Export
#write.table(coll.dat5,"pcoll.surface.csv",sep=",",col.names=TRUE,row.names=FALSE)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Sensitivity without Pcoll added
pred.grid2 <- merge(grid.out,data3,by="square",all=TRUE)
r.sen <- r
r.sen[] <- pred.grid2$pred
plotquant(r.sen,whatplot = "Sensitivity",Colony = colony,plotlocal=TRUE)

#######################################
# Read in Pcoll
#######################################
coll.dat5 <- read.csv("pcoll.surface.csv",sep=",",header=TRUE)
coll.dat5 <- coll.dat5[coll.dat5$square %in% pred.grid$square,]

# check spatially - merge pcoll into the swathe of squares for colony
pred.grid2 <- merge(grid.out,coll.dat5,by="square",all=TRUE)
r.pcoll <- r
r.pcoll[] <- pred.grid2$pcoll
plotquant(r.pcoll,whatplot = "Pcoll",Colony = colony,plotlocal=TRUE)

# merging in of pcoll
data4 <- merge(data3,coll.dat5,by="square")
data4$pred <- data4$pred * data4$pcoll

# check spatially
pred.grid2 <- merge(grid.out,data4,by="square",all=TRUE)
r.sen2 <- r
r.sen2[] <- pred.grid2$pred
plotquant(r.sen2,whatplot = "Pcoll",Colony = colony,plotlocal=TRUE)

# metric = distance travelled in CRW per sq (summed for year) * pcoll
x11(); hist(na.omit(pred.grid2$pred),breaks = 100) # very skewed distribution

#######################################
# Relative Standard Errors (of modelled grid prior to Pcoll)
#######################################
data22 <- with(data,aggregate(RSE,by = list(julian,square), 'mean'))
names(data22)[c(1,2,3)] <- c("julian","square","RSE") 
data33 <- with(data22,aggregate(RSE,by = list(square), 'mean')) 
names(data33)[c(1,2)] <- c("square","RSE")
data33 <- data33[data33$square %in% pred.grid$square,] # If Orford Ness total grid surface was run

# check spatially
pred.grid2 <- merge(grid.out,data33,by="square",all=TRUE)
r.rse <- r
r.rse[] <- pred.grid2$RSE
plotquant(r.rse,whatplot = "Pcoll",Colony = colony,plotlocal=TRUE)



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
# EXPOSURE
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################

#######################################
# Number of turbines per square
#######################################
# Note "raw" turbine data not supplied, aggregated number per square is provided
# Code for raw wind farm data manipulation 

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#igotwind <- function(){
#  
#  wind.locs <- read.csv("World_windfarms_chrisextra.csv",sep=",",na.strings = "#ND")
#  
#  # TRANSFORM TO PROJECTED
#  wind.locs.sub <- wind.locs[wind.locs$Continent == "Europe" | wind.locs$Continent == "Africa",]
#  wind.locs.sub <- wind.locs.sub[wind.locs.sub$Latitude > 0,]
#  wind.locs.sub <- wind.locs.sub[!is.na(wind.locs.sub$Latitude),]
#
#  newdat <- data.frame(Longitude = wind.locs.sub$Longitude,Latitude = wind.locs.sub$Latitude)
#  coordinates(newdat) <- ~ Longitude + Latitude
#  proj4string(newdat) <- CRS("+proj=longlat + ellps=WGS84 + datum=WGS84")
#  newdat <- spTransform(newdat,CRS("+proj=utm +zone=31N +ellps=GRS80 +units=m +no_defs"))  
#  wind.locs.sub$X <- newdat@coords[,1]
#  wind.locs.sub$Y <- newdat@coords[,2]
#  
#  # drop out anything not constructed or operational
#  wind.locs.sub1 <- wind.locs.sub[wind.locs.sub$Status == "Construction" | wind.locs.sub$Status == "Production",]
#  wind.locs.sub2 <- wind.locs.sub[wind.locs.sub$Status != "Construction" & wind.locs.sub$Status != "Production",]
#  
#  wind.locs.sub1
#}
#wind.locs.sub1 <- igotwind()
## process wf data
#Point <- data.frame(X=wind.locs.sub1$X, Y=wind.locs.sub1$Y)
#sp2   <- SpatialPoints(Point)
#Overlap <- data.frame(square = cellFromXY(r,sp2),Point)
#wind.locs <- cbind(wind.locs.sub1,Overlap[1])
#wind.locs2 <- subset(wind.locs,select=c(square,Number.of.turbines))
#wind.locs3 <- with(wind.locs2,aggregate(Number.of.turbines,by=list(square),sum))
#names(wind.locs3)[c(1,2)] <- c("square","n")
#wind.locs3 <- wind.locs3[order(wind.locs3$square),]
#
## save this layer of n. turbines per square
#write.table(wind.locs3,"n.turb.csv",sep=",",col.names=TRUE,row.names=FALSE)


# read in the dataset of no. turbines per square
wind.locs3 <- read.csv("n.turb.csv",sep=",",header=TRUE)

# check wind turbines spatially
pred.grid2 <- merge(grid.out,wind.locs3,by="square",all=TRUE)
r.tubs <- r
r.tubs[] <- pred.grid2$n
plotquant(r.tubs,whatplot = "N turbines",Colony = colony,plotlocal=TRUE)


##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
# VULNERABILITY
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################

##############################################
# merge in n.turbs into sensitivity grid
##############################################
data5 <- merge(data4,wind.locs3,by="square")
data5$pred <- data5$pred * data5$n
data5$pred <-  ifelse(is.na(data5$pred),0,data5$pred)
data5 <- merge(data5,pred.grid,by="square",all=TRUE)
data5$pred <-  ifelse(is.na(data5$pred),-999,data5$pred)

# check vulnerability surface spatially
pred.grid2 <- merge(grid.out,data5,by="square",all=TRUE)
r.vuln <- r
r.vuln[] <- pred.grid2$pred
plotquant(r.vuln,whatplot = "Vulnerability",Colony = colony,plotlocal=TRUE)



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
# RAW DATA CHECK
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################

##################################################
# read in raw data that was modelled
##################################################

setwd("A:/")
data <- read.csv(paste0(colony,".csv"),sep=",",header=TRUE)

# mean across birds
data44 <- with(data,aggregate(dist.risk.secs,by = list(julian,square), 'mean'))
names(data44)[c(1,2,3)] <- c("julian","square","dist.risk.secs") # NOTE THIS WAY - issues of how to combine SEs!!

# sum across the year for each square
data44 <- with(data44,aggregate(dist.risk.secs,by = list(square), 'sum'))
names(data44)[c(1,2)] <- c("square","dist.risk.secs")

pred.grid.obs <- merge(grid.out,data44,by="square",all=TRUE)
r.obs <- r
r.obs[] <- pred.grid.obs$dist.risk.secs
plotquant(r.obs,whatplot = "Distance in CRW (km)",Colony = colony,plotlocal=TRUE)


###########################################
# Export examples of data for Github
###########################################

# raw data
write.table(data44,"Walney.sp.agg.raw.csv",col.names=TRUE,row.names=FALSE)
data44 <- read.csv("Walney.sp.agg.raw.csv",header=TRUE)

# sensitivity without Pcol
write.table(data3,"Walney.sp.agg.csv",sep=",",col.names=TRUE,row.names=FALSE)
data3 <- read.csv("Walney.sp.agg.csv",header=TRUE)






##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
# MAPS FOR PAPER
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################



if(t1 == T){legend(pos1, inset=c(0.005,-0.01), h, bty="n",cex=CEX) } # Add a legend
if(t2 == T){legend(pos2, inset=c(-0.045,-0.01), Colony2, bty="n",cex=CEX) } # Add a legend







# --------------------------------------------------------------- #
# PLOTTING HERE
# --------------------------------------------------------------- #

CEX <<- 0.8
pos1 <<- "topright"
pos2 <<- "topleft"

w.cm <- 12
h.cm <- 12
#h.cm <- 6

xx1<<--700000; xx2<<-500000		
yy1<<-3600000; yy2<<-6200000

path.plot <- "DR:/Gull Tracking/DECC01 gulls skuas and windfarms/papers/Migration_appliedpaper/outputs/maps/New.outputmaps.13082018/Fig2.exposure.tif"
path.plot <- gsub("DR",Drive,path.plot)
plot.store.tiff <- paste(path.plot)
tiff(plot.store.tiff, width=w.cm, height=h.cm, units="cm", pointsize=9, res=600, compression ="lzw") 

# MULTI MAPS
#par(mfrow=c(1,3))
par(mfrow=c(2,3))

h <- "Sensitivity"
# PLOT EXPOSURE SURFACES
years <<- "first"; Colony <<- "Orford Ness"; get.grids("Orford Ness",se=F); plot.grids("e"); box()
years <<- "all"; Colony <<- "Walney"; get.grids("Walney",se=F); plot.grids("e"); box()
years <<- "all"; Colony <<- "Skokholm"; get.grids("Skokholm",se=F); plot.grids("e"); box()

h <- "RSE"
# PLOT RELATIVE STANDARD ERRORS
years <<- "first"; Colony <<- "Orford Ness"; get.grids("Orford Ness",se=T); plot.grids(type = "e"); box()
years <<- "all"; Colony <<- "Walney"; get.grids("Walney",se=T); plot.grids("e"); box()
years <<- "all"; Colony <<- "Skokholm"; get.grids("Skokholm",se=T); plot.grids("e"); box()

dev.off()


x11()

# --------------------------------------------------------------- #
# PLOTTING HERE
# --------------------------------------------------------------- #

CEX <<- 0.8
pos1 <<- "topright"
pos2 <<- "topleft"

w.cm <- 12
h.cm <- 6

xx1<<--700000; xx2<<-500000		
yy1<<-3600000; yy2<<-6200000

path.plot <- "DR:/Gull Tracking/DECC01 gulls skuas and windfarms/papers/Migration_appliedpaper/outputs/maps/New.outputmaps.13082018/Fig3b.vulnerability.tif"
path.plot <- gsub("DR",Drive,path.plot)
plot.store.tiff <- paste(path.plot)
tiff(plot.store.tiff, width=w.cm, height=h.cm, units="cm", pointsize=9, res=600, compression ="lzw") 

# MULTI MAPS
par(mfrow=c(1,3))
#par(mfrow=c(2,3))

h <- "Vulnerability"

# PLOT VULNERABILITY SURFACES
years <<- "first"; Colony <<- "Orford Ness"; get.grids("Orford Ness",se="V"); plot.grids("v"); box()
years <<- "all"; Colony <<- "Walney"; get.grids("Walney",se="V"); plot.grids("v"); box()
years <<- "all"; Colony <<- "Skokholm"; get.grids("Skokholm",se="V"); plot.grids("v"); box()


dev.off()




























































##########################################################################
# model of weighted UK vs non-weighted
#
#wr <- readRDS("Walney.200.5_WR.rds")
#nwr <- readRDS("Walney.200.5.rds")
#
#summary(wr)
#summary(nwr)
#
#r.mod.wr <- r.mod
#r.mod <- r.mod
#
#al <- data.frame(square = 1:35525,wr = r.mod.wr@data@values, or = r.mod@data@values)
#al <- na.omit(al)
#
#View(al)
#
#setwd("Y:/Gull Tracking/Data/GIS/")
#world <- readOGR(getwd(),layer="country")
#crop_lim = extent(c(-12,6), c(45,65)) # we modelled habitat use around 100 km around the breeding colony encompassing previous 95% total area use estimates from previous analyses.....
#world.crop <- crop(world, crop_lim)
#world.crop <- spTransform(world.crop,proj4stringA)
#pol.crop <- crop(pol,extent(world.crop))
#IDs <- extract(r, extent(world.crop)) # get raster IDs for the world.crop extent
#
#al.uk <- al[al$square %in% IDs,]
#
#View(al.uk)


#x11(); plot(al.uk$wr,al.uk$or)






