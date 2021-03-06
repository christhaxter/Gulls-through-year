###################################################################
# LBBG annual vulnerability paper 18/03/2019
# Example model structure, reading in colony.WR.csv raw data dist.risk.secs, modelling sensitivity over xyt
# for square i, julian date j. Zero data for squares within the swathe per day per year
# Model weights - geographically weighted for UK, using proportion of birds with active tags vs total
##################################################################

library(mgcv)
library(parallel)

####################################################################
# SET COLONY 
####################################################################
#colony <- "Skokholm"
#colony <- "Walney"
colony <- "Orford Ness"

# WALNEY
if(colony == "Walney"){birds <- c(5024,5026,5025,5027,5029,503,5033,5034,5023,4032,506,4034,4033,497,501,504)}
if(colony == "Skokholm"){birds <- c(1511,4000,4009,4022,4023,4024,5000,5001,5015,5002,5003,5004,5006,5009,5011,5013,5017,5021,5022)}
if(colony == "Orford Ness"){birds <- c(334,336,388,391,395,407,459,460,478,479,480,482,483,484,485,486,492,493)}

####################################################################
# Read in the data 
####################################################################
#setwd("E:/Gull Tracking/DECC01 gulls skuas and windfarms/papers/Migration_appliedpaper/outputs/GAMs/stupid subsetting/")
##############################################################

#setwd("A:/")
mod.data.in <- "CCC.WR.csv"
mod.data.in <- gsub("CCC",colony,mod.data.in)
data <- read.csv(mod.data.in,sep=",",header=T)
data <- data[data$bird %in% birds,]
data$bird <- as.factor(data$bird)
data$dum <- 1

####################################################################
# Read in the base prediction file from program: Get.prediction.grid.R 
####################################################################
#setwd("E:/Gull Tracking/DECC01 gulls skuas and windfarms/papers/Migration_appliedpaper/outputs/ppi grid exports/")
path.in <- "predgrid_CCC.csv"
path.in <- gsub("CCC",colony,path.in)
pred.grid <- read.csv(path.in,sep=",",header=T)
sub <- subset(pred.grid,select=c(square,Longitude,Latitude))
data2 <- merge(data,sub,by="square")

#####################################################################
# MODEL 
#####################################################################
# run through ALL birds for chosen colony
detectCores()
if (detectCores()>1) {
cl <- makeCluster(detectCores()-1)
} else cl <- NULL

bin <- bam(dist.risk.secs ~ 
	te(Longitude, Latitude,julian,bs=c("tp","cc"),d=c(2,1),k=c(200,5))+ #25,100
	s(bird, by=dum, bs = "re"), 
	data=data2,
	family=Tweedie(p=1.2, link = power(0)),
	cluster=cl,
	chunk.size=5000,
	w = prop,
	control = list(trace = T, maxit = 20,epsilon=0.1,newton=list(conv.tol=0.01)) # bam best
	)

unique(data$bird)
stopCluster(cl)

str(data2)

# save the model
out.mod.path <- paste("CCC.200.5_WR",".rds",sep="")
out.mod.path <- gsub("CCC",colony,out.mod.path)
saveRDS(bin,out.mod.path)

# ------------------------------------- #
# OUTPUT plotting paper files
# ------------------------------------- #

# Prediction code needs a bird chosen to go into the model even though dum=0 avrages over birds
if(colony == "Walney"){bird.dum <- 4034}
if(colony == "Skokholm"){bird.dum <- 5000}
if(colony == "Orford Ness"){bird.dum <- 395}

# cycle through 1:365 days and output predictions for each square across birds
ppi <- ppi1 <- ppi2 <- NULL
for(i in 1:365){
  pred.grid2 <- pred.grid
  pred.grid2$bird <- bird.dum
  pred.grid2$dum <- 0
  pred.grid2$julian <- i
  #pred.grid2$year <- 2012
  preds <- predict.gam(bin, pred.grid2,type="response",se.fit=T)
  pred.grid2$pred <- as.vector(preds$fit)
  pred.grid2$pred.se <- as.vector(preds$se)
  predsout <- subset(pred.grid2,select = c("square","pred","pred.se"))
  predsout$julian <- i
  #predsout$year <- 2012
  ppi1 <- rbind(ppi,predsout)
  ppi <- ppi1
}

# OUTPUT THE FINAL PREDICTIONS
path <- "CCC.200.5_WR.csv";
path <- gsub("CCC",colony,path)
write.table(ppi,path,sep=",",row.names=F,col.names=T)


################################################################################
# END
################################################################################










###############################################################################
###############################################################################
###############################################################################
# Get IDs of raster in the UK that we want to apply weighting for
###############################################################################
###############################################################################
###############################################################################
#xx1<--2200000; xx2<-700000	### new massive extent
#yy1<-1500000; yy2<-6400000
#
#proj4stringA <- CRS("+proj=utm +zone=31N +ellps=GRS80 +units=m +no_defs")
#r <- raster(nrow=40, ncol=80, xmn=xx1, xmx=xx2, ymn=yy1, ymx = yy2, crs=proj4stringA) ### create a raster at the extent you want
#r[] <- rep(0,ncell(r)) ### add ficticious data
#res(r) <- 20000	# set a suitable reolution - 10 or 20 km?
#values(r) <- 1:ncell(r)
#pol <- rasterToPolygons(r)	### create a polygon from the raster
#
#setwd("A:/")
#world <- readOGR(getwd(),layer="country")
#crop_lim = extent(c(-12,6), c(45,65)) 
#world.crop <- crop(world, crop_lim)
#world.crop <- spTransform(world.crop,proj4stringA)
#pol.crop <- crop(pol,extent(world.crop))
#IDs <- extract(r, extent(world.crop)) # get raster IDs for the world.crop extent
#
###########################################################
# Get numbers of "active" tags vs those "with data"
###########################################################
#bb <- with(data,aggregate(dum,by=list(bird,year,julian),FUN='sum'))
#names(bb)[c(1,2,3,4)] <- c("bird","year","julian","n")
#bb$dum <- 1
#b <- with(bb,aggregate(dum,by=list(year,julian),FUN='sum'))
#
## Start and end periods incuded for each bird - for tag active time
#if(colony == "Skokholm"){yrmin <- 2014; yrmax <- 2015}
#if(colony == "Walney"){yrmin <- 2014; yrmax <- 2015}
#
#fin2 <- NULL
#for(i in unique(bb$bird)){
#  print(i)
#  if(i %in% c(334,336,391,395,407,388)){  # Orford Ness options
#    yrmin <- 2010; yrmax <- 2011
#  }
#  if(i %in% c(459,460,478,479,480,482,483,484,485,486,492,493)){  # Orford Ness options
#    yrmin <- 2011; yrmax <- 2012
#  }
#  bbb <- bb[bb$bird == i,]
#  mi <- min(bbb[bbb$year == yrmin,]$julian)
#  ma <- max(bbb[bbb$year == yrmax,]$julian)   
#  
#  # construct dates when tracking should have been available
#  fyr <- data.frame(julian = mi:365, year = yrmin)
#  lyr <- data.frame(julian = 1:ma, year = yrmax)
#  fin <- rbind(fyr,lyr)
#  fin$bird = i
#  fin2 <- rbind(fin2,fin)
#}
#
## merge with actual data at bird level
#dd <- merge(bb,fin2,by=c("bird","year","julian"),all=TRUE)
#dd$dum <- 1
#dd$n <- ifelse(is.na(dd$n),0,1)
#
## now sum new n column (same as above) AND the full dum column
#d <- with(dd,aggregate(dum,by=list(year,julian),FUN='sum'))
#names(d)[c(1,2,3)] <- c("year","julian","n.track")
#b <- with(dd,aggregate(n,by=list(year,julian),FUN='sum'))
#names(b)[c(1,2,3)] <- c("year","julian","n.active")
#f <- merge(d,b,by=c("year","julian"),all=TRUE)
#f$prop <- f$n.active / f$n.track
#f$prop <- 1/f$prop # take inverse for weighting - want to give more weight to times when fewer data were there - i.e. to account for missing partially
#
##merge into full dataset (takes a few mins)
#data2 <- merge(data,f,by=c("year","julian"))
#
## using the identified UK (ish) sqs where weighting needs applying:
#data2$prop <- ifelse(data2$square %in% IDs,data2$prop,1)
#
## suggest exporting this
##data2a <- subset(data2,select=c(-dum,-n.track,-n.active))
#setwd("A:/")
#write.table(data2,"Orford Ness.WR.csv",sep=",",col.names=TRUE,row.names=FALSE)
