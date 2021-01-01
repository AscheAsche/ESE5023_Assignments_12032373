# Packages
library(fields)
library(RNetCDF)

# 1. SST interpolation
# Read data
sst_interp=read.csv('../sst_interpolation/sst_interpolated.csv',header=T)
sst_interp=as.matrix(sst_interp)
sst_recon=read.csv('../sst_interpolation/sst_reconstruct.csv',header=T)
sst_recon=as.matrix(sst_recon)
nc=RNetCDF::open.nc('../sst.nc')
lat=RNetCDF::var.get.nc(nc,'latitude')
lon=RNetCDF::var.get.nc(nc,'longitude')
sst=RNetCDF::var.get.nc(nc,'sstMasked')
RNetCDF::close.nc(nc)
sst[1,1]=29.45623
sst_interp[1,1]=29.45623
sst_recon[1,1]=29.45623

# Plot
jpeg('SST.jpg',width=1000,height=1000,res=72)
fields::image.plot(lon,sort(lat),sst,
                   horizontal=T,legend.cex=2,
                   axis.args=list(cex.axis=2), 
                   legend.width=1.25, legend.mar=3,
                   legend.args=list(text="                                           unit: degC",
                                    cex=2.5),           
                   xlab='',ylab='',midpoint=T, axes=F, ann=F)
title(xlab="Lattitude",cex.lab=2.5,font.lab=2)
axis(1,at=pretty(lon),tck=-0.01,lwd=4,cex.axis=2,font=1,cex.lab=1.75)
title(ylab="Longitude",cex.lab=2.5,font.lab=2)
axis(2,at=pretty(lat),tck=-0.01,lwd=4,cex.axis=2,font=1,cex.lab=1.75)
title(main=paste("SST orginal data"),
      cex.main=3,font.main=2)
dev.off()

jpeg('SST_interp.jpg',width=1000,height=1000,res=72)
fields::image.plot(lon,sort(lat),sst_interp,
                   horizontal=T,legend.cex=2,
                   axis.args=list(cex.axis=2), 
                   legend.width=1.25, legend.mar=3,
                   legend.args=list(text="                                           unit: degC",
                                    cex=2.5),           
                   xlab='',ylab='',midpoint=T, axes=F, ann=F)
title(xlab="Lattitude",cex.lab=2.5,font.lab=2)
axis(1,at=pretty(lon),tck=-0.01,lwd=4,cex.axis=2,font=1,cex.lab=1.75)
title(ylab="Longitude",cex.lab=2.5,font.lab=2)
axis(2,at=pretty(lat),tck=-0.01,lwd=4,cex.axis=2,font=1,cex.lab=1.75)
title(main=paste("Interpolated SST"),
      cex.main=3,font.main=2)
dev.off()

jpeg('SST_recon.jpg',width=1000,height=1000,res=72)
fields::image.plot(lon,sort(lat),sst_recon,
                   horizontal=T,legend.cex=2,
                   axis.args=list(cex.axis=2), 
                   legend.width=1.25, legend.mar=3,
                   legend.args=list(text="                                           unit: degC",
                                    cex=2.5),           
                   xlab='',ylab='',midpoint=T, axes=F, ann=F)
title(xlab="Lattitude",cex.lab=2.5,font.lab=2)
axis(1,at=pretty(lon),tck=-0.01,lwd=4,cex.axis=2,font=1,cex.lab=1.75)
title(ylab="Longitude",cex.lab=2.5,font.lab=2)
axis(2,at=pretty(lat),tck=-0.01,lwd=4,cex.axis=2,font=1,cex.lab=1.75)
title(main=paste("Reconstructed SST"),
      cex.main=3,font.main=2)
dev.off()

# 2. Picture interpolation
# Read data
pic=read.csv('../picture_interpolation/pic_20x20miss_square.csv',header=F)
pic=as.matrix(pic)
pic[pic==-99999]=NaN
pic=t(apply(pic, 2, rev))
pic_interp=read.csv('../picture_interpolation/X_interp.csv',header=T)
pic_interp=as.matrix(pic_interp)
pic_interp=t(apply(pic_interp, 2, rev))
pic_interp[which(pic_interp<0)]=0
pic_interp[which(pic_interp>255)]=255
pic_original=read.csv('../picture_interpolation/pic_original.csv',header=F)
pic_original=as.matrix(pic_original)
pic_original=t(apply(pic_original, 2, rev))

# Plot
jpeg('pic_original.jpg',width=1000,height=1000,res=72)
image(pic_original, useRaster=TRUE, axes=FALSE)
dev.off()

jpeg('pic.jpg',width=1000,height=1000,res=72)
image(pic, useRaster=TRUE, axes=FALSE)
dev.off()

jpeg('pic_interp.jpg',width=1000,height=1000,res=72)
image(pic_interp, useRaster=TRUE, axes=FALSE)
dev.off()
