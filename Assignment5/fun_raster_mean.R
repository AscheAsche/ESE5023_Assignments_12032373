# Input: director of several tiff files
# Output: mean of those tiff files and masked by China region
func_raster_mean=function(dirs){
  mask=c(72,136,3,54)
  wd=getwd()
  files=dir(dirs)
  setwd(paste0(wd,'/',dirs))
  for(i in 1:12){
    eval(parse(text=paste0('r',i,'=raster::raster(files[i])')))
    eval(parse(text=paste0('r',i,'=crop(r',i,',mask)')))
  }
  result=sum(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12)/12
  setwd(wd)
  return(result)
}