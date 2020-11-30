func_plot=function(raster_data,mask_data,name,unit){
  file_name=paste0('China annual mean ',name,'.jpg')
  jpeg(file_name,width=1100,height=1000)
  raster::plot(raster_data,
               main=paste0('China annual mean ',name,' (',unit,')'),
               xlab='Latitude',
               ylab='Longitude',
               cex.lab=1.5,
               cex.main=2,
               cex.axis=1.5,)
  raster::plot(map,add=T)
  dev.off()
}