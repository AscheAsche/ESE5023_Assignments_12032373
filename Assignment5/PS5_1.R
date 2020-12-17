map=readOGR('China_map',"bou2_4p")
# function defined
source('fun_raster_mean.R')
source('func_plot.R')
# Solar Radiation
solar=func_raster_mean('wc2.1_2.5m_srad')
func_plot(solar,map,'solar radiation','kJ/m2 per day')
# Wind
wind=func_raster_mean('wc2.1_2.5m_wind')
func_plot(wind,map,'wind speed','m/s')
# Precipitation
prec=func_raster_mean('wc2.1_2.5m_prec')
func_plot(prec,map,'precipitation','mm')
