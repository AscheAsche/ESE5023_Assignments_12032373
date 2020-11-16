# 1. Boxplot
#library(gapminder)
data1=as_tibble(gapminder)
data1 %>%
  select(continent,year,gdpPercap) %>%
  filter(year==2007) %>%
  mutate(gdp_log=log(gdpPercap)) %>%
  group_by(continent) %>%
  ggplot(aes(x=continent,y=gdp_log,fill=continent))+
  geom_boxplot(size=0.5)+
  labs(title='2007 Global log-GDP per capita in five continents',
       subtitle='Data from "gapminder" package',
       x='Continent',
       y='GDP per capita in log term')+
  theme_dark()+
  theme(plot.title=element_text(size=15, face="bold"),
        plot.subtitle=element_text(size=10, face="bold"),
        axis.text.x=element_text(size=12,face="bold"), 
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)) + 
  scale_fill_discrete(name="Continent Name")

# 2. Time series
data2=as_tibble(read.csv('Time_Series_Data.csv',header=T,skip=2))
data2_ts=ts(data=data2[1:1598,2:3],
            start=c(1990,3),
            frequency=365.25/7)
data2=data.frame(Date=c(time(data2_ts)),SST=c(data2_ts[,1]),SSTA=c(data2_ts[,2]))
data2 %>%
  ggplot(aes(x=Date,y=SSTA))+
  geom_line(size=0.25,color='blue')+
  geom_hline(aes(yintercept=0),color='red',size=1.5,alpha=0.5)+
  scale_fill_manual()+
  theme_bw()+
  labs(title='Sea Surface Temperature Abormal (SSTA) at station Nino3.4',
       subtitle='Data from NOAA website',
       x='Date (year)',
       y='SSTA (degree_C)')+
  theme(plot.title=element_text(size=15, face="bold"),
        plot.subtitle=element_text(size=10),
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=12,face="bold"),
        axis.title.y=element_text(size=12,face="bold"))
decom=decompose(data2_ts[,2])
plot(decom)
  
# 3. Histogram
data3=read.csv('Histogram_Data.csv',header=T)

ggplot(data=data3,aes(x=JUL))+
  geom_histogram(bins=50,color='red',size=0.5,fill='orange',alpha=0.3)+
  theme_gray()+
  theme(plot.title=element_text(size=15, face="bold"),
        plot.subtitle=element_text(size=10),
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=12,face="bold"),
        axis.title.y=element_text(size=12,face="bold"))+
  labs(x='Precipitation (inch)',
        y='Count',
        title='July Precipitation distribution in Boulder CO',
        subtitle='Data from Boulder Climate Center')

# 4. Scatter plot
data4_pm=as_tibble(read.csv('Scatter_Plot_PM25.csv',header=F,skip=9))
data4_rh=as_tibble(read.csv('Scatter_Plot_RH.csv',header=F,skip=9))
data4=cbind(data4_pm,data4_rh$V2)
ix1=which(data4_pm[,2]==-99999|data4_pm[,2]==0)
ix2=which(data4_rh[,2]==-99999|data4_rh[,2]>=100)
ix=unique(c(ix1,ix2))
data4=as_tibble(data4[-ix,])
colnames(data4)=c('Time','PM25','RH')
rm(data4_pm,data4_rh,ix1,ix2,ix)
model=lm(data4$PM25~data4$RH)
summary(model)
ggplot(data=data4,aes(x=RH,y=PM25))+
  geom_point(size=0.35,color=rainbow(8128))+
  geom_smooth(method='lm',alpha=0,color='black',size=1.25)+
  theme_minimal()+
  theme(plot.title=element_text(size=15, face="bold"),
        plot.subtitle=element_text(size=10),
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=12,face="bold"),
        axis.title.y=element_text(size=12,face="bold"))+
  labs(x='Relative Humidity (%)',
       y='PM2.5 (μg/m3)',
       title='Relative Humidity and PM2.5 Relationship in Wuhan 2018',
       subtitle='Data from national AQ station and meteorology station')+
  annotate(geom='text',x=20,y=60,label='Regression Line')+
  annotate(geom='text',x=25,y=200,label='R_square=0.004',size=3.5)

# 5. Image plot
#library(fields)
#library(RNetCDF)
# Result figure is "SST_Image_Plot.png"
# Run this section will be VERY SLOW
# because image.plot in R is NOT parallel computing
unzip('Image_Data.zip')
ex.nc=open.nc("Image_Data.nc")
lat=var.get.nc(ex.nc, "latitude")
lon=var.get.nc(ex.nc, "longitude")
SST=var.get.nc(ex.nc, "analysed_sst")
image.plot(lon,lat,SST,xlab='',ylab='')
title(xlab='Longitude',
  ylab='Latitude',
  main='Global Sea Surface Temperature on 2020-11-13 12:00',
  sub='Source: ERDDAP website')
  