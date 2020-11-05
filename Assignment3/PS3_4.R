data=read.csv('PS3_4_data.txt',header=F,sep=',')
data=data[,-1]
data=t(data)
colnames(data)=c('elevation','temperature')
data=as_tibble(data)

# Regression
model=lm(temperature~elevation,data)
summary(model)
# Plot
ggplot(data,aes(x=elevation,y=temperature))+
  geom_point()+
  labs(title='Linear regression of temperature and elevation result')+
  xlab('Elevation (m)')+
  ylab('Temperature (degrees C)')+
  geom_smooth(method='lm',alpha=0)+
  geom_abline(intercept=13.3+180*0.0098,slope=-0.0098,lwd=1,color='red')+
  geom_label(x=300,y=10,label='line of lapse rate
  -9.8 centigrade/km',color='red')+
  geom_label(x=500,y=12.5,label='line of regression result',color='blue')
