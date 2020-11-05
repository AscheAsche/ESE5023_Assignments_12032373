data=as_tibble(read.csv('PS3_5_data.txt',header=F,sep=','))
data=data[-1,-1]
# change char to numeric
data=mutate(data,as.numeric(data$V3[1:24]))
data=mutate(data,as.numeric(data$V4[1:24]))
data=data[,c(-2,-3)]
colnames(data)=c('Nebula','Velocity','Distance')

# 5.1 and 5.2
ggplot(data,aes(x=Velocity,y=Distance))+
  geom_point(size=2)+
  labs(title='Relationship between distance and recession velocity of Nebula')+
  xlab('Recession velocity (km/s)')+
  ylab('Distance to Earth (megaparsecs)')+
  geom_smooth(method='lm',alpha=0)

# 5.3
model=lm(Distance~Velocity,data)
summary(model)
