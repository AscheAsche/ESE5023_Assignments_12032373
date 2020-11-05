rawdata=read.csv('PS3_2_data.txt',header=F,sep=',')
rawdata[is.na(rawdata)]=0

# Re-arrange the dataset into 2 columns: 
# name of bone, oxygen isotopic composition value
data=as_tibble(data.frame('',NaN))
k=1
for(i in 1:12){
  for(j in 2:7){
    data[k,1]=rawdata[i,1]
    data[k,2]=rawdata[i,j]
    k=k+1
  }
}
IX=which(data[,2]==0)
data=data[-IX,]
colnames(data)=c('bone','value')

# ANOVA
ggplot(data=data,aes(x=bone,y=value,fill=bone))+
  geom_boxplot()+
  labs(x='Bone name',
       y='Oxygen isotopic composition',
       title='Oxygen isotopic composition	of T-rex bones')+
  scale_fill_discrete(name='Legend')
summary(aov(value~bone,data=data))
