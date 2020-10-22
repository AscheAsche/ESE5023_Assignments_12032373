
rawdata=read.csv('AQ_FSPMC-20180101-20200311.csv',header=F,skip=9,sep=',')

data=data.frame(NaN,NaN,NaN,NaN,NaN)
colnames(data)=c('year','month','day','hour','conc')
data=as_tibble(data)
for(i in 1:nrow(rawdata)){
  str=rawdata$V1[i]
  data[i,1]=as.numeric(substr(str,1,4))
  data[i,2]=as.numeric(substr(str,6,7))
  data[i,3]=as.numeric(substr(str,9,10))
  data[i,4]=as.numeric(substr(str,12,13))
  data[i,5]=as.numeric(rawdata$V2[i])
}
data=as_tibble(data)

month_mean=data %>%
  filter(conc!=-99999) %>%
  group_by(year,month) %>%
  summarise(mean_conc=mean(conc))

month_mean=as_tibble(month_mean)
# monthly concentration figure
month_mean %>%
  ggplot(aes(x=month,y=mean_conc,color=year)) + 
  geom_line() +
  facet_wrap(~ year)

