rawdata=read.csv('2281305.csv',)
data=data.frame('',NaN,NaN,'',NaN,NaN)
colnames(data)=c('year_month','angle','angle_q','type','speed','speed_q')
data=as_tibble(data)
for(i in 1:nrow(rawdata)){
  data[i,1]=paste0(substr(rawdata$DATE[i],1,4),substr(rawdata$DATE[i],6,7))
  str=rawdata$WND[i]
  data[i,2]=as.numeric(substr(str,1,3))
  data[i,3]=as.numeric(substr(str,5,5))
  data[i,4]=substr(str,7,7)
  data[i,5]=as.numeric(substr(str,9,12))
  data[i,6]=as.numeric(substr(str,14,14))
}
# load the 'data.RData' will save a lot of time
# function 'substr' runs quite slow but it has to run
# 'data.RData' is the result of previous steps

month_mean=data %>%
  select(year_month,speed,speed_q,angle_q) %>%
  filter(speed_q==1 & angle_q==1) %>%
  group_by(year_month) %>%
  summarize(mean_speed=mean(speed))

month_mean=month_mean %>%
  mutate(date=NaN)
for(i in 1:nrow(month_mean)){
  month_mean$date[i]=as.numeric(substr(month_mean$year_month[i],1,4))+as.numeric(substr(month_mean$year_month[i],5,6))/12
}
plot(month_mean$date,month_mean$mean_speed,type='l')
