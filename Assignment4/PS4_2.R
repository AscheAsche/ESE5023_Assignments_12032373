rawdata=read.csv('2281305.csv')

# 2.1
data=rawdata %>%
  select(DATE,TMP) %>%
  mutate(ym=substr(DATE,1,7)) %>%
  filter(substr(TMP,7,7)=='1') %>%
  group_by(ym) %>%
  summarise(mean_ws=mean(as.numeric(substr(TMP,1,5))*0.1))
September=data[129,] # Store data on Sep.for question 2.4
data=data[-129,] # Make data ranges to 2020-08
data_ts=ts(data=data$mean_ws,
           start=c(2010,1),
           frequency=12)

# 2.2
deco_result=decompose(data_ts)
plot(deco_result,xlab='Time')

hist(deco_result$random,breaks=10,prob=TRUE,
     xlab='Random part of decomposition',
     ylab='Density',
     main='White noise distribution of random part')
curve(dnorm(x,mean=mean(deco_result$random,na.rm=T),
            sd=sd(deco_result$random,na.rm=T)),
      add=TRUE,col="red")

# 2.3
model=auto.arima(data_ts)
print(model)

# 2.4
fore_result=forecast(model,2)
print(fore_result)
print(September$mean_ws)
ratio=(fore_result$mean[1]-September$mean_ws)/September$mean_ws
print(ratio)
