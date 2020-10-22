encoding='UTF-8'
# data from national AQ station CN_1329A in Wuhan
# 1. read raw data as string
setwd('D:/南科�?/硕士/环境科学的中编程/Assignment1')
strdata_pm=read.csv('AQ_FSPMC-20180101-20200311.csv',header=F,skip=9,sep=',')
strdata_o3=read.csv('AQ_O3-20180101-20200311.csv',header=F,skip=9,sep=',')

# 2. find and delete invalid data
IX_pm=which(strdata_pm[,2]=='-99999'|strdata_pm[,2]>=200)
IX_o3=which(strdata_o3[,2]=='-99999'|strdata_o3[,2]>=200)
strdata_pm<-strdata_pm[-IX_pm,]
strdata_o3<-strdata_o3[-IX_o3,]

# 3. convert string to number, convert time to numeric time
data_pm=matrix(NaN,nrow(strdata_pm),ncol(strdata_pm))
data_o3=matrix(NaN,nrow(strdata_o3),ncol(strdata_o3))
data_pm[,1]=unclass(as.POSIXct(strptime(strdata_pm[,1],'%Y/%m/%d %H:%M:%S')))
data_pm[,2]=as.numeric(strdata_pm[,2])
data_o3[,1]=unclass(as.POSIXct(strptime(strdata_o3[,1],'%Y/%m/%d %H:%M:%S')))
data_o3[,2]=as.numeric(strdata_o3[,2])

# 4. PLOT DATA (entire time series) and basic analysis
time_pm=as.POSIXct.numeric(as.character(data_pm[,1]))
time_o3=as.POSIXct.numeric(as.character(data_o3[,1]))
plot(time_pm,data_pm[,2],type='l')
plot(time_o3,data_o3[,2],type='l')
mean_pm=round(mean(data_pm[,2]),2)
mean_o3=round(mean(data_o3[,2]),2)
print(paste0('The mean concentration of PM2.5 and ozone for data period are: '
      ,mean_pm,' ug/m3 and ',mean_o3,' ppb.'))
print(paste('Based on the figures we can reveal the PM2.5 and ozone have a',
'significant chemical coupling.'))

# 5. average concentration during knockdown period
# compared with the corresponding period in 2019
IX_pm2=which(data_pm[,1]>=unclass(as.POSIXct(strptime('2019/01/23','%Y/%m/%d')))
             &data_pm[,1]<unclass(as.POSIXct(strptime('2019/03/10','%Y/%m/%d'))))
IX_o32=which(data_o3[,1]>=unclass(as.POSIXct(strptime('2019/01/23','%Y/%m/%d')))
             &data_o3[,1]<unclass(as.POSIXct(strptime('2019/03/10','%Y/%m/%d'))))
IX_pm3=which(data_pm[,1]>=unclass(as.POSIXct(strptime('2020/01/23','%Y/%m/%d')))
             &data_pm[,1]<unclass(as.POSIXct(strptime('2020/03/10','%Y/%m/%d'))))
IX_o33=which(data_o3[,1]>=unclass(as.POSIXct(strptime('2020/01/23','%Y/%m/%d')))
             &data_o3[,1]<unclass(as.POSIXct(strptime('2020/03/10','%Y/%m/%d'))))

mean_pm_2019=round(mean(data_pm[IX_pm2,2]),2)
mean_o3_2019=round(mean(data_o3[IX_o32,2]),2)
mean_pm_2020=round(mean(data_pm[IX_pm3,2]),2)
mean_o3_2020=round(mean(data_o3[IX_o33,2]),2)
result_mean=array('',c(2,2))
rownames(result_mean)=c('PM2.5','O3')
colnames(result_mean)=c('2019','2020')
result_mean[1,]=c(mean_pm_2019,mean_pm_2020)
result_mean[2,]=c(mean_o3_2019,mean_o3_2020)

print('Mean concencration during lockdown:')
print(result_mean)
print('A reduction of PM2.5 was observer but for O3 is an increase.')

std_pm_2019=round(sd(data_pm[IX_pm2,2]),2)
std_o3_2019=round(sd(data_o3[IX_o32,2]),2)
std_pm_2020=round(sd(data_pm[IX_pm3,2]),2)
std_o3_2020=round(sd(data_o3[IX_o33,2]),2)
result_std=array(NaN,c(2,2))
rownames(result_std)=c('PM2.5','O3')
colnames(result_std)=c('2019','2020')
result_std[1,]=c(std_pm_2019,std_pm_2020)
result_std[2,]=c(std_o3_2019,std_o3_2020)

print('Concentration STD during lockdown:')
print(result_std)
print('Less data volatility was indicated.')
