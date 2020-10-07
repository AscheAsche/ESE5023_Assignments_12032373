# open with UTF-8
setwd('D:/南科大/硕士/环境科学的中编程/Assignment1')
csvdata=read.csv('2281305.csv')

data=array('',c(nrow(csvdata),5))

# 1. import data
for(i in 1:nrow(csvdata)){
  # 1.1 deal with time
  str_time=as.POSIXct(strptime(csvdata$DATE[i],'%Y-%m-%dT%H:%M:%S'))
  data[i,1]=unclass(str_time)
  # 1.2 deal with data in string
  str_vis=csvdata[i,42]
  data[i,2]=substr(str_vis,1,6)
  data[i,3]=substr(str_vis,8,8)
  data[i,4]=substr(str_vis,10,10)
  data[i,5]=substr(str_vis,12,12)
}
# 2. quality test
IX=NaN
for(i in 1:nrow(data)){
  if(data[i,2]=='999999'|data[i,3]!='1'|data[i,4]!='N'|data[i,5]!='1'){
    IX=c(IX,i)
  }
}
IX<-IX[-1]
data<-data[-IX,]

# 3. PLOT
time=as.POSIXct.numeric(data[,1])
plot(time,data[,2],type='l')
print('Trend: Visiblity has a obivous seasonality.')
# 4. annual data screening
  # 4.1 get indexes by year
IX1=which(data[,1]>=unclass(as.POSIXct(strptime('2010-01-01','%Y-%m-%d')))
          &data[,1]<unclass(as.POSIXct(strptime('2011-01-01','%Y-%m-%d'))))
IX2=which(data[,1]>=unclass(as.POSIXct(strptime('2011-01-01','%Y-%m-%d')))
          &data[,1]<unclass(as.POSIXct(strptime('2012-01-01','%Y-%m-%d'))))
IX3=which(data[,1]>=unclass(as.POSIXct(strptime('2012-01-01','%Y-%m-%d')))
          &data[,1]<unclass(as.POSIXct(strptime('2013-01-01','%Y-%m-%d'))))
IX4=which(data[,1]>=unclass(as.POSIXct(strptime('2013-01-01','%Y-%m-%d')))
          &data[,1]<unclass(as.POSIXct(strptime('2014-01-01','%Y-%m-%d'))))
  # 4.2 screening data by year
data1=data[IX1,]
data2=data[IX2,]
data3=data[IX3,]
data4=data[IX4,]
  # 4.3 screening by visibility
result=array(0,c(7,4))
# year1
for(i in 1:nrow(data1)){
  if(as.numeric(data1[i,2])>=0 & as.numeric(data1[i,2])<5000){
    result[1,1]<-result[1,1]+1
  }else if(as.numeric(data1[i,2])>=5000 & as.numeric(data1[i,2])<10000){
    result[2,1]<-result[2,1]+1
  }else if(as.numeric(data1[i,2])>=10000 & as.numeric(data1[i,2])<15000){
    result[3,1]<-result[3,1]+1
  }else if(as.numeric(data1[i,2])>=15000 & as.numeric(data1[i,2])<20000){
    result[4,1]<-result[4,1]+1
  }else if(as.numeric(data1[i,2])>=20000 & as.numeric(data1[i,2])<25000){
    result[5,1]<-result[5,1]+1
  }else if(as.numeric(data1[i,2])>=25000 & as.numeric(data1[i,2])<30000){
    result[6,1]<-result[6,1]+1
  }else{
    result[7,1]<-result[7,1]+1
  }
}

# year2
for(i in 1:nrow(data2)){
  if(as.numeric(data2[i,2])>=0 & as.numeric(data2[i,2])<5000){
    result[1,2]<-result[1,2]+1
  }else if(as.numeric(data2[i,2])>=5000 & as.numeric(data2[i,2])<10000){
    result[2,2]<-result[2,2]+1
  }else if(as.numeric(data2[i,2])>=10000 & as.numeric(data2[i,2])<15000){
    result[3,2]<-result[3,2]+1
  }else if(as.numeric(data2[i,2])>=15000 & as.numeric(data2[i,2])<20000){
    result[4,2]<-result[4,2]+1
  }else if(as.numeric(data2[i,2])>=20000 & as.numeric(data2[i,2])<25000){
    result[5,1]<-result[5,2]+1
  }else if(as.numeric(data2[i,2])>=25000 & as.numeric(data2[i,2])<30000){
    result[6,2]<-result[6,2]+1
  }else{
    result[7,2]<-result[7,2]+1
  }
}

# year3
for(i in 1:nrow(data3)){
  if(as.numeric(data3[i,2])>=0 & as.numeric(data3[i,2])<5000){
    result[1,3]<-result[1,3]+1
  }else if(as.numeric(data3[i,2])>=5000 & as.numeric(data3[i,2])<10000){
    result[2,3]<-result[2,3]+1
  }else if(as.numeric(data3[i,2])>=10000 & as.numeric(data3[i,2])<15000){
    result[3,3]<-result[3,3]+1
  }else if(as.numeric(data3[i,2])>=15000 & as.numeric(data3[i,2])<20000){
    result[4,3]<-result[4,3]+1
  }else if(as.numeric(data3[i,2])>=20000 & as.numeric(data3[i,2])<25000){
    result[5,3]<-result[5,3]+1
  }else if(as.numeric(data3[i,2])>=25000 & as.numeric(data3[i,2])<30000){
    result[6,3]<-result[6,3]+1
  }else{
    result[7,3]<-result[7,3]+1
  }
}

# year4
for(i in 1:nrow(data4)){
  if(as.numeric(data4[i,2])>=0 & as.numeric(data4[i,2])<5000){
    result[1,4]<-result[1,4]+1
  }else if(as.numeric(data4[i,2])>=5000 & as.numeric(data4[i,2])<10000){
    result[2,4]<-result[2,4]+1
  }else if(as.numeric(data4[i,2])>=10000 & as.numeric(data4[i,2])<15000){
    result[3,4]<-result[3,4]+1
  }else if(as.numeric(data4[i,2])>=15000 & as.numeric(data4[i,2])<20000){
    result[4,4]<-result[4,4]+1
  }else if(as.numeric(data4[i,2])>=20000 & as.numeric(data4[i,2])<25000){
    result[5,4]<-result[5,4]+1
  }else if(as.numeric(data4[i,2])>=25000 & as.numeric(data4[i,2])<30000){
    result[6,4]<-result[6,4]+1
  }else{
    result[7,4]<-result[7,4]+1
  }
}

result_ratio=array(0,c(7,4))
result_ratio[,1]=result[,1]/nrow(data1)
result_ratio[,2]=result[,2]/nrow(data1)
result_ratio[,3]=result[,3]/nrow(data1)
result_ratio[,4]=result[,4]/nrow(data1)

disp_result=array('',c(7,8))
colnames(disp_result)=c('2010','2010(ratio)','2011','2011(datio)','2012','2012(datio)','2013','2013(ratio)')
rownames(disp_result)=c('[0,5km)','[5km,10km)','[10km,15km)','[15km,20km)','[20km,25km)','[25km,30km)','>=30km')

disp_result[,c(1,3,5,7)]=result[]
disp_result[,c(2,4,6,8)]=round(result_ratio[],2)

print('Anaual grouping result are showed below:')
print(disp_result)
print('Visibility has a slight reduction in 2013.')
print('But the others years have similar result.')
