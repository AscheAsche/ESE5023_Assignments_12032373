encoding='UTF-8'
setwd('D:/南科大/硕士/环境科学的中编程/Assignment1')
csvdata=read.csv('2281305.csv')

data=array('',c(nrow(csvdata),9))

# 1. import data
for(i in 1:nrow(csvdata)){
  # 1.1 deal with time
  str_time=as.POSIXct(strptime(csvdata$DATE[i],'%Y-%m-%dT%H:%M:%S'))
  data[i,1]=unclass(str_time) # absolute POSIXlct time
  data[i,2]=substr(csvdata$DATE[i],1,4) # year in string
  data[i,3]=substr(csvdata$DATE[i],6,7) # month
  data[i,4]=substr(csvdata$DATE[i],9,10) # day
  # 1.2 deal with data in string
  str_vis=csvdata[i,42]
  data[i,5]=substr(str_vis,1,6)
  data[i,6]=substr(str_vis,8,8)
  data[i,7]=substr(str_vis,10,10)
  data[i,8]=substr(str_vis,12,12)
  # 1.3 store "yyyymmdd" in a column
  data[i,9]=paste0(data[i,2],data[i,3],data[i,4])
}
# data (array in string) has 9 columns: col1~4: POSIXlct time, year, month, day
# col5~8: original dataset
# col9: string in format "yyyymmdd"

# 2. quality test
IX=NaN
for(i in 1:nrow(data)){
  if(data[i,5]=='999999'|data[i,6]!='1'|data[i,7]!='N'|data[i,8]!='1'){
    IX=c(IX,i)
  }
}
IX<-IX[-1]
data<-data[-IX,]

# 3. PLOT
time=as.POSIXct.numeric(data[,1])
plot(time,data[,5],type='l')
print('Trend: Visiblity has a obivous seasonality.')

# 4. annual data screening
year=unique(data[,2])
days=unique(data[,9])
daily_data=array(0,c(length(days),2))
daily_data[,1]=as.numeric(days)
j=1
for(i in 1:length(days)){
  summ=0
  counter=0
  date=days[i]
  maxnum=0
  while(data[j,9]==date){
    maxnum=max(maxnum,as.numeric(data[j,5]))
    j=j+1
    if(j==nrow(data)+1){break}
  }
  daily_data[i,2]=maxnum
}

# 5. grouping
disp_result=array(0,c(7,4))
colnames(disp_result)=c('2010','2011','2012','2013')
rownames(disp_result)=c('[0,5km)','[5km,10km)','[10km,15km)','[15km,20km)','[20km,25km)','[25km,30km)','>=30km')


for(i in 1:nrow(daily_data)){
  if(substr(as.character(daily_data[i,1]),1,4)==year[1]){j=1}
  else if(substr(as.character(daily_data[i,1]),1,4)==year[2]){j=2}
  else if(substr(as.character(daily_data[i,1]),1,4)==year[3]){j=3}
  else{j=4}
  
  if(daily_data[i,2]<5000){
    disp_result[1,j]=disp_result[1,j]+1
  }else if(daily_data[i,2]>=5000 & daily_data[i,2]<10000){
    disp_result[2,j]=disp_result[2,j]+1
  }else if(daily_data[i,2]>=10000 & daily_data[i,2]<15000){
    disp_result[3,j]=disp_result[3,j]+1
  }else if(daily_data[i,2]>=15000 & daily_data[i,2]<20000){
    disp_result[4,j]=disp_result[4,j]+1
  }else if(daily_data[i,2]>=20000 & daily_data[i,2]<25000){
    disp_result[5,j]=disp_result[5,j]+1
  }else if(daily_data[i,2]>=25000 & daily_data[i,2]<30000){
    disp_result[6,j]=disp_result[3,j]+1
  }else{
    disp_result[7,j]=disp_result[7,j]+1
  }
}


print('Anaual grouping result are showed below:')
print(disp_result)
