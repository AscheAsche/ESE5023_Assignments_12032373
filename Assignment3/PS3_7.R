## 7.1
data1=read.csv('PS3_7_data1.csv')
IX1=which(data1[,2]==-99999)
IX2=which(data1[,3]==-99999)
data1=as_tibble(data1[-c(IX1,IX2),])
# sample for 2000 times and store in two group
sample_mean=matrix(NaN,1000,2)
for(j in 1:2){
  for(i in 1:1000){
    IX=sample(nrow(data1),1000,F)
    sample_mean[i,j]=sum(data1[IX,2])/1000
  }
}
# t-test
t.test(sample_mean[,1],sample_mean[,2])

## 7.2
# combine the concentration into one column
data2=matrix(NaN,2*nrow(data1),1)
data2[1:31050]=data1$CN_1360A[1:31050]
data2[31051:62100]=data1$CN_1365A[1:31050]
data2=as_tibble(data2)
data2=mutate(data2,'')
data2[1:31050,2]='CN_1360A'
data2[31051:62100,2]='CN_1365A'
colnames(data2)=c('concentration','station')
# ANOVA 
summary(aov(concentration~station,data=data2))

## 7.3 
data3=as_tibble(read.csv('PS3_7_data2.csv'))
IX=which(data3$Cal>50 | data3$Uncal>50)
data3=data3[-IX,]
model=lm(Cal~Uncal,data=data3)
summary(model)
ggplot(data=data3,aes(y=Cal,x=Uncal))+
  geom_point()+
  geom_smooth(method='lm',alpha=0)+
  labs(title='Concentration of PM10 of two sensors')+
  labs(subtitle='unit in μg/m3')+
  ylab('Calibrated sensor')+
  xlab('Un-alibrated sensor')
