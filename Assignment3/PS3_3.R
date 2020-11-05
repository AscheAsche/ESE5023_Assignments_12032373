data=as_tibble(data.frame('','',NaN))
colnames(data)=c('pregnancy','diet','zinc')
data[1:6,1]='pregnant'
data[1:6,2]='nonvegetarian'
data[1:6,3]=c(185,189,187,181,150,176)
data[7:18,1]='pregnant'
data[7:18,2]='vegetarian'
data[7:18,3]=c(171,174,202,171,207,125,189,179,163,174,184,186)

summary(aov(zinc~diet,data=data))
t.test(x=data$zinc[1:5],y=data$zinc[6:18])
