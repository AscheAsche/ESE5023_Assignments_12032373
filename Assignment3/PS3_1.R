# 1.1
data=as_tibble(matrix(NaN,52,1))
colnames(data)=c('pre','seed_or_not')
data[]=c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 163.0, 147.8, 95.0, 87.0, 81.2, 68.5, 47.3, 41.1, 36.6, 29.0, 28.6, 26.3, 26.0, 24.4, 21.4, 17.3, 11.5, 4.9, 4.9, 1.0,2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 430.0, 334.1, 302.8, 274.7, 274.7, 255.0, 242.5, 200.7, 198.6, 129.6, 119.0, 118.3, 115.3, 92.4, 40.6, 32.7, 31.4, 17.5, 7.7, 4.1)
data[1:26,2]='unseed'
data[27:52,2]='seed'

ggplot(data=data,aes(x=seed_or_not,y=pre,fill=seed_or_not))+
  geom_boxplot()+
  ylab('Precipitation (acre-feet)')+
  xlab('Seeding or not')+
  labs(title='Precipitation distribution')+
  scale_fill_discrete(name='Legend')

# 1.2
test=ks.test(data$pre[1:26],data$pre[27:52])
print(test)

print(mean(data$pre[27:52])-mean(data$pre[1:26]))
