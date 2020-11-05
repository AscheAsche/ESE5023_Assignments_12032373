data=cpus
IX=sample(209,as.integer(209*0.8),F)
data0=data[IX,] # train
data1=data
data1=data1[-IX,] # test
model=lm(perf~syct+mmin+mmax+cach+chmin+chmax,data=data0)
summary(model)
coef(model)

predict_result=predict(model,data1)
cor.test(data1$perf,predict_result)
plot(data1$perf,predict_result)
(mean(predict_result) - mean(data1$perf))/mean(data1$perf)*100
