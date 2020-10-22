
Sig_Eqs=as_tibble(read.delim('signif.txt'))

# 1.2
Sig_Eqs %>%
  select(TOTAL_DEATHS,COUNTRY) %>%
  filter(!is.na(TOTAL_DEATHS)) %>%
  group_by(COUNTRY) %>%
  summarise(sum(TOTAL_DEATHS)) %>%
  arrange(desc(`sum(TOTAL_DEATHS)`))

# 1.3
times=Sig_Eqs %>%
  mutate(COUNT=1) %>%
  select(YEAR,COUNT,EQ_PRIMARY) %>%
  filter(EQ_PRIMARY>6) %>%
  group_by(YEAR) %>%
  summarise(TIMES=sum(COUNT))

plot(x=times$YEAR,y=times$TIMES,type='l')

# 1.4
CountEq_LargestEq=function(country){
  Sig_Eqs=as_tibble(read.delim('signif.txt'))
  
  time=Sig_Eqs %>%
    mutate(COUNT=1) %>%
    select(COUNTRY,COUNT) %>%
    filter(COUNTRY==country) %>%
    summarise(sum(COUNT))
  
  group=Sig_Eqs %>%
    select(EQ_PRIMARY,YEAR,MONTH,DAY,COUNTRY) %>%
    filter(COUNTRY==country) %>%
    arrange(desc(EQ_PRIMARY))
  
  return(c(time$`sum(COUNT)`[1],group$YEAR[1],group$MONTH[1],group$DAY[1]))
}

countries=unique(Sig_Eqs$COUNTRY)
result=data.frame('',NaN,NaN,NaN,NaN)
for(i in 1:length(countries)){
  result[i,1]=countries[i]
  result[i,2:5]=CountEq_LargestEq(countries[i])
}
colnames(result)=c('COUNTRY','TOTAL_EARTHQUAKE','YEAR','MONTH','DAY')
write.csv(result,file='result.csv')
