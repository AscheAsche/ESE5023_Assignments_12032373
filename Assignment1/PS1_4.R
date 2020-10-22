x=sample(1:100,1)

Least_moves=function(x){

  y=1
  
  while(y<=x){
    processes=permutations(2,y,c(1,2),TRUE,TRUE)
    for(i in 1:nrow(processes)){
      result=1
      process=processes[i,]
      for(j in 1:length(process)){
        if(process[j]==1){result=result+1}
        else{result=result*2}
        }
      if(result==x){
        final_result=c(y,process)
        return(final_result)}
    }
    y=y+1
  }
}

result=Least_moves(x)

process=array('',c(1,length(result)-1))

for(i in 2:length(result)){
  if(result[i]==1){process[i-1]='+1'}
  else{process[i-1]='double'}
}

print(paste0('Need ',result[1],' times of move to reach ',x,'. Processes are:'))
print(process)

