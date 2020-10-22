Pascal_triangle=function(k){
  
  k=as.integer(k)
  
  if(k==1){
    result=1
  }else if(k==2){
      result=c(1,1)
  }else if(k>2){
    line=c(1,1) # initial preparation for the first loop
    counter=3 # current line order
    while(counter<=k){
      temp_line=vector('numeric',length(line)-1)
      for(i in 1:length(temp_line)){
        temp_line[i]=line[i]+line[i+1]
      }
      line=c(1,temp_line,1) # prepare for the next loop
      counter=counter+1
    }
    result=line
  }
  print(result)
}

print('100th line:')
Pascal_triangle(100)
print('200th line:')
Pascal_triangle(200)
