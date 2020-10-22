Matrix_multip=function(){
  
  M1=matrix(NaN,5,10)
  M2=matrix(NaN,10,5)
  for(i in 1:length(M1)){
    M1[i]=sample(0:50,1)
  }
  for(i in 1:length(M2)){
    M2[i]=sample(0:50,1)
  }
  
  M3=matrix(NaN,nrow(M1),ncol(M2))
  for(i in 1:nrow(M3)){
    for(j in 1:ncol(M3)){
      temp_vector=M1[i,]*M2[,j]
      summ=0
      for(k in 1:length(temp_vector)){
        summ=summ+temp_vector[k]
      }
      M3[i,j]=summ
    }
  }
  print(M3)
  
  M4=M1%*%M2
  print('Verification result shows:')
  all.equal(M3,M4)
}

