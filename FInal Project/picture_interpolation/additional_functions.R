# This R scripts contains all needed defined functions
# which will be called by the main scripts.
# 
# Including: 
#   1. Function "func_mat_rotate"
#       Rotate the data matrix 90 degree clockwise.
#   2. Function "func_full"
#       Check if all the missing value were interpolated.
#   3. Function "func_indicator"
#       Return the validity of observed data with valid as 1, vise as 0.
#   3. Function "func_interp"
#       Interpolate the msising data by PCA-reconstruct.
#
#--------------------------------------------------------
#
#--------------------------------------------------------
# Function "func_mat_rotate"
# input: Any matrix
# output: The rotated matrix
func_mat_rotate=function(a){
  t(apply(a, 2, rev))
}

#--------------------------------------------------------
# Function "func_full"
# Input: Indicator matrix with consisted with 1 and 0
# output: True or False
func_full=function(I){
  summ=sum(I)
  full=nrow(I)*ncol(I)
  if(summ==full){return(T)}
  else{return(F)}
}

#--------------------------------------------------------
# Function "func_indicator"
# Input: Data matrix
# output: Indicator matrix with 1 and 0
func_indicator=function(a){
  if (a==missing_value){
    b=0
  }else{
    b=1
  }
  return(b)
}

#--------------------------------------------------------
# Function "func_interp"
# Input: Data matrix(a small part)
# output: Data after interpolation with PCA-reconstruct
func_interp=function(X){
  #
  I_vector=sfLapply(X,func_indicator)
  I=matrix(unlist(I_vector), nrow=nrow(X))
  mis_index=which(I==0)
  
  # Counters and col-mean
  m=foreach(a=1:ncol(X), .combine='cbind') %dopar% {
    vector1=X[,a]
    vector2=I[,a]
    IX=which(vector2!=0)
    valid_counter=sum(vector2[IX])
    result=crossprod(vector1,vector2)/valid_counter
  }
  C=length(which(I==1))
  
  # Co-variance matrix
  Y=foreach(a=1:ncol(X), .combine='cbind') %dopar% {
    vector1=X[,a]
    vector2=I[,a]
    col_mean=rep(m[a],nrow(X))
    result=(vector1-col_mean)*vector2
  }
  R=cov(t(Y))
  
  # Eigenvalues and eigenvector matrix
  eigen_list=eigen(R)
  eigenvalues=as.matrix(eigen_list$values)
  U=eigen_list$vectors
  a=length(eigenvalues)+1
  ratio=1
  sum_eigenvalues=sum(eigenvalues)
  while(ratio>=ratio_limit){
    a=a-1
    if(a==1) {break}
    if(sum(eigenvalues[1:a])==0){
      X[nrow(X),ncol(X)]=mean(X[1:(length(X)-1)])
      return(X)
    }
    ratio=sum(eigenvalues[1:a])/sum_eigenvalues
  } # take a+1
  eigenvalues=eigenvalues[1:(a+1)]
  U=U[,1:(a+1)]
  rm(a,ratio,eigen_list)
  
  # Interpolation
  X_recon=foreach(a=1:ncol(X), .combine='cbind') %dopar% {
    x=X[,a]
    L=diag(I[,a])
    W=diag(rep(1,nrow(X)))
    temp=t(U)%*%L%*%W%*%L%*%U
    if(qr(temp)$rank==base::dim(temp)[1]){
      temp_inv=solve(temp)
    }else{
      temp_inv=MASS::ginv(temp)
    }
    v=temp_inv%*%t(U)%*%L%*%t(W)%*%(x-L%*%rep(m[a],ncol(L)))
    r=U%*%v+rep(m[a],nrow(U))
    gc()
    return(r)
  }
  
  X_interp=X
  X_interp[mis_index]=X_recon[mis_index]
  return(X_interp)
}
