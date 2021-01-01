# Call: "additional_functions.R"
# Packages required: parallel, snowfall, foreach, doParallel
#---------------------------------------------------------
library(parallel)
library(foreach)
library(doParallel)
library(snowfall)
library(MASS)

#---------------------------------------------------------
# Initialize parameters
missing_value=-99999
ratio_limit=0.99
cores=detectCores(logical=F)-1

# Start parallel clusters and prepare parallel computing parameters
sfInit(parallel = TRUE, cpus = cores) 
registerDoParallel(makeCluster(cores))
sfSource('additional_functions.R')
sfExport('missing_value')

#----------------------------------------------
# Read data
X=read.csv('sst.csv',header=F)
X[is.na(X)]=-99999
X=as.matrix(X)

# Indicator matrix I
I_vector=sfLapply(X,func_indicator)
I=matrix(unlist(I_vector), nrow=nrow(X))
mis_index=which(I==0)
rm(I_vector)


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
  ratio=sum(eigenvalues[1:a])/sum_eigenvalues
} # take a+1
eigenvalues=eigenvalues[1:a+1]
U=U[,1:a+1]
rm(a,ratio,eigen_list)

# Interpolation
X_recon=foreach(a=1:ncol(X), .combine='cbind') %dopar% {
  x=X[,a]
  L=diag(I[,a])
  #W=R^-1
  #W=t(R^-1)
  W=diag(rep(1,nrow(X)))
  matrix_A=solve(t(U)%*%L%*%W%*%L%*%U)
  v=matrix_A%*%t(U)%*%L%*%t(W)%*%(x-L%*%rep(m[a],ncol(L)))
  r=U%*%v+rep(m[a],nrow(U))
  gc()
  return(r)
}

#----------------------------------------------
# Write results as csv
X_interp=X
X_interp[mis_index]=X_recon[mis_index]
write.csv(X_interp,'sst_interpolated.csv',row.names=F)
write.csv(X_recon,'sst_reconstruct.csv',row.names=F)

#----------------------------------------------
# Shut down parallel clusters
stopImplicitCluster()
sfStop()


