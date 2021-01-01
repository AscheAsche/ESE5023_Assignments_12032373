# This scripts is runned on Taiyi.
# If you want to run on personal laptop, please change
# the line "cores=40" by your own cores of PC 
# (Intel logical CPUs take no effect ).
# 
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
cores=40
missing_value=-99999
ratio_limit=0.99
rotate_index=0

# Start parallel pools and transmit self-defined functions
sfInit(parallel = TRUE, cpus = cores) 
registerDoParallel(makeCluster(cores))
sfSource('additional_functions.R')
sfExport('missing_value')
sfExport('ratio_limit')

#----------------------------------------------
# Read data
X=read.csv('pic_20x20miss_square.csv',header = F)
X=as.matrix(X)
X[is.na(X)]=-99999

I_vector=sfLapply(X,func_indicator)
I=matrix(unlist(I_vector), nrow=nrow(X))
mis_index=which(I==0)
rm(I_vector)

#----------------------------------------------
# judge if indexes matrix I is full, which means
# data matrix X is well-interpolated
X_rotate=X
I_rotate=I

while(func_full(I_rotate)==F){
  
  # Store index
  x_miss=which(I_rotate==0)[1]
  
  # Get interpolation region
  ix_row=x_miss%%ncol(X_rotate)
  ix_col=floor(x_miss/ncol(X_rotate))+1
  ix_row2=ix_row-6
  ix_col2=ix_col-6
  X_small=X_rotate[ix_row2:ix_row,ix_col2:ix_col]
  
  # Interpolation applied on small region
  X_small=func_interp(X_small)
  interp_element=X_small[nrow(X_small),ncol(X_small)]
  X_rotate[x_miss]=interp_element
  I_rotate[x_miss]=1
  
  # Rotate matrix
  X_rotate=func_mat_rotate(X_rotate)
  I_rotate=func_mat_rotate(I_rotate)
  rotate_index=rotate_index+1

}


#---------------------------------------------------------
# Write result as csv
write.csv(X_rotate,'X_interp.csv',row.names=F)

#---------------------------------------------------------
# Shut down parallel clusters
stopImplicitCluster()
sfStop()

