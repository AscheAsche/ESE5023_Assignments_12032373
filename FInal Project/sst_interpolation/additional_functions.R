# This R scripts contains all needed defined functions
# which will be called by the main scripts.
# 
# Including: 
#   1. Function "func_indicator"
#       Return the validity of observed data with valid as 1, vise as 0.
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
