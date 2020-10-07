# Problem 5.1
Find_expression=function(n){
  
  processes=permutations(3,8,c('+','','-'),TRUE,TRUE)
  result_process=''
  
  for(i in 1:nrow(processes)){
    process=processes[i,]
    eval_string=paste0('1',process[1],
                       '2',process[2],
                       '3',process[3],
                       '4',process[4],
                       '5',process[5],
                       '6',process[6],
                       '7',process[7],
                       '8',process[8],
                       '9')
    eval(parse(text=paste0('result=',eval_string)))
    if(result==n){
      result_process=c(result_process,eval_string)
      }
  }
  result_process=result_process[2:length(result_process)] # delete 1st element
  if(is.na(result_process[1])){
    result_process=NaN
    print(paste0(n,' has no solution.'))
  }else{
    for(i in 1:length(result_process)){
      print(paste0(result_process[i],'=',n))
    }
  }
  return(result_process)
}

# Problem 5.2
Total_solutions=vector('numeric',100)

for(i in 1:100){
  methods=Find_expression(i)
  Total_solutions[i]=length(methods)
}
max_num=which(Total_solutions==max(Total_solutions))
min_num=which(Total_solutions==min(Total_solutions))
plot(c(1:100),Total_solutions)
print('Integer have maximum solutions is(are):')
print(max_num)
print('Integer have minimum solutions is(are):')
print(min_num)

