Print_values=function(a,b,c){
  
  if(a==b | b==c | a==c){
    warning('Equal values received')}
  result=vector('numeric',3)
  if(a>b)
    {
    if(b>c){
      result=c(a,b,c)
      }else{
        if(a>c){
          result=c(a,c,b)
        }else{
          result=c(c,a,b)
        }
      }
    }else{
      if(b>c){
        if(a>c){
          result=c(b,a,c)}else{result=c(b,c,a)}
        }else{
      result=c(c,b,a)
      }
    }
  print(result)
}
