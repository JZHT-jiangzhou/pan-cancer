#' @title for class num as class_result 
#' @export
#' @param class int,class_num 
#' @param cancer the cancer  need to be cout,likec('COAD','READ')
#' @param groups class_result
#' @param info2 info of cancer
#' @return data.frame result of part of cancer
##input
#int class
#names vector groups(with names)
#c cancer the cancer  need to be cout
#data.frame info cancer 
##output 
#data.frame result to be saved
class_num<-function(class,cancer,groups,info2)
{
  result = list()
  for(j in 1:length(class))
  {
    t = groups[groups==class[j]]
    t1 = names(t)
    c1 = c()
    for(i in 1:length(cancer))
    {
      c1[i]=sum(info2[t1,]$project==cancer[i])
    }
    result[[j]] = c1
  }
  result =as.data.frame(result)
  row.names(result) = cancer
  colnames(result) = as.character(class)
  return(result)
}