#' @title get the sample name for class
#' @export
#' @description get the class and type's sample_id, and get info for sur
#' @param class int class,which class
#' @param type string cancer type
#' @param info cancer info
#' @param sur_num int use for suv(sur groups)
#' @return 2length list, 1 is data.frame of sample_id,2 is for sur
##input
#int class: which class
#c type: cancer type
#c groups: classification results
#data.frame info:
#int sur_sum: the number use for sur 
##output
#2legth list
#1sample_id,data.frame,use for diff analysis,2 use for survival analysis
class_name<-function(class,type,groups,info,sur_num=0)
{
  t1=names(groups[groups==class])
  info_c1 = info[t1,]
  lihc_c1 = info_c1[info_c1$project==type,]
  if(length(row.names(lihc_c1))>=1)
  {
    lihc_c1$group=sur_num
    lihc_c1_n = as.character(lihc_c1$sample_id)
    lihc_c1_n = data.frame(sample_id = lihc_c1_n)
  }
  else
  {
    lihc_c1_n = lihc_c1
  }
  return(list(lihc_c1_n,lihc_c1))
}
