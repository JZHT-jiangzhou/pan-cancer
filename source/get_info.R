#' @export
#' @title get the info of specific cancer 找出特定肿瘤类型的infomation，input：info列表，肿瘤类型 return ：特定肿瘤瘤类型的infomation
#' @param info info
#' @param cancer cancer list
#' @return return the info of cancer
get_info<-function(info,cancer)
{
  for(i in 1:length(cancer))
  {
    info1 = info[info$project==cancer[i],]
    if(i==1)
    {
      info_all=info1
    }
    else
    {
      info_all = rbind(info_all,info1)
    }
  }
  return(info_all)
}
