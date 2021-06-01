#' @export
#' @title partiton of cancer sample according to groups
#' @param r output of icluster_groups
#' @param cut_ratio cut_raio for sur
#' @param groups groups
#' @param info info of cancer
partition<-function(r,cut_ratio,groups,info)
{
  n1 = list()
  n2 = list() #sample_list
  names_sig = c()
  type_sur = c()
  count_sur = 1
  count_sig = 1
  for(i in 1:length(row.names(r)))
  {
    c1 = c()
    count = 1
    for(j in 1:length(colnames(r)))
    {
      if(r[i,j]>=(cut_ratio*sum(r[i,])))
      {
        c1[count] = colnames(r)[j]
        count = count+1
        n2[[count_sig]]=as.character(class_name(j,row.names(r)[i],groups,info,1)[[1]]$sample_id)
        names_sig[count_sig] = paste(row.names(r)[i],j,sep='')
        count_sig = count_sig+1
      }
    }
    if(length(c1)>1)
    {
      type_sur[count_sur] = row.names(r)[i]
      n1[[count_sur]] = c1
      count_sur = count_sur+1
    }
  }
  for(j in 1:length(colnames(r)))
  {
    sample2 = names(groups[groups==j])
    n2[[count_sig]] = sample2
    names_sig[count_sig] = paste('class',j,sep='')
    count_sig = count_sig+1
  }
  names(n2) = names_sig
  return(list(n1,n2,type_sur))
}
