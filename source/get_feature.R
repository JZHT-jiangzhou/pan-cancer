#' @export
#' @title save sigfeature of icluster
#' @param best.fit the result of icluster
#' @param data_list data list col gene, row sample, output of data_pretreat_icluster
#' @param savename sigfeature info savename. default:sigfeature_info.csv
#' @return info of sigfeature
get_feature<-function(best.fit,data_list,savename='sigfeature_info.csv',prob=0.75)
{
  feature = alist()
  for(i in 1:length(data_list))
  {
    feature[[i]] = colnames(data_list[[i]])
  }
  sig = alist()
  for(i in 1:length(data_list)){
    rowsum=apply(abs(best.fit$beta[[i]]),1, sum)
    upper=quantile(rowsum,prob=prob)
    sig[[i]]=(feature[[i]])[which(rowsum>upper)]
  }
  names(sig) = names(data_list)
  r = data.frame()
  for(i in 1:length(sig))
  {
    #t = unlist(head(sig[[i]]))
    t = unlist(sig[[i]])
    #print(head(sig[[i]]))
    for(j in 1:length(t))
    {
      r[j,names(sig)[i]]=t[j]
    }
    
  }
  write.csv(r,savename)
  return(sig)
}

