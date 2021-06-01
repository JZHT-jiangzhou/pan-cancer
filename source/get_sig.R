#' @export
#' @title get gene feature
#' @param sig list gene
#' @param rawdata rawdata
#' @param sample_list output of partition
#' @param method like mean
get_sig<-function(sig,rawdata,sample_list,method)
{
  count = 1
  for(i in 1:length(sig))
  {
    gene = unlist(sig[[i]])
    #gene = gene[gene!=NaN]
    gene = na.omit(gene)
    if(length(gene)==0)
    {
      next
    }
    
    if(names(rawdata)[i]=='snv')
    {
      sig_info = icluster_sig_info(rawdata[[i]],sample_list,gene,sum)
    }
    else
    {
      sig_info = icluster_sig_info(rawdata[[i]],sample_list,gene,method)
    }
    if(count==1)
    {
      sig_info_all = sig_info
      count = count +1
    }
    else
    {
      sig_info_all = rbind(sig_info_all,sig_info)
    }
  }
  return(sig_info_all)
}
