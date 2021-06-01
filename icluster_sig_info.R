#' @title get siguare of icluster info
#' @param data data col=sample_id row=gene
#' @param sample list sample_list with diff class' samples,with names
#' @param gene c genes for statistics
#' @param m function m like:mean,median,sum
#' @return data.frame result col=sample_name row=gene
##input
#data.frame data col=sample_id row=gene
#list sample  sample_list with diff class' samples,with names
#c gene genes for statistics
#function m like:mean,median,sum
##output
#data.frame result col=sample_name row=gene
icluster_sig_info<-function(data,sample,gene,m)
{
  result = list()
  for(i in 1:length(gene))
  { 
    t = c()
    for(j in 1:length(sample))
    {
      t[j] = m(as.matrix(data[gene[i],sample[[j]]]))
    } 
    result[[i]] = t
  }
  result = as.data.frame(result)
  colnames(result) = gene
  row.names(result) = names(sample)
  result = t(result)
  return(result)
}
