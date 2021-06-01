#' @title for pheatmap data pretreat
#' @export
#' @param data raw_data
#' @param info info of cancer
#' @param type data  type
#' @param top get top most variable gene
#' @param all bool ,choose all genes
#' @return 2length list,one is data after pretreat,2 is info of cancer in data
data_pretreat<-function(data,info,type,top,all=FALSE)
{
  data = na.omit(data)
  if((type=='rna')|(type=='mirna'))
  {
    data = log10(data+1)
  }
  c = colnames(data)
  t = trans_name(c)
  colnames(data) = t
  t = intersect(t,info$sample_id)
  info2 = info[t,]
  data = data[,t]
  #data = as.matrix(data)
  if(all ==TRUE)
  {
    d1 = data
  }
  else
  {
    d1 = data
    d1$var = apply(data,1,var)
    d1 = d1[order(d1$var,decreasing=T),]
    d1 = d1[1:top,]
    t1 = length(d1[1,])
    d1 = d1[,-t1]
  }
  d1 = as.matrix(d1)
  d2 = t(d1)
  d2 = scale(d2,center=TRUE,scale=TRUE)
  d2=t(d2)
  colnames(d2)=colnames(d1)
  row.names(d1)=row.names(d1)
  return(list(d1,info2))
}