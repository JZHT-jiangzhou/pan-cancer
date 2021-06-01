#' @description input data list, col is sample_id, row is gene. it will output the format of icluster need
#' @title get the format of icluster data
#' @param data data list
#' @param info info of all cancer
#' @param top choose most variable of top gene for rna and met
#' @param snv_ratio choose mutation rate
#' @return data list for icluster
#' @export
##col sample_id.row gene

get_icluster_data<-function(data,info,top_rna=1000,top_met=1000,snv_ratio=0.05)
{
  n = names(data)
  for(i in 1:length(n))
  {
    if(i==1)
    {
      t = colnames(data[[i]])
    }
    else
    {
      t = intersect(t,colnames(data[[i]]))
    }  
  }
  t = intersect(t,info$sample_id)
  sort_index<-function(d1,axis=1)
  {
    if(axis==0)
    {
      d1['index'] = row.names(d1)
      d1 = d1[order(d1$index),]
      d1 = subset(d1,select=-index)
    }
    else
    {
      d1 = t(d1)
      d1 = as.data.frame(d1)
      d1$index = row.names(d1)
      d1 = d1[order(d1$index),]
      d1 = subset(d1,select=-index)
      d1 = t(d1)
    }
    return(d1)
  }
  data_pre<-function(data,type,sample,top)
  {
    data = na.omit(data)
    data = data[,sample]
    if((type=='rna')|(type=='met'))#对RNA，met数据处理，降序
    {
      data['var']=apply(as.matrix(data),1,var)
      data = data[order(data$var,decreasing = TRUE),]
      if(top!='all')
      {
        data = data[1:top,]
      }
      data = subset(data,select=-var)
    }
    else if(type=='snv')#选掉突变太少的基因
    {
      mu = apply(data,1,mean)
      data = data[which(mu>snv_ratio),]
    }
    data = sort_index(data)
    return(data)
  }
  d = alist()
  for(j in 1:length(n))
  {
    print(n[j])
    if(n[j]=='rna')
    {
      tt =  as.matrix(data_pre(data[[j]],n[j],t, top_rna))
    }
    else
    {
      tt = as.matrix(data_pre(data[[j]],n[j],t, top_met))
    }
    z = row.names(tt)
    tt = apply(tt,2,as.numeric)
    row.names(tt) = z
    d[[n[j]]]= tt
  }
  return(d)
}
